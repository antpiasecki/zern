use std::{collections::HashMap, fmt::Write};

use crate::{
    parser::{Expr, Stmt},
    tokenizer::{TokenType, ZernError, error},
};

pub struct Var {
    // pub var_type: String,
    pub stack_offset: usize,
}

pub struct Env {
    scopes: Vec<HashMap<String, Var>>,
    next_offset: usize,
    loop_begin_label: String,
    loop_end_label: String,
}

impl Env {
    pub fn new() -> Env {
        Env {
            scopes: vec![HashMap::new()],
            next_offset: 24,
            loop_begin_label: String::new(),
            loop_end_label: String::new(),
        }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn define_var(&mut self, name: String, _var_type: String) -> usize {
        let offset = self.next_offset;
        self.next_offset += 16;
        self.scopes.last_mut().unwrap().insert(name, Var {
            stack_offset: offset,
        });
        offset
    }

    pub fn get_var(&self, name: &str) -> Option<&Var> {
        for scope in self.scopes.iter().rev() {
            if let Some(var) = scope.get(name) {
                return Some(var);
            }
        }
        None
    }
}

macro_rules! emit {
    ($($arg:tt)*) => {
        let _ = writeln!($($arg)*);
    };
}

static REGISTERS: [&str; 8] = ["a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7"];

pub struct CodegenRISCV {
    output: String,
    data_section: String,
    label_counter: usize,
    data_counter: usize,
}

impl CodegenRISCV {
    pub fn new() -> CodegenRISCV {
        CodegenRISCV {
            output: String::new(),
            data_section: String::new(),
            label_counter: 0,
            data_counter: 1,
        }
    }

    fn label(&mut self) -> String {
        self.label_counter += 1;
        format!(".L{}", self.label_counter)
    }

    pub fn get_output(&self) -> String {
        format!(
            ".section .data
{}{}",
            self.data_section, self.output
        )
    }

    pub fn emit_prologue(&mut self) -> Result<(), ZernError> {
        emit!(
            &mut self.output,
            ".section .note.GNU-stack,\"\",@progbits

.section .text"
        );

        // take that rustfmt
        for name in "malloc,calloc,realloc,free,puts,printf,sprintf,snprintf,strtol,strlen,strcmp,strcat,strcpy,strdup,strncpy,syscall,fopen,fseek,ftell,fread,fwrite,fclose,rewind,system,opendir,readdir,closedir,exit,gettimeofday,connect,socket,send,write,read,close,bind,listen,accept,getchar,gethostbyname".split(",")
        {
            emit!(&mut self.output, ".extern {}", name);
            emit!(&mut self.output, ".equ c.{}, {}", name, name);
        }

        emit!(
            &mut self.output,
            "
.section .text._builtin_deref8
_builtin_deref8:
    lbu a0, 0(a0)
    ret

.section .text._builtin_deref64
_builtin_deref64:
    ld a0, 0(a0)
    ret

.section .text._builtin_set8
_builtin_set8:
    sb a1, 0(a0)
    ret

.section .text._builtin_set64
_builtin_set64:
    sd a1, 0(a0)
    ret

.section .text._builtin_lshift
_builtin_lshift:
    sll a0, a0, a1
    ret

.section .text._builtin_rshift
_builtin_rshift:
    sra a0, a0, a1
    ret
"
        );
        Ok(())
    }

    pub fn compile_stmt(&mut self, env: &mut Env, stmt: Stmt) -> Result<(), ZernError> {
        match stmt {
            Stmt::Expression(expr) => self.compile_expr(env, expr)?,
            Stmt::Let {
                name,
                var_type,
                initializer,
            } => {
                // TODO: move to analyzer
                if env.get_var(&name.lexeme).is_some() {
                    return error!(
                        name.loc,
                        format!("variable already defined: {}", &name.lexeme)
                    );
                }

                self.compile_expr(env, initializer)?;
                let offset = env.define_var(name.lexeme.clone(), var_type.lexeme);
                emit!(&mut self.output, "    sd a0, -{}(fp)", offset);
            }
            Stmt::Block(statements) => {
                env.push_scope();
                for stmt in statements {
                    self.compile_stmt(env, stmt)?;
                }
                env.pop_scope();
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let else_label = self.label();
                let end_label = self.label();

                self.compile_expr(env, condition)?;
                emit!(&mut self.output, "    beqz a0, {}", else_label);
                self.compile_stmt(env, *then_branch.clone())?;
                emit!(&mut self.output, "    j {}", end_label);
                emit!(&mut self.output, "{}:", else_label);
                self.compile_stmt(env, *else_branch.clone())?;
                emit!(&mut self.output, "{}:", end_label);
            }
            Stmt::While { condition, body } => {
                let old_loop_begin_label = env.loop_begin_label.clone();
                let old_loop_end_label = env.loop_end_label.clone();
                env.loop_begin_label = self.label();
                env.loop_end_label = self.label();

                emit!(&mut self.output, "{}:", env.loop_begin_label);
                self.compile_expr(env, condition)?;
                emit!(&mut self.output, "    beqz a0, {}", env.loop_end_label);
                self.compile_stmt(env, *body.clone())?;
                emit!(&mut self.output, "    j {}", env.loop_begin_label);
                emit!(&mut self.output, "{}:", env.loop_end_label);

                env.loop_begin_label = old_loop_begin_label;
                env.loop_end_label = old_loop_end_label;
            }
            Stmt::Function {
                name,
                params,
                return_type: _,
                body,
            } => {
                if name.lexeme == "main" {
                    emit!(&mut self.output, ".globl {}", name.lexeme);
                }
                emit!(&mut self.output, ".section .text.{}", name.lexeme);
                emit!(&mut self.output, "{}:", name.lexeme);
                emit!(&mut self.output, "    addi sp, sp, -16");
                emit!(&mut self.output, "    sd ra, 8(sp)");
                emit!(&mut self.output, "    sd fp, 0(sp)");
                emit!(&mut self.output, "    addi fp, sp, 16");
                emit!(&mut self.output, "    addi sp, sp, -256"); // TODO

                for (i, param) in params.iter().enumerate() {
                    let offset = env
                        .define_var(param.var_name.lexeme.clone(), param.var_type.lexeme.clone());
                    let reg = match REGISTERS.get(i) {
                        Some(x) => x,
                        None => return error!(&name.loc, "only up to 8 params allowed"),
                    };
                    emit!(&mut self.output, "    sd {}, -{}(fp)", reg, offset);
                }

                self.compile_stmt(env, *body)?;

                if name.lexeme == "main" {
                    emit!(&mut self.output, "    li a0, 0");
                }

                emit!(&mut self.output, "    addi sp, fp, -16");
                emit!(&mut self.output, "    ld ra, 8(sp)");
                emit!(&mut self.output, "    ld fp, 0(sp)");
                emit!(&mut self.output, "    addi sp, sp, 16");
                emit!(&mut self.output, "    ret");
            }
            Stmt::Return(expr) => {
                self.compile_expr(env, expr)?;
                emit!(&mut self.output, "    addi sp, fp, -16");
                emit!(&mut self.output, "    ld ra, 8(sp)");
                emit!(&mut self.output, "    ld fp, 0(sp)");
                emit!(&mut self.output, "    addi sp, sp, 16");
                emit!(&mut self.output, "    ret");
            }
            Stmt::For {
                var,
                start,
                end,
                body,
            } => {
                let old_loop_begin_label = env.loop_begin_label.clone();
                let old_loop_end_label = env.loop_end_label.clone();
                env.loop_begin_label = self.label();
                env.loop_end_label = self.label();

                env.push_scope();
                let offset = env.define_var(var.lexeme, "I64".into());

                self.compile_expr(env, start)?;
                emit!(&mut self.output, "    sd a0, -{}(fp)", offset);
                emit!(&mut self.output, "{}:", env.loop_begin_label);
                emit!(&mut self.output, "    ld a0, -{}(fp)", offset);
                emit!(&mut self.output, "    addi sp, sp, -16");
                emit!(&mut self.output, "    sd a0, 0(sp)");
                self.compile_expr(env, end)?;
                emit!(&mut self.output, "    ld a1, 0(sp)");
                emit!(&mut self.output, "    addi sp, sp, 16");
                emit!(&mut self.output, "    bge a1, a0, {}", env.loop_end_label);
                self.compile_stmt(env, *body)?;
                emit!(&mut self.output, "    ld a0, -{}(fp)", offset);
                emit!(&mut self.output, "    addi a0, a0, 1");
                emit!(&mut self.output, "    sd a0, -{}(fp)", offset);
                emit!(&mut self.output, "    j {}", env.loop_begin_label);
                emit!(&mut self.output, "{}:", env.loop_end_label);
                env.pop_scope();

                env.loop_begin_label = old_loop_begin_label;
                env.loop_end_label = old_loop_end_label;
            }
            Stmt::Break => {
                emit!(&mut self.output, "    j {}", env.loop_end_label);
            }
            Stmt::Continue => {
                // TODO: skips incrementing when used in a for loop
                emit!(&mut self.output, "    j {}", env.loop_begin_label);
            }
        }
        Ok(())
    }

    pub fn compile_expr(&mut self, env: &mut Env, expr: Expr) -> Result<(), ZernError> {
        match expr {
            Expr::Binary { left, op, right } => {
                self.compile_expr(env, *left)?;
                emit!(&mut self.output, "    addi sp, sp, -16");
                emit!(&mut self.output, "    sd a0, 0(sp)");
                self.compile_expr(env, *right)?;
                emit!(&mut self.output, "    mv a1, a0");
                emit!(&mut self.output, "    ld a0, 0(sp)");
                emit!(&mut self.output, "    addi sp, sp, 16");

                match op.token_type {
                    TokenType::Plus => {
                        emit!(&mut self.output, "    add a0, a0, a1");
                    }
                    TokenType::Minus => {
                        emit!(&mut self.output, "    sub a0, a0, a1");
                    }
                    TokenType::Star => {
                        emit!(&mut self.output, "    mul a0, a0, a1");
                    }
                    TokenType::Slash => {
                        emit!(&mut self.output, "    div a0, a0, a1");
                    }
                    TokenType::Mod => {
                        emit!(&mut self.output, "    rem a0, a0, a1");
                    }
                    TokenType::Xor => {
                        emit!(&mut self.output, "    xor a0, a0, a1");
                    }
                    TokenType::And => {
                        emit!(&mut self.output, "    and a0, a0, a1");
                    }
                    TokenType::Or => {
                        emit!(&mut self.output, "    or a0, a0, a1");
                    }
                    TokenType::DoubleEqual => {
                        emit!(&mut self.output, "    xor a0, a0, a1");
                        emit!(&mut self.output, "    seqz a0, a0");
                    }
                    TokenType::NotEqual => {
                        emit!(&mut self.output, "    xor a0, a0, a1");
                        emit!(&mut self.output, "    snez a0, a0");
                    }
                    TokenType::Greater => {
                        emit!(&mut self.output, "    slt a0, a1, a0");
                    }
                    TokenType::GreaterEqual => {
                        emit!(&mut self.output, "    slt a0, a0, a1");
                        emit!(&mut self.output, "    xori a0, a0, 1");
                    }
                    TokenType::Less => {
                        emit!(&mut self.output, "    slt a0, a0, a1");
                    }
                    TokenType::LessEqual => {
                        emit!(&mut self.output, "    slt a0, a1, a0");
                        emit!(&mut self.output, "    xori a0, a0, 1");
                    }
                    _ => unreachable!(),
                }
            }
            Expr::Grouping(expr) => self.compile_expr(env, *expr)?,
            Expr::Literal(token) => match token.token_type {
                TokenType::Number => {
                    emit!(&mut self.output, "    li a0, {}", token.lexeme);
                }
                TokenType::Char => {
                    emit!(
                        &mut self.output,
                        "    li a0, {}",
                        token.lexeme.chars().nth(1).unwrap() as u8
                    );
                }
                TokenType::String => {
                    // TODO: actual string parsing in the tokenizer
                    let value = &token.lexeme[1..token.lexeme.len() - 1]
                        .replace("\\n", "\n")
                        .replace("\\r", "\r")
                        .replace("\\t", "\t")
                        .replace("\\033", "\x1b")
                        .replace("\\0", "\0");

                    let label = format!("str_{:03}", self.data_counter);

                    if value.is_empty() {
                        emit!(&mut self.data_section, "    {}: .byte 0", label);
                    } else {
                        let charcodes = value
                            .chars()
                            .map(|x| (x as u8).to_string())
                            .collect::<Vec<String>>()
                            .join(",");
                        emit!(
                            &mut self.data_section,
                            "    {}: .byte {},0",
                            label,
                            charcodes,
                        );
                    }
                    emit!(&mut self.output, "    la a0, {}", label);
                    self.data_counter += 1;
                }
                TokenType::True => {
                    emit!(&mut self.output, "    li a0, 1");
                }
                TokenType::False => {
                    emit!(&mut self.output, "    li a0, 0");
                }
                _ => unreachable!(),
            },
            Expr::Unary { op, right } => {
                self.compile_expr(env, *right)?;
                match op.token_type {
                    TokenType::Minus => {
                        emit!(&mut self.output, "    sub a0, x0, a0");
                    }
                    TokenType::Bang => {
                        emit!(&mut self.output, "    seqz a0, a0");
                    }
                    _ => unreachable!(),
                }
            }
            Expr::Variable(name) => {
                // TODO: move to analyzer
                let var = match env.get_var(&name.lexeme) {
                    Some(x) => x,
                    None => {
                        return error!(name.loc, format!("undefined variable: {}", &name.lexeme));
                    }
                };
                emit!(&mut self.output, "    ld a0, -{}(fp)", var.stack_offset);
            }
            Expr::Assign { name, value } => {
                self.compile_expr(env, *value)?;

                // TODO: move to analyzer
                let var = match env.get_var(&name.lexeme) {
                    Some(x) => x,
                    None => {
                        return error!(name.loc, format!("undefined variable: {}", &name.lexeme));
                    }
                };
                emit!(&mut self.output, "    sd a0, -{}(fp)", var.stack_offset);
            }
            Expr::Call {
                callee,
                paren,
                args,
            } => {
                let callee = match *callee {
                    Expr::Variable(name) => name.lexeme,
                    _ => return error!(&paren.loc, "tried to call a non-constant expression"),
                };

                for arg in &args {
                    self.compile_expr(env, arg.clone())?;
                    emit!(&mut self.output, "    addi sp, sp, -16");
                    emit!(&mut self.output, "    sd a0, 0(sp)");
                }

                for i in (0..args.len()).rev() {
                    let reg = match REGISTERS.get(i) {
                        Some(x) => x,
                        None => return error!(&paren.loc, "only up to 8 args allowed"),
                    };
                    emit!(&mut self.output, "    ld {}, 0(sp)", reg);
                    emit!(&mut self.output, "    addi sp, sp, 16");
                }

                emit!(&mut self.output, "    call {}", callee);
            }
            Expr::ArrayLiteral(exprs) => {
                emit!(&mut self.output, "    call array.new");
                emit!(&mut self.output, "    addi sp, sp, -16");
                emit!(&mut self.output, "    sd a0, 0(sp)");

                for expr in exprs {
                    self.compile_expr(env, expr)?;
                    emit!(&mut self.output, "    mv a1, a0");
                    emit!(&mut self.output, "    ld a0, 0(sp)");
                    emit!(&mut self.output, "    sd a0, 0(sp)");
                    emit!(&mut self.output, "    call array.push");
                }
                emit!(&mut self.output, "    ld a0, 0(sp)");
                emit!(&mut self.output, "    addi sp, sp, 16");
            }
            Expr::Index { expr, index } => {
                self.compile_expr(env, *expr)?;
                emit!(&mut self.output, "    addi sp, sp, -16");
                emit!(&mut self.output, "    sd a0, 0(sp)");
                self.compile_expr(env, *index)?;
                emit!(&mut self.output, "    ld t0, 0(sp)");
                emit!(&mut self.output, "    addi sp, sp, 16");
                emit!(&mut self.output, "    ld t0, 0(t0)");
                emit!(&mut self.output, "    li t1, 8");
                emit!(&mut self.output, "    mul t2, a0, t1");
                emit!(&mut self.output, "    add t0, t0, t2");
                emit!(&mut self.output, "    ld a0, 0(t0)");
            }
        }
        Ok(())
    }
}
