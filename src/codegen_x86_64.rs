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
            next_offset: 8,
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
        self.next_offset += 8;
        self.scopes.last_mut().unwrap().insert(
            name,
            Var {
                stack_offset: offset,
            },
        );
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

static REGISTERS: [&str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

pub struct CodegenX86_64 {
    output: String,
    data_section: String,
    label_counter: usize,
    data_counter: usize,
}

impl CodegenX86_64 {
    pub fn new() -> CodegenX86_64 {
        CodegenX86_64 {
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
            "section .data
{}{}",
            self.data_section, self.output
        )
    }

    pub fn emit_prologue(&mut self) -> Result<(), ZernError> {
        emit!(
            &mut self.output,
            "section .note.GNU-stack
    db 0

section .text
"
        );

        // take that rustfmt
        for name in "malloc,calloc,realloc,free,puts,putchar,printf,sprintf,snprintf,strtol,strlen,strcmp,strncmp,strcat,strcpy,strdup,strncpy,syscall,fopen,fseek,ftell,fread,fwrite,fclose,rewind,system,opendir,readdir,closedir,exit,gettimeofday,connect,socket,send,write,read,close,bind,listen,accept,getchar,gethostbyname,dlopen,dlsym,dlerror".split(",")
        {
            emit!(&mut self.output, "extern {}", name);
            emit!(&mut self.output, "c.{} equ {}", name, name);
        }

        emit!(
            &mut self.output,
            "
section .text._builtin_read8
_builtin_read8:
    xor rax, rax 
    mov al, byte [rdi]
    ret

section .text._builtin_read64
_builtin_read64:
    mov rax, qword [rdi]
    ret

section .text._builtin_set8
_builtin_set8:
    mov [rdi], sil
    ret

section .text._builtin_set64
_builtin_set64:
    mov [rdi], rsi
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
                emit!(&mut self.output, "    mov QWORD [rbp-{}], rax", offset);
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
                emit!(&mut self.output, "    test rax, rax");
                emit!(&mut self.output, "    je {}", else_label);
                self.compile_stmt(env, *then_branch.clone())?;
                emit!(&mut self.output, "    jmp {}", end_label);
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
                emit!(&mut self.output, "    test rax, rax");
                emit!(&mut self.output, "    je {}", env.loop_end_label);
                self.compile_stmt(env, *body.clone())?;
                emit!(&mut self.output, "    jmp {}", env.loop_begin_label);
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
                    emit!(&mut self.output, "global {}", name.lexeme);
                }
                emit!(&mut self.output, "section .text.{}", name.lexeme);
                emit!(&mut self.output, "{}:", name.lexeme);
                emit!(&mut self.output, "    push rbp");
                emit!(&mut self.output, "    mov rbp, rsp");
                emit!(&mut self.output, "    sub rsp, 256"); // TODO

                for (i, param) in params.iter().enumerate() {
                    let offset = env
                        .define_var(param.var_name.lexeme.clone(), param.var_type.lexeme.clone());
                    let reg = match REGISTERS.get(i) {
                        Some(x) => x,
                        None => return error!(&name.loc, "only up to 6 params allowed"),
                    };
                    emit!(&mut self.output, "    mov QWORD [rbp-{}], {}", offset, reg);
                }

                self.compile_stmt(env, *body)?;

                // fallback to returning null
                emit!(&mut self.output, "    mov rax, 0");
                emit!(&mut self.output, "    mov rsp, rbp");
                emit!(&mut self.output, "    pop rbp");
                emit!(&mut self.output, "    ret");
            }
            Stmt::Return(expr) => {
                self.compile_expr(env, expr)?;
                emit!(&mut self.output, "    mov rsp, rbp");
                emit!(&mut self.output, "    pop rbp");
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
                emit!(&mut self.output, "    mov QWORD [rbp-{}], rax", offset);
                emit!(&mut self.output, "{}:", env.loop_begin_label);
                emit!(&mut self.output, "    mov rax, QWORD [rbp-{}]", offset);
                emit!(&mut self.output, "    push rax");
                self.compile_expr(env, end)?;
                emit!(&mut self.output, "    pop rcx");
                emit!(&mut self.output, "    cmp rcx, rax");
                emit!(&mut self.output, "    jge {}", env.loop_end_label);
                self.compile_stmt(env, *body)?;
                emit!(&mut self.output, "    mov rax, QWORD [rbp-{}]", offset);
                emit!(&mut self.output, "    add rax, 1");
                emit!(&mut self.output, "    mov QWORD [rbp-{}], rax", offset);
                emit!(&mut self.output, "    jmp {}", env.loop_begin_label);
                emit!(&mut self.output, "{}:", env.loop_end_label);
                env.pop_scope();

                env.loop_begin_label = old_loop_begin_label;
                env.loop_end_label = old_loop_end_label;
            }
            Stmt::Break => {
                emit!(&mut self.output, "    jmp {}", env.loop_end_label);
            }
            Stmt::Continue => {
                // TODO: skips incrementing when used in a for loop
                emit!(&mut self.output, "    jmp {}", env.loop_begin_label);
            }
        }
        Ok(())
    }

    pub fn compile_expr(&mut self, env: &mut Env, expr: Expr) -> Result<(), ZernError> {
        match expr {
            Expr::Binary { left, op, right } => {
                self.compile_expr(env, *left)?;
                emit!(&mut self.output, "    push rax");
                self.compile_expr(env, *right)?;
                emit!(&mut self.output, "    mov rbx, rax");
                emit!(&mut self.output, "    pop rax");

                match op.token_type {
                    TokenType::Plus => {
                        emit!(&mut self.output, "    add rax, rbx");
                    }
                    TokenType::Minus => {
                        emit!(&mut self.output, "    sub rax, rbx");
                    }
                    TokenType::Star => {
                        emit!(&mut self.output, "    imul rax, rbx");
                    }
                    TokenType::Slash => {
                        emit!(&mut self.output, "    cqo");
                        emit!(&mut self.output, "    idiv rbx");
                    }
                    TokenType::Mod => {
                        emit!(&mut self.output, "    cqo");
                        emit!(&mut self.output, "    idiv rbx");
                        emit!(&mut self.output, "    mov rax, rdx");
                    }
                    TokenType::Xor => {
                        emit!(&mut self.output, "    xor rax, rbx");
                    }
                    TokenType::And => {
                        emit!(&mut self.output, "    and rax, rbx");
                    }
                    TokenType::Or => {
                        emit!(&mut self.output, "    or rax, rbx");
                    }
                    TokenType::DoubleEqual => {
                        emit!(&mut self.output, "    cmp rax, rbx");
                        emit!(&mut self.output, "    sete al");
                        emit!(&mut self.output, "    movzx rax, al");
                    }
                    TokenType::NotEqual => {
                        emit!(&mut self.output, "    cmp rax, rbx");
                        emit!(&mut self.output, "    setne al");
                        emit!(&mut self.output, "    movzx rax, al");
                    }
                    TokenType::Greater => {
                        emit!(&mut self.output, "    cmp rax, rbx");
                        emit!(&mut self.output, "    setg al");
                        emit!(&mut self.output, "    movzx rax, al");
                    }
                    TokenType::GreaterEqual => {
                        emit!(&mut self.output, "    cmp rax, rbx");
                        emit!(&mut self.output, "    setge al");
                        emit!(&mut self.output, "    movzx rax, al");
                    }
                    TokenType::Less => {
                        emit!(&mut self.output, "    cmp rax, rbx");
                        emit!(&mut self.output, "    setl al");
                        emit!(&mut self.output, "    movzx rax, al");
                    }
                    TokenType::LessEqual => {
                        emit!(&mut self.output, "    cmp rax, rbx");
                        emit!(&mut self.output, "    setle al");
                        emit!(&mut self.output, "    movzx rax, al");
                    }
                    TokenType::ShiftLeft => {
                        emit!(&mut self.output, "    mov rcx, rbx");
                        emit!(&mut self.output, "    sal rax, cl");
                    }
                    TokenType::ShiftRight => {
                        emit!(&mut self.output, "    mov rcx, rbx");
                        emit!(&mut self.output, "    sar rax, cl");
                    }
                    _ => unreachable!(),
                }
            }
            Expr::Grouping(expr) => self.compile_expr(env, *expr)?,
            Expr::Literal(token) => match token.token_type {
                TokenType::Number => {
                    emit!(&mut self.output, "    mov rax, {}", token.lexeme);
                }
                TokenType::Char => {
                    emit!(
                        &mut self.output,
                        "    mov rax, {}",
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
                        emit!(&mut self.data_section, "    {} db 0", label);
                    } else {
                        let charcodes = value
                            .chars()
                            .map(|x| (x as u8).to_string())
                            .collect::<Vec<String>>()
                            .join(",");
                        emit!(&mut self.data_section, "    {} db {},0", label, charcodes,);
                    }
                    emit!(&mut self.output, "    mov rax, {}", label);
                    self.data_counter += 1;
                }
                TokenType::True => {
                    emit!(&mut self.output, "    mov rax, 1");
                }
                TokenType::False => {
                    emit!(&mut self.output, "    mov rax, 0");
                }
                _ => unreachable!(),
            },
            Expr::Unary { op, right } => {
                self.compile_expr(env, *right)?;
                match op.token_type {
                    TokenType::Minus => {
                        emit!(&mut self.output, "    neg rax");
                    }
                    TokenType::Bang => {
                        emit!(&mut self.output, "    test rax, rax");
                        emit!(&mut self.output, "    sete al");
                        emit!(&mut self.output, "    movzx rax, al");
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
                emit!(
                    &mut self.output,
                    "    mov rax, QWORD [rbp-{}]",
                    var.stack_offset,
                );
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
                emit!(
                    &mut self.output,
                    "    mov QWORD [rbp-{}], rax",
                    var.stack_offset,
                );
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
                    emit!(&mut self.output, "    push rax");
                }

                for i in (0..args.len()).rev() {
                    let reg = match REGISTERS.get(i) {
                        Some(x) => x,
                        None => return error!(&paren.loc, "only up to 6 args allowed"),
                    };
                    emit!(&mut self.output, "    pop {}", reg);
                }

                match env.get_var(&callee) {
                    Some(var) => {
                        emit!(
                            &mut self.output,
                            "    mov rax, QWORD [rbp-{}]",
                            var.stack_offset,
                        );
                        emit!(&mut self.output, "    call rax");
                    }
                    None => {
                        emit!(&mut self.output, "    call {}", callee);
                    }
                };
            }
            Expr::ArrayLiteral(exprs) => {
                emit!(&mut self.output, "    call array.new");
                emit!(&mut self.output, "    push rax");

                for expr in exprs {
                    self.compile_expr(env, expr)?;
                    emit!(&mut self.output, "    mov rsi, rax");
                    emit!(&mut self.output, "    pop rdi");
                    emit!(&mut self.output, "    push rdi");
                    emit!(&mut self.output, "    call array.push");
                }
                emit!(&mut self.output, "    pop rax");
            }
            Expr::Index { expr, index } => {
                self.compile_expr(env, *expr)?;
                emit!(&mut self.output, "    push rax");
                self.compile_expr(env, *index)?;
                emit!(&mut self.output, "    pop rbx");
                emit!(&mut self.output, "    mov rbx, [rbx]");
                emit!(&mut self.output, "    mov rax, [rbx + rax*8]");
            }
        }
        Ok(())
    }
}
