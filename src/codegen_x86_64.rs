use std::{collections::HashMap, fmt::Write};

use crate::{
    parser::{Expr, Stmt},
    tokenizer::{TokenType, ZernError, error},
};

pub struct Var {
    pub var_type: String,
    pub stack_offset: usize,
}

pub struct Env {
    scopes: Vec<HashMap<String, Var>>,
    next_offset: usize,
}

impl Env {
    pub fn new() -> Env {
        Env {
            scopes: vec![HashMap::new()],
            next_offset: 8,
        }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn define_var(&mut self, name: String, var_type: String) -> usize {
        let offset = self.next_offset;
        self.next_offset += 8;
        self.scopes.last_mut().unwrap().insert(name, Var {
            var_type,
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

static REGISTERS: [&str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
static TYPES: [&str; 6] = ["I64", "String", "Bool", "Ptr", "Char", "Array"];

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
            data_counter: 0,
        }
    }

    fn label(&mut self) -> String {
        self.label_counter += 1;
        format!(".L{}", self.label_counter)
    }

    pub fn get_output(&self) -> String {
        format!(
            "section .data
    SASSERT db \"assertion failed on line %d\",10,0
{}{}",
            self.data_section, self.output
        )
    }

    pub fn emit_prologue(&mut self) -> Result<(), ZernError> {
        emit!(
            &mut self.output,
            "
section .text
extern malloc
extern calloc
extern realloc
extern free
extern printf
extern sprintf
extern strlen
extern strcmp
extern strcat
extern strcpy
extern strdup
extern strlcpy
extern puts
extern fopen
extern fseek
extern ftell
extern fread
extern rewind
extern system
extern exit
extern gettimeofday
copystr equ strdup

String.nth:
    movzx rax, byte [rdi + rsi]
    ret

String.set:
    mov [rdi + rsi], dl
    ret

time:
    push rbx
    sub rsp, 16
    mov rbx, rsp
    mov rdi, rbx
    xor esi, esi
    call gettimeofday
    imul rcx, qword [rbx], 1000
    mov rax, qword [rbx+8]
    mov esi, 1000
    cqo
    idiv rsi
    add rax, rcx
    add rsp, 16
    pop rbx
    ret

Array.new:
    mov rdi, 1
    mov rsi, 24
    jmp calloc

Array.nth:
    mov rax, [rdi]
    mov rax, [rax + rsi*8]
    ret

Array.push:
    push r14
    push rbx
    push rax
    mov r14, rsi
    mov rbx, rdi
    mov rax, [rdi]
    mov rcx, [rdi + 16]
    cmp rcx, [rdi + 8]
    jne .no_realloc
    lea rdx, [rcx + rcx]
    mov rsi, 4
    test rcx, rcx
    cmovnz rsi, rdx
    mov [rbx + 8], rsi
    shl rsi, 3
    mov rdi, rax
    call realloc
    mov [rbx], rax
    mov rcx, [rbx + 16]
.no_realloc:
    mov [rax + rcx*8], r14
    inc qword [rbx + 16]
    add rsp, 8
    pop rbx
    pop r14
    ret

Array.size:
    mov rax, [rdi + 16]
    ret

Array.free:
    push rbx
    mov rbx, rdi
    mov rdi, [rdi]
    call free
    mov rdi, rbx
    pop rbx
    jmp free

Math.isqrt:
    xor rax, rax
    mov rcx, 1
    mov rbx, rdi
    shl rcx, 62
.isqrt.1:
    cmp rcx, 0
    je .isqrt.5
    cmp rcx, rbx
    jbe .isqrt.2
    shr rcx, 2
    jmp .isqrt.1
.isqrt.2:
    cmp rcx, 0
    je .isqrt.5
    mov rdx, rax
    add rdx, rcx
    cmp rbx, rdx
    jb .isqrt.3
    sub rbx, rdx
    shr rax, 1
    add rax, rcx
    jmp .isqrt.4
.isqrt.3:
    shr rax, 1
.isqrt.4:
    shr rcx, 2
    jmp .isqrt.2
.isqrt.5:
    ret
",
        );
        Ok(())
    }

    pub fn emit_epilogue(&mut self) -> Result<(), ZernError> {
        emit!(&mut self.output, "section .note.GNU-stack");
        emit!(&mut self.output, "    db 0");
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
                // TODO: types
                if !TYPES.contains(&var_type.lexeme.as_str()) {
                    return error!(&name.loc, format!("unknown type: {}", var_type.lexeme));
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
                let begin_label = self.label();
                let end_label = self.label();

                emit!(&mut self.output, "{}:", begin_label);
                self.compile_expr(env, condition)?;
                emit!(&mut self.output, "    test rax, rax");
                emit!(&mut self.output, "    je {}", end_label);
                self.compile_stmt(env, *body.clone())?;
                emit!(&mut self.output, "    jmp {}", begin_label);
                emit!(&mut self.output, "{}:", end_label);
            }
            Stmt::Function {
                name,
                params,
                return_type,
                body,
            } => {
                if !TYPES.contains(&return_type.lexeme.as_str()) {
                    return error!(&name.loc, format!("unknown type: {}", return_type.lexeme));
                }

                // TODO
                if name.lexeme == "main" {
                    emit!(&mut self.output, "global {}", name.lexeme);
                }
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
                    emit!(&mut self.output, "    mov QWORD [rbp-{}], {}", offset, reg,);
                }

                self.compile_stmt(env, *body)?;

                // TODO: default exit code only for main
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
            Stmt::Assert { keyword, value } => {
                self.compile_expr(env, value)?;
                let skip_label = self.label();
                emit!(&mut self.output, "    test rax, rax");
                emit!(&mut self.output, "    jne {}", skip_label);
                emit!(&mut self.output, "    mov rdi, SASSERT");
                emit!(&mut self.output, "    mov rsi, {}", keyword.loc.line);
                emit!(&mut self.output, "    call printf");
                emit!(&mut self.output, "    mov rdi, 1");
                emit!(&mut self.output, "    call exit");
                emit!(&mut self.output, "{}:", skip_label);
            }
            Stmt::For {
                var,
                start,
                end,
                body,
            } => {
                let begin_label = self.label();
                let end_label = self.label();

                let offset = env.define_var(var.lexeme, "I64".into());

                self.compile_expr(env, start)?;
                emit!(&mut self.output, "    mov QWORD [rbp-{}], rax", offset);
                emit!(&mut self.output, "{}:", begin_label);
                emit!(&mut self.output, "    mov rax, QWORD [rbp-{}]", offset);
                emit!(&mut self.output, "    push rax");
                self.compile_expr(env, end)?;
                emit!(&mut self.output, "    pop rcx");
                emit!(&mut self.output, "    cmp rcx, rax");
                emit!(&mut self.output, "    jge {}", end_label);
                self.compile_stmt(env, *body)?;
                emit!(&mut self.output, "    mov rax, QWORD [rbp-{}]", offset);
                emit!(&mut self.output, "    add rax, 1");
                emit!(&mut self.output, "    mov QWORD [rbp-{}], rax", offset);
                emit!(&mut self.output, "    jmp {}", begin_label);
                emit!(&mut self.output, "{}:", end_label);
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
                    _ => unreachable!(),
                }
            }
            Expr::Grouping(expr) => self.compile_expr(env, *expr)?,
            Expr::Literal(token) => match token.token_type {
                TokenType::Number => {
                    emit!(&mut self.output, "    mov rax, {}", token.lexeme);
                }
                TokenType::String => {
                    // TODO: actual string parsing in the tokenizer
                    let value = &token.lexeme[1..token.lexeme.len() - 1]
                        .replace("\\n", "\n")
                        .replace("\\r", "\r")
                        .replace("\\t", "\t")
                        .replace("\\033", "\x1b")
                        .replace("\\0", "\0");

                    if value.is_empty() {
                        emit!(&mut self.data_section, "    S{} db 0", self.data_counter);
                    } else {
                        let charcodes = value
                            .chars()
                            .map(|x| (x as u8).to_string())
                            .collect::<Vec<String>>()
                            .join(",");
                        emit!(
                            &mut self.data_section,
                            "    S{} db {},0",
                            self.data_counter,
                            charcodes,
                        );
                    }
                    emit!(&mut self.output, "    mov rax, S{}", self.data_counter);
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
                // TODO: in function calls like a(1, b(2, 3)) the first argument will get overwritten when calling b
                let callee = match *callee {
                    Expr::Variable(name) => name.lexeme,
                    _ => return error!(&paren.loc, "tried to call a non-constant expression"),
                };

                for (i, arg) in args.iter().enumerate() {
                    self.compile_expr(env, arg.clone())?;
                    let reg = match REGISTERS.get(i) {
                        Some(x) => x,
                        None => return error!(&paren.loc, "only up to 6 args allowed"),
                    };
                    emit!(&mut self.output, "    mov {}, rax", reg,);
                }

                emit!(&mut self.output, "    call {}", callee);
            }
        }
        Ok(())
    }
}
