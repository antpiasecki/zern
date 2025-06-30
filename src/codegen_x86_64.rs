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
extern stdin
extern malloc
extern calloc
extern realloc
extern free
extern printf
extern sprintf
extern strtol
extern strlen
extern strcmp
extern strcat
extern strcpy
extern strdup
extern strncpy
extern fgets
extern fopen
extern fseek
extern ftell
extern fread
extern fwrite
extern fclose
extern rewind
extern system
extern opendir
extern readdir
extern closedir
extern exit
extern gettimeofday
extern connect
extern inet_addr
extern socket
extern send
extern read
extern close
extern bind
extern listen
extern accept

section .text._builtin_deref
_builtin_deref:
    mov rax, qword [rdi]
    ret

section .text._builtin_stdin
_builtin_stdin:
    mov rax, [rel stdin]
    ret

section .text._builtin_lshift
_builtin_lshift:
    mov rcx, rsi
    mov rax, rdi
    shl rax, cl
    ret

section .text._builtin_rshift
_builtin_rshift:
    mov rcx, rsi
    mov rax, rdi
    sar rax, cl
    ret

section .text._builtin_string_set
_builtin_string_set:
    mov [rdi + rsi], dl
    ret

section .text._builtin_listdir
_builtin_listdir:
    push r14
    push rbx
    push rax
    mov r14, rdi
    call Array.new
    mov rbx, rax
    mov rdi, r14
    call opendir
    mov r14, rax
._builtin_listdir.1:
    mov rdi, r14
    call readdir
    test rax, rax
    je ._builtin_listdir.3
    cmp byte [rax+19], 46
    jne ._builtin_listdir.2
    movzx ecx, byte [rax+20]
    test ecx, ecx
    je ._builtin_listdir.1
    cmp ecx, 46
    jne ._builtin_listdir.2
    cmp byte [rax+21], 0
    je ._builtin_listdir.1
._builtin_listdir.2:
    add rax, 19
    mov rdi, rax
    call strdup
    mov rsi, rax
    mov rdi, rbx
    call Array.push
    jmp ._builtin_listdir.1
._builtin_listdir.3:
    mov rdi, r14
    call closedir
    mov rax, rbx
    add rsp, 8
    pop rbx
    pop r14
    ret

section .text._builtin_array_set
_builtin_array_set:
    mov rax, [rdi]
    mov [rax + rsi*8], rdx
    ret

section .text._builtin_array_push
_builtin_array_push:
    push r14
    push rbx
    push rax
    mov r14, rsi
    mov rbx, rdi
    mov rax, [rdi]
    mov rcx, [rdi + 16]
    cmp rcx, [rdi + 8]
    jne ._builtin_array_push.1
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
._builtin_array_push.1:
    mov [rax + rcx*8], r14
    inc qword [rbx + 16]
    add rsp, 8
    pop rbx
    pop r14
    ret

section .text._builtin_array_size
_builtin_array_size:
    mov rax, [rdi + 16]
    ret

section .text._builtin_array_free
_builtin_array_free:
    push rbx
    mov rbx, rdi
    mov rdi, [rdi]
    call free
    mov rdi, rbx
    pop rbx
    jmp free
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
                if name.lexeme == "main" {
                    emit!(&mut self.output, "global {}", name.lexeme);
                    if return_type.lexeme != "I64" {
                        return error!(&name.loc, "main must return I64");
                    }
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

                if name.lexeme == "main" {
                    emit!(&mut self.output, "    mov rax, 0");
                }

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
                let begin_label = self.label();
                let end_label = self.label();

                env.push_scope();
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
                env.pop_scope();
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

                emit!(&mut self.output, "    call {}", callee);
            }
            Expr::ArrayLiteral(exprs) => {
                emit!(&mut self.output, "    call Array.new");
                emit!(&mut self.output, "    push rax");

                for expr in exprs {
                    self.compile_expr(env, expr)?;
                    emit!(&mut self.output, "    mov rsi, rax");
                    emit!(&mut self.output, "    pop rdi");
                    emit!(&mut self.output, "    push rdi");
                    emit!(&mut self.output, "    call Array.push");
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
