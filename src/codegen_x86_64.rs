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
static TYPES: [&str; 2] = ["I64", "String"];

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
extern free
extern printf
extern sprintf
extern strlen
extern strcmp
extern puts
extern system
print equ puts

; generated with clang
strrev:
    push    r14
    push    rbx
    push    rax
    mov     rbx, rdi
    call    strlen
    mov     r14, rax
    lea     rdi, [rax + 1]
    call    malloc
    mov     rcx, rax
    mov     rsi, r14
    mov     rdx, r14
.LBB0_1:
    sub     rdx, 1
    jb      .LBB0_2
    mov     sil, byte [rbx + rsi - 1]
    mov     byte [rcx], sil
    inc     rcx
    mov     rsi, rdx
    jmp     .LBB0_1
.LBB0_2:
    mov     byte [rax + r14], 0
    add     rsp, 8
    pop     rbx
    pop     r14
    ret

isqrt:
    xor     rax, rax
    mov     rcx, 1
    mov     rbx, rdi
    shl     rcx, 62
.LBB0_3:
    cmp     rcx, 0
    je      .LBB0_5
    cmp     rcx, rbx
    jbe     .LBB0_4
    shr     rcx, 2
    jmp     .LBB0_3
.LBB0_4:
    cmp     rcx, 0
    je      .LBB0_5
    mov     rdx, rax
    add     rdx, rcx
    cmp     rbx, rdx
    jb      .LBB0_7
    sub     rbx, rdx
    shr     rax, 1
    add     rax, rcx
    jmp     .LBB0_6
.LBB0_7:
    shr     rax, 1
.LBB0_6:
    shr     rcx, 2
    jmp     .LBB0_4
.LBB0_5:
    ret

nth:
    movzx rax, byte [rdi + rsi]
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

                emit!(&mut self.output, "global {}", name.lexeme);
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

                emit!(&mut self.output, "    mov rax, 0"); // TODO: remove default return value
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
                    let value = &token.lexeme[1..token.lexeme.len() - 1].replace("\\n", "\n");
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
                    emit!(&mut self.output, "    mov rax, S{}", self.data_counter);
                    self.data_counter += 1;
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
