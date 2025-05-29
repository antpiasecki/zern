use std::{collections::HashMap, error::Error, fmt::Write};

use crate::{
    parser::{Expr, Stmt},
    tokenizer::{MotError, TokenType, error},
};

pub struct Env {
    locals: HashMap<String, usize>,
}

impl Env {
    pub fn new() -> Env {
        Env {
            locals: HashMap::new(),
        }
    }
}

pub struct Codegen {
    output: String,
}

impl Codegen {
    pub fn new() -> Codegen {
        Codegen {
            output: String::new(),
        }
    }

    pub fn emit_prologue(&mut self) -> Result<(), Box<dyn Error>> {
        writeln!(
            &mut self.output,
            "section .data
format db \"%d\", 10, 0
section .text
global main
main:
    extern printf
    push rbp
    mov rbp, rsp
    sub rsp, 256" // TODO
        )?;
        Ok(())
    }

    pub fn emit_epilogue(&mut self) -> Result<(), Box<dyn Error>> {
        write!(
            &mut self.output,
            "   mov rsp, rbp
    pop rbp
    mov rax, 0
    ret

section .note.GNU-stack
    db 0
"
        )?;
        Ok(())
    }

    pub fn get_output(self) -> String {
        self.output
    }

    pub fn compile_stmt(&mut self, env: &mut Env, stmt: Stmt) -> Result<(), Box<dyn Error>> {
        match stmt {
            Stmt::Expression(expr) => self.compile_expr(env, expr)?,
            Stmt::Print(expr) => {
                self.compile_expr(env, expr)?;
                writeln!(
                    &mut self.output,
                    "   
    mov rdi, format
    mov rsi, rax
    xor rax, rax
    call printf"
                )?;
            }
            Stmt::Var { name, initializer } => {
                self.compile_expr(env, initializer)?;
                let offset = (env.locals.len() + 1) * 8;
                env.locals.insert(name.lexeme, offset);
                writeln!(&mut self.output, "    mov QWORD [rbp-{}], rax", offset)?;
            }
            Stmt::Block(statements) => {
                let mut env = Env::new();
                for stmt in statements {
                    self.compile_stmt(&mut env, stmt)?;
                }
            }
            Stmt::If {
                condition: _,
                then_branch: _,
                else_branch: _,
            } => todo!(),
            Stmt::While {
                condition: _,
                body: _,
            } => todo!(),
        }
        Ok(())
    }

    pub fn compile_expr(&mut self, env: &mut Env, expr: Expr) -> Result<(), Box<dyn Error>> {
        match expr {
            Expr::Binary { left, op, right } => {
                self.compile_expr(env, *left)?;
                writeln!(&mut self.output, "    push rax")?;
                self.compile_expr(env, *right)?;
                writeln!(&mut self.output, "    mov rbx, rax")?;
                writeln!(&mut self.output, "    pop rax")?;

                match op.token_type {
                    TokenType::Plus => writeln!(&mut self.output, "    add rax, rbx")?,
                    TokenType::Minus => writeln!(&mut self.output, "    sub rax, rbx")?,
                    TokenType::Star => writeln!(&mut self.output, "    imul rax, rbx")?,
                    TokenType::Slash => {
                        writeln!(&mut self.output, "    cqo")?;
                        writeln!(&mut self.output, "    idiv rbx")?;
                    }
                    TokenType::Mod => {
                        writeln!(&mut self.output, "    cqo")?;
                        writeln!(&mut self.output, "    idiv rbx")?;
                        writeln!(&mut self.output, "    mov rax, rdx")?;
                    }
                    TokenType::Xor => writeln!(&mut self.output, "    xor rax, rbx")?,
                    TokenType::And => todo!(),
                    TokenType::Or => todo!(),
                    TokenType::DoubleEqual => todo!(),
                    TokenType::NotEqual => todo!(),
                    TokenType::Greater => todo!(),
                    TokenType::GreaterEqual => todo!(),
                    TokenType::Less => todo!(),
                    TokenType::LessEqual => todo!(),
                    _ => unreachable!(),
                }
            }
            Expr::Grouping(expr) => self.compile_expr(env, *expr)?,
            Expr::Literal(token) => match token.token_type {
                TokenType::Number => writeln!(&mut self.output, "    mov rax, {}", token.lexeme)?,
                TokenType::String => todo!(),
                _ => unreachable!(),
            },
            Expr::Unary { op, right } => {
                self.compile_expr(env, *right)?;
                match op.token_type {
                    TokenType::Minus => writeln!(&mut self.output, "    neg rax")?,
                    TokenType::Bang => todo!(),
                    _ => unreachable!(),
                }
            }
            Expr::Variable(name) => {
                let offset = match env.locals.get(&name.lexeme) {
                    Some(x) => x,
                    None => {
                        return error!(name.loc, format!("undefined variable: {}", &name.lexeme));
                    }
                };
                writeln!(&mut self.output, "    mov rax, QWORD [rbp-{}]", offset)?
            }
            Expr::Assign { name, value } => {
                self.compile_expr(env, *value)?;
                let offset = match env.locals.get(&name.lexeme) {
                    Some(x) => x,
                    None => {
                        return error!(name.loc, format!("undefined variable: {}", &name.lexeme));
                    }
                };
                writeln!(&mut self.output, "    mov QWORD [rbp-{}], rax", offset)?;
            }
        }
        Ok(())
    }
}
