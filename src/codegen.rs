use std::{error::Error, fmt::Write};

use crate::{parser::Expr, tokenizer::TokenType};

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
    extern printf"
        )?;
        Ok(())
    }

    pub fn emit_epilogue(&mut self) -> Result<(), Box<dyn Error>> {
        write!(
            &mut self.output,
            "
    mov rdi, format
    mov rsi, rax
    xor rax, rax
    call printf

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

    pub fn compile_expr(&mut self, expr: Expr) -> Result<(), Box<dyn Error>> {
        match expr {
            Expr::Binary { left, op, right } => {
                self.compile_expr(*left)?;
                writeln!(&mut self.output, "    push rax")?;
                self.compile_expr(*right)?;
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
            Expr::Grouping(expr) => self.compile_expr(*expr)?,
            Expr::Literal(token) => match token.token_type {
                TokenType::Number => writeln!(&mut self.output, "    mov rax, {}", token.lexeme)?,
                TokenType::String => todo!(),
                _ => unreachable!(),
            },
            Expr::Unary { op, right } => {
                self.compile_expr(*right)?;
                match op.token_type {
                    TokenType::Minus => writeln!(&mut self.output, "    neg rax")?,
                    TokenType::Bang => todo!(),
                    _ => unreachable!(),
                }
            }
        }
        Ok(())
    }
}
