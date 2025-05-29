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
        writeln!(&mut self.output, "section .data")?;
        writeln!(&mut self.output, "format db \"%d\", 10, 0")?;
        writeln!(&mut self.output, "section .text")?;
        writeln!(&mut self.output, "global main")?;
        writeln!(&mut self.output, "main:")?;
        writeln!(&mut self.output, "    extern printf")?;
        Ok(())
    }

    pub fn emit_epilogue(&mut self) -> Result<(), Box<dyn Error>> {
        writeln!(&mut self.output, "    mov rdi, format")?;
        writeln!(&mut self.output, "    mov rsi, rax")?;
        writeln!(&mut self.output, "    xor rax, rax")?;
        writeln!(&mut self.output, "    call printf")?;
        writeln!(&mut self.output, "    mov rax, 0")?;
        writeln!(&mut self.output, "    ret")?;
        writeln!(&mut self.output, "section .note.GNU-stack")?;
        writeln!(&mut self.output, "    db 0")?;
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
                    _ => todo!(),
                }
            }
            Expr::Grouping(expr) => self.compile_expr(*expr)?,
            Expr::Literal(token) => match token.token_type {
                TokenType::Number => writeln!(&mut self.output, "    mov rax, {}", token.lexeme)?,
                _ => todo!(),
            },
            Expr::Unary { op, right } => todo!(),
        }
        Ok(())
    }
}
