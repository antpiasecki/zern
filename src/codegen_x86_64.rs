use std::{error::Error, fmt::Write};

use crate::{
    codegen::{Codegen, Env},
    parser::{Expr, Stmt},
    tokenizer::{TokenType, ZernError, error},
};

pub struct CodegenX86_64 {
    output: String,
    label_counter: usize,
}

impl CodegenX86_64 {
    pub fn new_boxed() -> Box<dyn Codegen> {
        Box::new(CodegenX86_64 {
            output: String::new(),
            label_counter: 0,
        })
    }

    fn label(&mut self) -> String {
        self.label_counter += 1;
        format!(".L{}", self.label_counter)
    }
}

impl Codegen for CodegenX86_64 {
    fn get_output(&self) -> String {
        self.output.clone()
    }

    fn emit_prologue(&mut self) -> Result<(), Box<dyn Error>> {
        writeln!(
            &mut self.output,
            "section .data
format db \"%ld\", 10, 0

section .text
extern printf

print:
    push rbp
    mov rbp, rsp
    mov rdi, format
    mov rsi, rax
    xor rax, rax
    call printf
    pop rbp
    ret
"
        )?;
        Ok(())
    }

    fn emit_epilogue(&mut self) -> Result<(), Box<dyn Error>> {
        writeln!(&mut self.output, "section .note.GNU-stack")?;
        writeln!(&mut self.output, "    db 0")?;
        Ok(())
    }

    fn compile_stmt(&mut self, env: &mut Env, stmt: Stmt) -> Result<(), Box<dyn Error>> {
        match stmt {
            Stmt::Expression(expr) => self.compile_expr(env, expr)?,
            Stmt::Var {
                name,
                var_type,
                initializer,
            } => {
                // TODO: types
                assert!(var_type.lexeme == "I64");

                self.compile_expr(env, initializer)?;
                let offset = env.define_var(name.lexeme.clone(), var_type.lexeme);
                writeln!(&mut self.output, "    mov QWORD [rbp-{}], rax", offset)?;
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
                writeln!(&mut self.output, "    test rax, rax")?;
                writeln!(&mut self.output, "    je {}", else_label)?;
                self.compile_stmt(env, *then_branch.clone())?;
                writeln!(&mut self.output, "    jmp {}", end_label)?;
                writeln!(&mut self.output, "{}:", else_label)?;
                self.compile_stmt(env, *else_branch.clone())?;
                writeln!(&mut self.output, "{}:", end_label)?;
            }
            Stmt::While { condition, body } => {
                let begin_label = self.label();
                let end_label = self.label();

                writeln!(&mut self.output, "{}:", begin_label)?;
                self.compile_expr(env, condition)?;
                writeln!(&mut self.output, "    test rax, rax")?;
                writeln!(&mut self.output, "    je {}", end_label)?;
                self.compile_stmt(env, *body.clone())?;
                writeln!(&mut self.output, "    jmp {}", begin_label)?;
                writeln!(&mut self.output, "{}:", end_label)?;
            }
            Stmt::Function {
                name,
                params,
                return_type,
                body,
            } => {
                assert!(params.is_empty()); // TODO
                assert!(return_type.lexeme == "I64");

                writeln!(&mut self.output, "global {}", name.lexeme)?;
                writeln!(&mut self.output, "{}:", name.lexeme)?;
                writeln!(&mut self.output, "    push rbp")?;
                writeln!(&mut self.output, "    mov rbp, rsp")?;
                writeln!(&mut self.output, "    sub rsp, 256")?; // TODO

                self.compile_stmt(env, *body)?;

                writeln!(&mut self.output, "    mov rax, 0")?;
                writeln!(&mut self.output, "    mov rsp, rbp")?;
                writeln!(&mut self.output, "    pop rbp")?;
                writeln!(&mut self.output, "    ret")?;
            }
            Stmt::Return(expr) => {
                self.compile_expr(env, expr)?;
                writeln!(&mut self.output, "    mov rsp, rbp")?;
                writeln!(&mut self.output, "    pop rbp")?;
                writeln!(&mut self.output, "    ret")?;
            }
        }
        Ok(())
    }

    fn compile_expr(&mut self, env: &mut Env, expr: Expr) -> Result<(), Box<dyn Error>> {
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
                    TokenType::DoubleEqual => {
                        writeln!(&mut self.output, "    cmp rax, rbx")?;
                        writeln!(&mut self.output, "    sete al")?;
                        writeln!(&mut self.output, "    movzx rax, al")?;
                    }
                    TokenType::NotEqual => {
                        writeln!(&mut self.output, "    cmp rax, rbx")?;
                        writeln!(&mut self.output, "    setne al")?;
                        writeln!(&mut self.output, "    movzx rax, al")?;
                    }
                    TokenType::Greater => {
                        writeln!(&mut self.output, "    cmp rax, rbx")?;
                        writeln!(&mut self.output, "    setg al")?;
                        writeln!(&mut self.output, "    movzx rax, al")?;
                    }
                    TokenType::GreaterEqual => {
                        writeln!(&mut self.output, "    cmp rax, rbx")?;
                        writeln!(&mut self.output, "    setge al")?;
                        writeln!(&mut self.output, "    movzx rax, al")?;
                    }
                    TokenType::Less => {
                        writeln!(&mut self.output, "    cmp rax, rbx")?;
                        writeln!(&mut self.output, "    setl al")?;
                        writeln!(&mut self.output, "    movzx rax, al")?;
                    }
                    TokenType::LessEqual => {
                        writeln!(&mut self.output, "    cmp rax, rbx")?;
                        writeln!(&mut self.output, "    setle al")?;
                        writeln!(&mut self.output, "    movzx rax, al")?;
                    }
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
                    TokenType::Bang => {
                        writeln!(&mut self.output, "    test rax, rax")?;
                        writeln!(&mut self.output, "    sete al")?;
                        writeln!(&mut self.output, "    movzx rax, al")?;
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
                writeln!(
                    &mut self.output,
                    "    mov rax, QWORD [rbp-{}]",
                    var.stack_offset
                )?
            }
            Expr::Assign { name, value } => {
                self.compile_expr(env, *value)?;

                let var = match env.get_var(&name.lexeme) {
                    Some(x) => x,
                    None => {
                        return error!(name.loc, format!("undefined variable: {}", &name.lexeme));
                    }
                };
                writeln!(
                    &mut self.output,
                    "    mov QWORD [rbp-{}], rax",
                    var.stack_offset
                )?;
            }
            Expr::Call {
                callee,
                paren: _,
                args,
            } => {
                let callee = match *callee {
                    Expr::Variable(name) => name.lexeme,
                    _ => todo!(),
                };

                // TODO
                assert!(args.len() <= 1);
                if args.len() == 1 {
                    self.compile_expr(env, args.first().unwrap().clone())?;
                    writeln!(&mut self.output, "    mov rdi, rax")?;
                }

                writeln!(&mut self.output, "    call {}", callee)?;
            }
        }
        Ok(())
    }
}
