use std::{collections::HashMap, error::Error, fmt::Write};

use crate::{
    parser::{Expr, Stmt},
    tokenizer::{MotError, TokenType, error},
};

pub struct Var {
    var_type: String,
    stack_offset: usize,
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

pub struct Codegen {
    output: String,
    label_counter: usize,
}

impl Codegen {
    pub fn new() -> Codegen {
        Codegen {
            output: String::new(),
            label_counter: 0,
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
            "    mov rsp, rbp
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

    pub fn label(&mut self) -> String {
        self.label_counter += 1;
        format!(".{}", self.label_counter)
    }

    pub fn compile_stmt(&mut self, env: &mut Env, stmt: Stmt) -> Result<(), Box<dyn Error>> {
        match stmt {
            Stmt::Expression(expr) => self.compile_expr(env, expr)?,
            Stmt::Print(expr) => {
                self.compile_expr(env, expr)?;
                writeln!(
                    &mut self.output,
                    "    mov rdi, format
    mov rsi, rax
    xor rax, rax
    call printf"
                )?;
            }
            Stmt::Var {
                name,
                var_type,
                initializer,
            } => {
                // TODO
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
                    TokenType::Less => {
                        writeln!(&mut self.output, "    cmp rax, rbx")?;
                        writeln!(&mut self.output, "    setl al")?;
                        writeln!(&mut self.output, "    movzx rax, al")?;
                    }
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
        }
        Ok(())
    }
}
