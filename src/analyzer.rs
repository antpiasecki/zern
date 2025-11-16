use std::collections::HashMap;

use crate::{
    parser::{Expr, Stmt},
    tokenizer::{ZernError, error},
};

pub struct Analyzer {
    pub functions: HashMap<String, usize>,
}

impl Analyzer {
    pub fn new() -> Analyzer {
        Analyzer {
            functions: HashMap::new(),
        }
    }

    pub fn register_function(&mut self, stmt: &Stmt) -> Result<(), ZernError> {
        if let Stmt::Function {
            name,
            params,
            return_type: _,
            body: _,
        } = stmt
        {
            if self.functions.contains_key(&name.lexeme) {
                return error!(name.loc, format!("tried to redefine '{}'", name.lexeme));
            }
            self.functions.insert(name.lexeme.clone(), params.len());
        }
        Ok(())
    }

    pub fn analyze_stmt(&mut self, stmt: &Stmt) -> Result<(), ZernError> {
        match stmt {
            Stmt::Expression(expr) => self.analyze_expr(expr)?,
            Stmt::Let {
                name: _,
                var_type: _,
                initializer,
            } => {
                self.analyze_expr(initializer)?;
            }
            Stmt::Block(statements) => {
                for stmt in statements {
                    self.analyze_stmt(stmt)?;
                }
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.analyze_expr(condition)?;
                self.analyze_stmt(then_branch)?;
                self.analyze_stmt(else_branch)?;
            }
            Stmt::While { condition, body } => {
                self.analyze_expr(condition)?;
                self.analyze_stmt(body)?;
            }
            Stmt::Function {
                name,
                params: _,
                return_type,
                body,
            } => {
                if name.lexeme == "main" && return_type.lexeme != "I64" {
                    return error!(&name.loc, "main must return I64");
                }

                self.analyze_stmt(body)?;
            }
            Stmt::Return(expr) => {
                self.analyze_expr(expr)?;
            }
            Stmt::For {
                var: _,
                start,
                end,
                body,
            } => {
                self.analyze_expr(start)?;
                self.analyze_expr(end)?;
                self.analyze_stmt(body)?;
            }
            Stmt::Break => {}
            Stmt::Continue => {}
            Stmt::Extern(_) => {}
        }
        Ok(())
    }

    pub fn analyze_expr(&mut self, expr: &Expr) -> Result<(), ZernError> {
        match expr {
            Expr::Binary { left, op: _, right } => {
                self.analyze_expr(left)?;
                self.analyze_expr(right)?;
            }
            Expr::Grouping(expr) => self.analyze_expr(expr)?,
            Expr::Literal(_) => {}
            Expr::Unary { op: _, right } => {
                self.analyze_expr(right)?;
            }
            Expr::Variable(_) => {}
            Expr::Assign { name: _, value } => {
                self.analyze_expr(value)?;
            }
            Expr::Call {
                callee,
                paren,
                args,
            } => {
                let callee = match callee.as_ref() {
                    Expr::Variable(name) => name.lexeme.clone(),
                    _ => return error!(&paren.loc, "tried to call a non-constant expression"),
                };

                if let Some(arity) = self.functions.get(&callee) {
                    if *arity != args.len() {
                        return error!(
                            &paren.loc,
                            format!("expected {} arguments, got {}", arity, args.len())
                        );
                    }
                } else {
                    // TODO: cant error here since we dont analyze externs/builtins
                }

                for arg in args {
                    self.analyze_expr(arg)?;
                }
            }
            Expr::ArrayLiteral(exprs) => {
                for expr in exprs {
                    self.analyze_expr(expr)?;
                }
            }
            Expr::Index { expr, index } => {
                self.analyze_expr(expr)?;
                self.analyze_expr(index)?;
            }
        }
        Ok(())
    }
}
