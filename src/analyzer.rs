use std::collections::HashMap;

use crate::{
    parser::{Expr, Stmt},
    tokenizer::{ZernError, error},
};

pub struct Analyzer {
    pub functions: HashMap<String, i32>,
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
            exported: _,
        } = stmt
        {
            if self.functions.contains_key(&name.lexeme) {
                return error!(name.loc, format!("tried to redefine '{}'", name.lexeme));
            }
            self.functions
                .insert(name.lexeme.clone(), params.len() as i32);
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
                exported: _,
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
            Stmt::Extern(name) => {
                if self.functions.contains_key(&name.lexeme) {
                    return error!(name.loc, format!("tried to redefine '{}'", name.lexeme));
                }
                self.functions.insert(name.lexeme.clone(), -1);
            }
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
                if let Expr::Variable(callee_name) = *callee.clone() {
                    if callee_name.lexeme.starts_with("_builtin_")
                        || self.functions.contains_key(&callee_name.lexeme)
                    {
                        // its a function (defined/builtin/extern)
                        if let Some(arity) = self.functions.get(&callee_name.lexeme) {
                            if *arity >= 0 && *arity != args.len() as i32 {
                                return error!(
                                    &paren.loc,
                                    format!("expected {} arguments, got {}", arity, args.len())
                                );
                            }
                        } else if !callee_name.lexeme.starts_with("_builtin_") {
                            return error!(
                                &paren.loc,
                                format!("undefined function: {}", callee_name.lexeme)
                            );
                        }
                    } else {
                        // its a variable containing function address
                        self.analyze_expr(callee)?;
                    }
                } else {
                    // its an expression that evalutes to function address
                    self.analyze_expr(callee)?;
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
            Expr::AddrOf { op: _, expr } => {
                self.analyze_expr(expr)?;
            }
        }
        Ok(())
    }
}
