use std::collections::HashMap;

use crate::{
    parser::{Expr, Stmt},
    tokenizer::{ZernError, error},
};

pub type Type = String;

pub struct StructField {
    pub offset: usize,
    pub field_type: Type,
}

#[derive(Clone)]
pub struct FnType {
    pub return_type: Type,
    pub params: Option<Vec<Type>>,
}

impl FnType {
    fn new(return_type: &str, params: Vec<&str>) -> FnType {
        FnType {
            return_type: return_type.to_string(),
            params: Some(params.iter().map(|x| x.to_string()).collect()),
        }
    }

    fn new_variadic(return_type: &str) -> FnType {
        FnType {
            return_type: return_type.to_string(),
            params: None,
        }
    }
}

pub struct Analyzer {
    pub functions: HashMap<String, FnType>,
    pub constants: HashMap<String, u64>,
    pub structs: HashMap<String, HashMap<String, StructField>>,
}

impl Analyzer {
    pub fn new() -> Analyzer {
        Analyzer {
            functions: HashMap::from([
                ("_builtin_heap_head".into(), FnType::new("ptr", vec![])),
                ("_builtin_heap_tail".into(), FnType::new("ptr", vec![])),
                ("_builtin_err_code".into(), FnType::new("ptr", vec![])),
                ("_builtin_err_msg".into(), FnType::new("ptr", vec![])),
                ("_builtin_read64".into(), FnType::new("i64", vec!["ptr"])),
                (
                    "_builtin_set64".into(),
                    FnType::new("void", vec!["ptr", "i64"]),
                ),
                ("_builtin_syscall".into(), FnType::new_variadic("i64")),
                ("io.printf".into(), FnType::new_variadic("void")),
                ("_builtin_environ".into(), FnType::new("ptr", vec![])),
            ]),
            constants: HashMap::new(),
            structs: HashMap::new(),
        }
    }

    pub fn register_function(&mut self, stmt: &Stmt) -> Result<(), ZernError> {
        if let Stmt::Function {
            name,
            params,
            return_type,
            body: _,
            exported: _,
        } = stmt
        {
            if self.functions.contains_key(&name.lexeme) {
                return error!(name.loc, format!("tried to redefine '{}'", name.lexeme));
            }
            self.functions.insert(
                name.lexeme.clone(),
                FnType {
                    return_type: return_type.lexeme.clone(),
                    params: Some(params.iter().map(|x| x.var_type.lexeme.clone()).collect()),
                },
            );
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
            Stmt::Const { name, value } => {
                if self.constants.contains_key(&name.lexeme)
                    || self.functions.contains_key(&name.lexeme)
                {
                    return error!(
                        name.loc,
                        format!("tried to redefine constant '{}'", name.lexeme)
                    );
                }
                if value.lexeme.starts_with("0x") {
                    self.constants.insert(
                        name.lexeme.clone(),
                        u64::from_str_radix(&value.lexeme[2..], 16).unwrap(),
                    );
                } else {
                    self.constants
                        .insert(name.lexeme.clone(), value.lexeme.parse().unwrap());
                }
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
                if name.lexeme == "main" && return_type.lexeme != "i64" {
                    return error!(&name.loc, "main must return i64");
                }

                self.analyze_stmt(body)?;
            }
            Stmt::Return { expr, keyword: _ } => {
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
                self.functions
                    .insert(name.lexeme.clone(), FnType::new_variadic("any"));
            }
            Stmt::Struct { name, fields } => {
                let mut fields_map: HashMap<String, StructField> = HashMap::new();

                let mut offset: usize = 0;
                for field in fields {
                    fields_map.insert(
                        field.var_name.lexeme.clone(),
                        StructField {
                            offset,
                            field_type: field.var_type.lexeme.clone(),
                        },
                    );
                    offset += 8;
                }

                self.structs.insert(name.lexeme.clone(), fields_map);
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
            Expr::Logical { left, op: _, right } => {
                self.analyze_expr(left)?;
                self.analyze_expr(right)?;
            }
            Expr::Grouping(expr) => self.analyze_expr(expr)?,
            Expr::Literal(_) => {}
            Expr::Unary { op: _, right } => {
                self.analyze_expr(right)?;
            }
            Expr::Variable(_) => {}
            Expr::Assign { left, op: _, value } => {
                self.analyze_expr(left)?;
                self.analyze_expr(value)?;
            }
            Expr::Call {
                callee,
                paren,
                args,
            } => {
                if let Expr::Variable(callee_name) = *callee.clone() {
                    if self.functions.contains_key(&callee_name.lexeme) {
                        // its a function (defined/builtin/extern)
                        if let Some(fn_type) = self.functions.get(&callee_name.lexeme) {
                            // if its None, its variadic
                            if let Some(params) = &fn_type.params
                                && params.len() != args.len()
                            {
                                return error!(
                                    &paren.loc,
                                    format!(
                                        "expected {} arguments, got {}",
                                        params.len(),
                                        args.len()
                                    )
                                );
                            }
                        } else {
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
            Expr::Index {
                expr,
                bracket: _,
                index,
            } => {
                self.analyze_expr(expr)?;
                self.analyze_expr(index)?;
            }
            Expr::AddrOf { op: _, expr } => {
                self.analyze_expr(expr)?;
            }
            Expr::New(_) => {}
            Expr::MemberAccess { left, field: _ } => {
                self.analyze_expr(left)?;
            }
            Expr::Cast { expr, type_name: _ } => {
                self.analyze_expr(expr)?;
            }
        }
        Ok(())
    }
}
