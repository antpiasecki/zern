#![allow(dead_code)]
#![allow(unused_variables)]

use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    analyzer::Analyzer,
    parser::{Expr, Stmt},
    tokenizer::{TokenType, ZernError, error},
};

type Type = String;

macro_rules! expect_type {
    ($expr_type:expr, $expected:expr, $loc:expr) => {
        if $expr_type != $expected {
            return error!(
                $loc,
                format!("expected type '{}', got '{}'", $expected, $expr_type)
            );
        }
    };
}

macro_rules! expect_types {
    ($expr_type:expr, [$( $expected:expr ),+], $loc:expr) => {
        if $( $expr_type != $expected )&&+ {
            return error!(
                $loc,
                format!(
                    "expected one of [{}], got '{}'",
                    [$( $expected ),+].join(", "),
                    $expr_type
                )
            );
        }
    };
}

// TODO: currently they are all just 64 bit values
static BUILTIN_TYPES: [&str; 8] = ["void", "u8", "i64", "str", "bool", "ptr", "fnptr", "any"];

pub struct Env {
    scopes: Vec<HashMap<String, Type>>,
}

impl Env {
    pub fn new() -> Env {
        Env {
            scopes: vec![HashMap::new()],
        }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn define_var(&mut self, name: String, var_type: String) {
        self.scopes.last_mut().unwrap().insert(name, var_type);
    }

    fn get_var_type(&self, name: &str) -> Option<&Type> {
        for scope in self.scopes.iter().rev() {
            if let Some(var) = scope.get(name) {
                return Some(var);
            }
        }
        None
    }
}

pub struct TypeChecker {
    analyzer: Rc<RefCell<Analyzer>>,
}

impl TypeChecker {
    pub fn new(analyzer: Rc<RefCell<Analyzer>>) -> TypeChecker {
        TypeChecker { analyzer }
    }

    pub fn typecheck_stmt(&mut self, env: &mut Env, stmt: &Stmt) -> Result<(), ZernError> {
        match stmt {
            Stmt::Expression(expr) => {
                self.typecheck_expr(env, expr)?;
            }
            Stmt::Let {
                name,
                var_type,
                initializer,
            } => {
                let inferred = self.typecheck_expr(env, initializer)?;
                if let Some(var_type) = var_type {
                    if !self.is_valid_type_name(&var_type.lexeme) {
                        return error!(
                            &name.loc,
                            "unrecognized type: ".to_owned() + &var_type.lexeme
                        );
                    }
                    expect_type!(inferred, var_type.lexeme, var_type.loc);
                }

                env.define_var(name.lexeme.clone(), inferred);
            }
            Stmt::Const { name, value } => {}
            Stmt::Block(stmts) => {
                for stmt in stmts {
                    self.typecheck_stmt(env, stmt)?;
                }
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.typecheck_expr(env, condition)?;
                self.typecheck_stmt(env, then_branch)?;
                self.typecheck_stmt(env, else_branch)?;
            }
            Stmt::While { condition, body } => todo!(),
            Stmt::For {
                var,
                start,
                end,
                body,
            } => todo!(),
            Stmt::Function {
                name,
                params,
                return_type,
                body,
                exported,
            } => {
                env.push_scope();

                if !self.is_valid_type_name(&return_type.lexeme) {
                    return error!(
                        &return_type.loc,
                        "unrecognized type: ".to_owned() + &return_type.lexeme
                    );
                }

                for (i, param) in params.iter().enumerate() {
                    if !self.is_valid_type_name(&param.var_type.lexeme) {
                        return error!(
                            &param.var_name.loc,
                            "unrecognized type: ".to_owned() + &param.var_type.lexeme
                        );
                    }

                    env.define_var(param.var_name.lexeme.clone(), param.var_type.lexeme.clone());
                }

                self.typecheck_stmt(env, body)?;

                env.pop_scope();
            }
            Stmt::Return(expr) => {
                // TODO
            }
            Stmt::Break => todo!(),
            Stmt::Continue => todo!(),
            Stmt::Extern(token) => todo!(),
            Stmt::Struct { name, fields } => {}
        }
        Ok(())
    }

    pub fn typecheck_expr(&mut self, env: &mut Env, expr: &Expr) -> Result<Type, ZernError> {
        match expr {
            Expr::Binary { left, op, right } => {
                expect_types!(self.typecheck_expr(env, left)?, ["i64", "ptr"], op.loc);
                expect_types!(self.typecheck_expr(env, right)?, ["i64", "ptr"], op.loc);

                match op.token_type {
                    TokenType::Plus
                    | TokenType::Minus
                    | TokenType::Star
                    | TokenType::Slash
                    | TokenType::Mod
                    | TokenType::Xor
                    | TokenType::BitAnd
                    | TokenType::BitOr
                    | TokenType::ShiftLeft
                    | TokenType::ShiftRight => Ok("i64".into()),
                    TokenType::DoubleEqual
                    | TokenType::NotEqual
                    | TokenType::Greater
                    | TokenType::GreaterEqual
                    | TokenType::Less
                    | TokenType::LessEqual => Ok("bool".into()),
                    _ => unreachable!(),
                }
            }
            Expr::Logical { left, op, right } => todo!(),
            Expr::Grouping(expr) => self.typecheck_expr(env, expr),
            Expr::Literal(token) => match token.token_type {
                TokenType::Number => Ok("i64".into()),
                TokenType::Char => Ok("u8".into()),
                TokenType::String => Ok("str".into()),
                TokenType::True => Ok("bool".into()),
                TokenType::False => Ok("bool".into()),
                _ => unreachable!(),
            },
            Expr::Unary { op, right } => {
                let right_type = self.typecheck_expr(env, right)?;
                match op.token_type {
                    TokenType::Minus => {
                        expect_type!(right_type, "i64", op.loc);
                        Ok("i64".into())
                    }
                    TokenType::Bang => {
                        expect_type!(right_type, "bool", op.loc);
                        Ok("bool".into())
                    }
                    _ => unreachable!(),
                }
            }
            Expr::Variable(name) => {
                if self.analyzer.borrow().constants.contains_key(&name.lexeme) {
                    Ok("i64".into())
                } else {
                    match env.get_var_type(&name.lexeme) {
                        Some(x) => Ok(x.clone()),
                        None => error!(name.loc, format!("undefined variable: {}", &name.lexeme)),
                    }
                }
            }
            Expr::Assign { left, op, value } => {
                let value_type = self.typecheck_expr(env, value)?;

                match left.as_ref() {
                    Expr::Variable(name) => {
                        let var_type = match env.get_var_type(&name.lexeme) {
                            Some(x) => x,
                            None => {
                                return error!(
                                    name.loc,
                                    format!("undefined variable: {}", &name.lexeme)
                                );
                            }
                        };
                        expect_type!(*var_type, value_type, name.loc);
                    }
                    Expr::Index {
                        expr,
                        bracket,
                        index,
                    } => {
                        expect_type!(self.typecheck_expr(env, expr)?, "ptr", bracket.loc);
                        expect_type!(self.typecheck_expr(env, index)?, "u8", bracket.loc);
                    }
                    Expr::MemberAccess { left, field } => {
                        let left_type = self.typecheck_expr(env, left)?;

                        let analyzer = self.analyzer.borrow();
                        let fields = match analyzer.structs.get(&left_type) {
                            Some(f) => f,
                            None => {
                                return error!(
                                    &field.loc,
                                    format!("unknown struct type: {}", left_type)
                                );
                            }
                        };

                        let f = match fields.get(&field.lexeme) {
                            Some(o) => o,
                            None => {
                                return error!(
                                    &field.loc,
                                    format!("unknown field: {}", &field.lexeme)
                                );
                            }
                        };

                        expect_type!(value_type, f.field_type, field.loc);
                    }
                    _ => return error!(&op.loc, "invalid assignment target"),
                }
                Ok(value_type)
            }
            Expr::Call {
                callee,
                paren,
                args,
            } => {
                if let Expr::Variable(callee_name) = &**callee {
                    if self
                        .analyzer
                        .borrow()
                        .functions
                        .contains_key(&callee_name.lexeme)
                    {
                        // its a function (defined/builtin/extern)
                    } else {
                        // its a variable containing function address
                        expect_type!(self.typecheck_expr(env, callee)?, "fnptr", paren.loc);
                    }
                } else {
                    // its an expression that evalutes to function address
                    expect_type!(self.typecheck_expr(env, callee)?, "fnptr", paren.loc);
                }

                for arg in args {
                    // TODO: actually check against the function we're calling
                    self.typecheck_expr(env, arg)?;
                }

                // TODO: actually look up return type
                Ok("any".into())
            }
            Expr::ArrayLiteral(exprs) => todo!(),
            Expr::Index {
                expr,
                bracket,
                index,
            } => todo!(),
            Expr::AddrOf { op, expr } => todo!(),
            Expr::New(token) => todo!(),
            Expr::MemberAccess { left, field } => {
                let left_type = self.typecheck_expr(env, left)?;

                let analyzer = self.analyzer.borrow();
                let fields = match analyzer.structs.get(&left_type) {
                    Some(f) => f,
                    None => {
                        return error!(&field.loc, format!("unknown struct type: {}", left_type));
                    }
                };

                let field = match fields.get(&field.lexeme) {
                    Some(o) => o,
                    None => return error!(&field.loc, format!("unknown field: {}", &field.lexeme)),
                };

                Ok(field.field_type.clone())
            }
            Expr::Cast { expr, type_name } => {
                self.typecheck_expr(env, expr)?;
                Ok(type_name.lexeme.clone())
            }
        }
    }

    fn is_valid_type_name(&self, name: &str) -> bool {
        if BUILTIN_TYPES.contains(&name) {
            return true;
        }
        if self.analyzer.borrow().structs.contains_key(name) {
            return true;
        }
        false
    }
}
