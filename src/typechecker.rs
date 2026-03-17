use std::{cell::RefCell, rc::Rc};

use crate::{
    analyzer::Analyzer,
    parser::{Expr, Stmt},
    tokenizer::ZernError,
};

pub struct TypeChecker {
    analyzer: Rc<RefCell<Analyzer>>,
}

type Type = String;

impl TypeChecker {
    pub fn new(analyzer: Rc<RefCell<Analyzer>>) -> TypeChecker {
        TypeChecker { analyzer }
    }

    pub fn typecheck_stmt(&mut self, stmt: &Stmt) -> Result<(), ZernError> {
        match stmt {
            _ => todo!(),
        }
    }

    pub fn typecheck_expr(&mut self, expr: &Expr) -> Result<Type, ZernError> {
        match expr {
            _ => todo!(),
        }
    }
}
