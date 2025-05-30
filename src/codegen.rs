use std::{collections::HashMap, error::Error};

use crate::parser::{Expr, Stmt};

pub struct Var {
    pub var_type: String,
    pub stack_offset: usize,
}

pub struct Env {
    scopes: Vec<HashMap<String, Var>>,
    next_offset: usize,
}

impl Env {
    pub fn new() -> Env {
        Env {
            scopes: vec![HashMap::new()],
            next_offset: 1,
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
        self.next_offset += 1;
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

pub trait Codegen {
    fn get_output(&self) -> String;
    fn emit_prologue(&mut self) -> Result<(), Box<dyn Error>>;
    fn emit_epilogue(&mut self) -> Result<(), Box<dyn Error>>;
    fn compile_stmt(&mut self, env: &mut Env, stmt: Stmt) -> Result<(), Box<dyn Error>>;
    fn compile_expr(&mut self, env: &mut Env, expr: Expr) -> Result<(), Box<dyn Error>>;
}
