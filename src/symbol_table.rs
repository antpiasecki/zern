use std::collections::HashMap;

use crate::{
    parser::{Params, Stmt},
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

pub struct SymbolTable {
    pub functions: HashMap<String, FnType>,
    pub constants: HashMap<String, u64>,
    pub structs: HashMap<String, HashMap<String, StructField>>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
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

    pub fn register_declaration(&mut self, stmt: &Stmt) -> Result<(), ZernError> {
        match stmt {
            Stmt::Const { name, value } => {
                if self.is_name_defined(&name.lexeme) {
                    return error!(name.loc, format!("tried to redefine '{}'", name.lexeme));
                }
                if value.lexeme.starts_with("0x") {
                    self.constants.insert(
                        name.lexeme.clone(),
                        u64::from_str_radix(&value.lexeme[2..], 16).unwrap(),
                    );
                } else if value.lexeme.starts_with("0o") {
                    self.constants.insert(
                        name.lexeme.clone(),
                        u64::from_str_radix(&value.lexeme[2..], 8).unwrap(),
                    );
                } else {
                    self.constants
                        .insert(name.lexeme.clone(), value.lexeme.parse().unwrap());
                }
            }
            Stmt::Extern(name) => {
                if self.is_name_defined(&name.lexeme) {
                    return error!(name.loc, format!("tried to redefine '{}'", name.lexeme));
                }
                self.functions
                    .insert(name.lexeme.clone(), FnType::new_variadic("any"));
            }
            Stmt::Function {
                name,
                params,
                return_type,
                body: _,
                exported: _,
            } => {
                if self.is_name_defined(&name.lexeme) {
                    return error!(name.loc, format!("tried to redefine '{}'", name.lexeme));
                }
                match params {
                    Params::Normal(params) => self.functions.insert(
                        name.lexeme.clone(),
                        FnType {
                            return_type: return_type.lexeme.clone(),
                            params: Some(
                                params.iter().map(|x| x.var_type.lexeme.clone()).collect(),
                            ),
                        },
                    ),
                    Params::Variadic(name) => self.functions.insert(
                        name.lexeme.clone(),
                        FnType {
                            return_type: return_type.lexeme.clone(),
                            params: None,
                        },
                    ),
                };
            }
            Stmt::Struct { name, fields } => {
                if self.is_name_defined(&name.lexeme) {
                    return error!(name.loc, format!("tried to redefine '{}'", name.lexeme));
                }
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
            _ => {}
        }
        Ok(())
    }

    fn is_name_defined(&self, s: &str) -> bool {
        self.functions.contains_key(s)
            || self.constants.contains_key(s)
            || self.structs.contains_key(s)
    }
}
