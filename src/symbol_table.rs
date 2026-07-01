use std::collections::HashMap;

use crate::{
    parser::{Params, Stmt},
    tokenizer::{ZernError, error},
};

pub struct StructField {
    pub offset: usize,
    pub field_type: String,
}

#[derive(Clone)]
pub enum FnParams {
    Normal(Vec<String>),
    Variadic,
}

#[derive(Clone)]
pub struct FnType {
    pub return_type: String,
    pub params: FnParams,
}

impl FnType {
    fn new(return_type: &str, params: Vec<&str>) -> FnType {
        FnType {
            return_type: return_type.to_string(),
            params: FnParams::Normal(params.iter().map(|x| x.to_string()).collect()),
        }
    }

    fn new_variadic(return_type: &str) -> FnType {
        FnType {
            return_type: return_type.to_string(),
            params: FnParams::Variadic,
        }
    }
}

pub struct SymbolTable {
    pub functions: HashMap<String, FnType>,
    pub constants: HashMap<String, i64>,
    pub structs: HashMap<String, HashMap<String, StructField>>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            functions: HashMap::from([
                ("_builtin_heap_head".into(), FnType::new("ptr", vec![])),
                ("_builtin_heap_tail".into(), FnType::new("ptr", vec![])),
                ("_builtin_read64".into(), FnType::new("i64", vec!["ptr"])),
                (
                    "_builtin_set64".into(),
                    FnType::new("void", vec!["ptr", "i64"]),
                ),
                ("_builtin_cvtsi2sd".into(), FnType::new("f64", vec!["i64"])),
                ("_builtin_cvttsd2si".into(), FnType::new("i64", vec!["f64"])),
                (
                    "_builtin_f64_to_f32".into(),
                    FnType::new("any", vec!["f64"]),
                ),
                ("_builtin_syscall".into(), FnType::new_variadic("i64")),
                ("_builtin_environ".into(), FnType::new("ptr", vec![])),
                ("_var_arg".into(), FnType::new("any", vec!["i64"])),
                ("_stackalloc".into(), FnType::new("ptr", vec!["i64"])),
            ]),
            constants: HashMap::new(),
            structs: HashMap::new(),
        }
    }

    pub fn register_declaration(&mut self, stmt: &Stmt) -> Result<(), ZernError> {
        match stmt {
            Stmt::Const { name, value, neg } => {
                if self.is_name_defined(&name.lexeme) {
                    return error!(name.loc, format!("tried to redefine '{}'", name.lexeme));
                }
                let mut value = if value.lexeme.starts_with("0x") {
                    match u64::from_str_radix(&value.lexeme[2..], 16) {
                        Ok(v) => v,
                        Err(_) => return error!(value.loc, "failed to parse hex numeric constant"),
                    }
                } else {
                    match value.lexeme.parse() {
                        Ok(v) => v,
                        Err(_) => return error!(value.loc, "failed to parse numeric constant"),
                    }
                } as i64;
                if *neg {
                    value = -value;
                }
                self.constants.insert(name.lexeme.clone(), value);
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
                return_types,
                body: _,
                exported: _,
            } => {
                if self.is_name_defined(&name.lexeme) {
                    return error!(name.loc, format!("tried to redefine '{}'", name.lexeme));
                }
                let return_type = return_types
                    .iter()
                    .map(|t| t.lexeme.clone())
                    .collect::<Vec<_>>()
                    .join(",");
                match params {
                    Params::Normal(params) => self.functions.insert(
                        name.lexeme.clone(),
                        FnType {
                            return_type,
                            params: FnParams::Normal(
                                params.iter().map(|x| x.var_type.lexeme.clone()).collect(),
                            ),
                        },
                    ),
                    Params::Variadic => self.functions.insert(
                        name.lexeme.clone(),
                        FnType {
                            return_type,
                            params: FnParams::Variadic,
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
