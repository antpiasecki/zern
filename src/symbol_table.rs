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
    pub attributes: Vec<String>,
}

impl FnType {
    fn new(params: Vec<&str>, return_type: &str) -> FnType {
        FnType {
            return_type: return_type.to_string(),
            params: FnParams::Normal(params.iter().map(|x| x.to_string()).collect()),
            attributes: vec![],
        }
    }

    fn new_variadic(return_type: &str) -> FnType {
        FnType {
            return_type: return_type.to_string(),
            params: FnParams::Variadic,
            attributes: vec![],
        }
    }
}

pub struct SymbolTable {
    pub functions: HashMap<String, FnType>,
    pub generic_functions: HashMap<String, Stmt>,
    pub constants: HashMap<String, i64>,
    pub structs: HashMap<String, HashMap<String, StructField>>,
    pub globals: HashMap<String, String>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            functions: HashMap::from([
                ("_builtin_read64".into(), FnType::new(vec!["ptr"], "i64")),
                ("_builtin_write64".into(), FnType::new(vec!["ptr", "i64"], "void")),
                ("_builtin_f64_to_f32".into(), FnType::new(vec!["f64"], "opaque")),
                ("_builtin_f32_to_f64".into(), FnType::new(vec!["f64"], "f64")),
                ("_builtin_syscall".into(), FnType::new_variadic("i64")),
                ("_var_arg".into(), FnType::new(vec!["i64"], "opaque")),
                ("_stackalloc".into(), FnType::new(vec!["i64"], "ptr")),
            ]),
            generic_functions: HashMap::new(),
            constants: HashMap::new(),
            structs: HashMap::new(),
            globals: HashMap::from([("_builtin_environ".into(), "_builtin_environ".into())]),
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
            Stmt::Function {
                name,
                params,
                return_types,
                type_vars,
                body,
                attributes,
            } => {
                if self.is_name_defined(&name.lexeme) {
                    return error!(name.loc, format!("tried to redefine '{}'", name.lexeme));
                }

                if type_vars.is_empty() {
                    let return_type = return_types
                        .iter()
                        .map(|t| t.lexeme.clone())
                        .collect::<Vec<_>>()
                        .join(",");
                    let attributes = attributes.iter().map(|t| t.lexeme.clone()).collect::<Vec<_>>();
                    match params {
                        Params::Normal(params) => self.functions.insert(
                            name.lexeme.clone(),
                            FnType {
                                return_type,
                                params: FnParams::Normal(params.iter().map(|x| x.var_type.lexeme.clone()).collect()),
                                attributes,
                            },
                        ),
                        Params::Variadic => self.functions.insert(
                            name.lexeme.clone(),
                            FnType {
                                return_type,
                                params: FnParams::Variadic,
                                attributes,
                            },
                        ),
                    };
                } else {
                    self.generic_functions.insert(
                        name.lexeme.clone(),
                        Stmt::Function {
                            name: name.clone(),
                            params: params.clone(),
                            return_types: return_types.clone(),
                            type_vars: type_vars.clone(),
                            body: body.clone(),
                            attributes: attributes.clone(),
                        },
                    );
                }
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
            Stmt::GlobalVariable(name) => {
                if self.is_name_defined(&name.lexeme) {
                    return error!(name.loc, format!("tried to redefine '{}'", name.lexeme));
                }
                let label = format!("global_{:03}", self.globals.len());
                self.globals.insert(name.lexeme.clone(), label);
            }
            _ => {}
        }
        Ok(())
    }

    fn is_name_defined(&self, s: &str) -> bool {
        self.functions.contains_key(s)
            || self.generic_functions.contains_key(s)
            || self.constants.contains_key(s)
            || self.structs.contains_key(s)
            || self.globals.contains_key(s)
    }
}
