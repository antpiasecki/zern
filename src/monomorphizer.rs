use std::{
    collections::{HashMap, HashSet},
    sync::atomic::Ordering,
};

use crate::{
    parser::{Expr, ExprKind, NEXT_EXPR_ID, Param, Params, Stmt},
    symbol_table::SymbolTable,
    tokenizer::{Token, ZernError, error},
};

pub struct Monomorphizer<'a> {
    symbol_table: &'a mut SymbolTable,
    function_queue: Vec<(Token, Vec<Token>)>,
    struct_queue: Vec<(Token, Vec<Token>)>,
    requested_functions: HashSet<String>,
    requested_structs: HashSet<String>,
}

impl<'a> Monomorphizer<'a> {
    pub fn new(symbol_table: &'a mut SymbolTable) -> Self {
        Self {
            symbol_table,
            function_queue: vec![],
            struct_queue: vec![],
            requested_functions: HashSet::new(),
            requested_structs: HashSet::new(),
        }
    }

    pub fn monomorphize(&mut self, mut statements: Vec<Stmt>) -> Result<Vec<Stmt>, ZernError> {
        let empty_bindings = HashMap::new();
        for stmt in &mut statements {
            match stmt {
                Stmt::Function {
                    params,
                    return_types,
                    type_vars,
                    body,
                    ..
                } if type_vars.is_empty() => {
                    let substituted_params = self.substitute_params(params, &empty_bindings);
                    let substituted_return_types = return_types
                        .iter()
                        .map(|t| self.substitute_type_token(t, &empty_bindings))
                        .collect();
                    let substituted_body = self.substitute_stmt(body, &empty_bindings);

                    *params = substituted_params;
                    *return_types = substituted_return_types;
                    *body = Box::new(substituted_body);
                }
                Stmt::Struct { type_vars, fields, .. } if type_vars.is_empty() => {
                    for field in fields {
                        field.var_type = self.substitute_type_token(&field.var_type, &empty_bindings);
                    }
                }
                Stmt::GlobalVariable { var_type, .. } => {
                    *var_type = self.substitute_type_token(var_type, &empty_bindings);
                }
                _ => {}
            }
        }

        let mut new_structs = vec![];
        let mut new_fns = vec![];

        while !self.function_queue.is_empty() || !self.struct_queue.is_empty() {
            self.instantiate_structs(&mut new_structs)?;

            while let Some((requested_name, types)) = self.function_queue.pop() {
                let Some(f) = self.symbol_table.generic_functions.get(&requested_name.lexeme).cloned() else {
                    continue;
                };

                let Stmt::Function {
                    name,
                    params,
                    return_types,
                    type_vars,
                    body,
                    attributes,
                } = f
                else {
                    unreachable!()
                };

                if types.len() != type_vars.len() {
                    return error!(
                        requested_name.loc,
                        format!(
                            "'{}' expects {} type argument(s), got {}",
                            name.lexeme,
                            type_vars.len(),
                            types.len()
                        )
                    );
                }

                let bindings: HashMap<String, Token> = type_vars
                    .iter()
                    .map(|tv| tv.lexeme.clone())
                    .zip(types.iter().cloned())
                    .collect();

                let mut mangled_name_token = name.clone();
                mangled_name_token.lexeme = type_name(&name.lexeme, &types);

                let substituted_params = self.substitute_params(&params, &bindings);
                let substituted_return_types: Vec<Token> = return_types
                    .iter()
                    .map(|t| self.substitute_type_token(t, &bindings))
                    .collect();
                let substituted_body = Box::new(self.substitute_stmt(&body, &bindings));

                new_fns.push(Stmt::Function {
                    name: mangled_name_token,
                    params: substituted_params,
                    return_types: substituted_return_types,
                    type_vars: vec![],
                    body: substituted_body,
                    attributes: attributes.clone(),
                });
            }
        }

        for structure in &new_structs {
            self.symbol_table.register_declaration(structure)?;
        }

        for f in &new_fns {
            self.symbol_table.register_declaration(f)?;
        }

        statements.retain(|s| !matches!(s, Stmt::Function { type_vars, .. } if !type_vars.is_empty()));
        statements.retain(|s| !matches!(s, Stmt::Struct { type_vars, .. } if !type_vars.is_empty()));
        statements.extend(new_structs);
        statements.extend(new_fns);

        Ok(statements)
    }

    fn enqueue_function(&mut self, name: Token, types: Vec<Token>) {
        if !self.symbol_table.generic_functions.contains_key(&name.lexeme) {
            return;
        }

        let key = type_name(&name.lexeme, &types);
        if self.requested_functions.insert(key) {
            self.function_queue.push((name, types));
        }
    }

    fn enqueue_method(&mut self, method: Token, types: Vec<Token>) {
        if types.is_empty() {
            return;
        }

        let mut matching_names: Vec<String> = self
            .symbol_table
            .generic_functions
            .iter()
            .filter_map(|(name, template)| {
                if !(name == &method.lexeme
                    || name
                        .rsplit_once('.')
                        .is_some_and(|(_, method_name)| method_name == method.lexeme))
                {
                    return None;
                }

                let Stmt::Function { type_vars, .. } = template else {
                    return None;
                };

                if type_vars.len() != types.len()
                    || name
                        .rsplit_once('.')
                        .is_some_and(|(owner, _)| self.symbol_table.generic_structs.contains_key(owner))
                {
                    return None;
                }

                Some(name.clone())
            })
            .collect();
        matching_names.sort();

        for name in matching_names {
            let mut method_name = method.clone();
            method_name.lexeme = name;
            self.enqueue_function(method_name, types.clone());
        }
    }

    fn enqueue_struct(&mut self, name: Token, types: Vec<Token>) {
        if !self.symbol_table.generic_structs.contains_key(&name.lexeme) {
            return;
        }

        let key = type_name(&name.lexeme, &types);
        if self.requested_structs.insert(key) {
            self.enqueue_struct_methods(&name.lexeme, &types);
            self.struct_queue.push((name, types));
        }
    }

    fn enqueue_struct_methods(&mut self, owner: &str, types: &[Token]) {
        let receiver_type = type_name(owner, types);
        let mut methods: Vec<(Token, Vec<Token>)> = self
            .symbol_table
            .generic_functions
            .iter()
            .filter_map(|(function_name, template)| {
                let (method_owner, _) = function_name.rsplit_once('.')?;
                if method_owner != owner {
                    return None;
                }

                let type_args = infer_method_type_args(&receiver_type, template);
                if type_args.is_empty() {
                    return None;
                }

                let Stmt::Function { name, .. } = template else {
                    return None;
                };

                let mut method_name = name.clone();
                method_name.lexeme = function_name.clone();
                Some((method_name, type_args))
            })
            .collect();
        methods.sort_by(|(left, _), (right, _)| left.lexeme.cmp(&right.lexeme));

        for (method, type_args) in methods {
            self.enqueue_function(method, type_args);
        }
    }

    fn instantiate_structs(&mut self, new_structs: &mut Vec<Stmt>) -> Result<(), ZernError> {
        while let Some((name, types)) = self.struct_queue.pop() {
            let Some(template) = self.symbol_table.generic_structs.get(&name.lexeme).cloned() else {
                continue;
            };

            let Stmt::Struct {
                name: template_name,
                type_vars,
                fields,
            } = template
            else {
                unreachable!()
            };

            if types.len() != type_vars.len() {
                return error!(
                    name.loc,
                    format!(
                        "'{}' expects {} type argument(s), got {}",
                        template_name.lexeme,
                        type_vars.len(),
                        types.len()
                    )
                );
            }

            let bindings: HashMap<String, Token> = type_vars
                .iter()
                .map(|tv| tv.lexeme.clone())
                .zip(types.iter().cloned())
                .collect();

            let mut concrete_name = template_name.clone();
            concrete_name.lexeme = type_name(&name.lexeme, &types);
            new_structs.push(Stmt::Struct {
                name: concrete_name,
                type_vars: vec![],
                fields: fields
                    .iter()
                    .map(|field| Param {
                        var_type: self.substitute_type_token(&field.var_type, &bindings),
                        var_name: field.var_name.clone(),
                    })
                    .collect(),
            });
        }

        Ok(())
    }

    fn substitute_type_token(&mut self, token: &Token, bindings: &HashMap<String, Token>) -> Token {
        let result = substitute_token(token, bindings);
        self.enqueue_type(&result);
        result
    }

    fn substitute_params(&mut self, params: &Params, bindings: &HashMap<String, Token>) -> Params {
        match params {
            Params::Normal(ps) => Params::Normal(
                ps.iter()
                    .map(|p| Param {
                        var_type: self.substitute_type_token(&p.var_type, bindings),
                        var_name: p.var_name.clone(),
                    })
                    .collect(),
            ),
            Params::Variadic => Params::Variadic,
        }
    }

    fn substitute_expr(&mut self, e: &Expr, bindings: &HashMap<String, Token>) -> Expr {
        let kind = match &e.kind {
            ExprKind::Binary { left, op, right } => ExprKind::Binary {
                left: Box::new(self.substitute_expr(left, bindings)),
                op: op.clone(),
                right: Box::new(self.substitute_expr(right, bindings)),
            },
            ExprKind::Logical { left, op, right } => ExprKind::Logical {
                left: Box::new(self.substitute_expr(left, bindings)),
                op: op.clone(),
                right: Box::new(self.substitute_expr(right, bindings)),
            },
            ExprKind::Grouping(expr) => ExprKind::Grouping(Box::new(self.substitute_expr(expr, bindings))),
            ExprKind::Literal(token) => ExprKind::Literal(token.clone()),
            ExprKind::Unary { op, right } => ExprKind::Unary {
                op: op.clone(),
                right: Box::new(self.substitute_expr(right, bindings)),
            },
            ExprKind::Variable(token) => ExprKind::Variable(token.clone()),
            ExprKind::Call {
                callee,
                paren,
                args,
                type_args,
            } => {
                let callee = self.substitute_expr(callee, bindings);
                let args = args.iter().map(|arg| self.substitute_expr(arg, bindings)).collect();
                let type_args = type_args
                    .iter()
                    .map(|t| substitute_token(t, bindings))
                    .collect::<Vec<_>>();

                if let ExprKind::Variable(name) = &callee.kind {
                    self.enqueue_function(name.clone(), type_args.clone());
                }

                ExprKind::Call {
                    callee: Box::new(callee),
                    paren: paren.clone(),
                    args,
                    type_args,
                }
            }
            ExprKind::ArrayLiteral(exprs) => {
                ExprKind::ArrayLiteral(exprs.iter().map(|x| self.substitute_expr(x, bindings)).collect())
            }
            ExprKind::Index {
                indexed,
                bracket,
                is_offset,
                index,
            } => ExprKind::Index {
                indexed: Box::new(self.substitute_expr(indexed, bindings)),
                bracket: bracket.clone(),
                is_offset: *is_offset,
                index: Box::new(self.substitute_expr(index, bindings)),
            },
            ExprKind::AddrOf { op, expr } => ExprKind::AddrOf {
                op: op.clone(),
                expr: Box::new(self.substitute_expr(expr, bindings)),
            },
            ExprKind::New { struct_name, use_heap } => ExprKind::New {
                struct_name: self.substitute_type_token(struct_name, bindings),
                use_heap: *use_heap,
            },
            ExprKind::MemberAccess { left, field } => ExprKind::MemberAccess {
                left: Box::new(self.substitute_expr(left, bindings)),
                field: field.clone(),
            },
            ExprKind::Cast { casted, type_name } => ExprKind::Cast {
                casted: Box::new(self.substitute_expr(casted, bindings)),
                type_name: self.substitute_type_token(type_name, bindings),
            },
            ExprKind::MethodCall {
                callee,
                method,
                args,
                type_args,
            } => {
                let callee = self.substitute_expr(callee, bindings);
                let args = args.iter().map(|arg| self.substitute_expr(arg, bindings)).collect();
                let type_args = type_args
                    .iter()
                    .map(|t| substitute_token(t, bindings))
                    .collect::<Vec<_>>();

                self.enqueue_method(method.clone(), type_args.clone());

                ExprKind::MethodCall {
                    callee: Box::new(callee),
                    method: method.clone(),
                    args,
                    type_args,
                }
            }
        };
        Expr {
            id: NEXT_EXPR_ID.fetch_add(1, Ordering::SeqCst),
            kind,
        }
    }

    fn substitute_stmt(&mut self, s: &Stmt, bindings: &HashMap<String, Token>) -> Stmt {
        match s {
            Stmt::Expression(e) => Stmt::Expression(self.substitute_expr(e, bindings)),
            Stmt::Declare { name, initializer } => Stmt::Declare {
                name: name.clone(),
                initializer: self.substitute_expr(initializer, bindings),
            },
            Stmt::Assign { left, op, value } => Stmt::Assign {
                left: self.substitute_expr(left, bindings),
                op: op.clone(),
                value: self.substitute_expr(value, bindings),
            },
            Stmt::Destructure { targets, op, value } => Stmt::Destructure {
                targets: targets.clone(),
                op: op.clone(),
                value: self.substitute_expr(value, bindings),
            },
            Stmt::Block(stmts) => Stmt::Block(stmts.iter().map(|x| self.substitute_stmt(x, bindings)).collect()),
            Stmt::If {
                keyword,
                condition,
                then_branch,
                else_branch,
            } => Stmt::If {
                keyword: keyword.clone(),
                condition: self.substitute_expr(condition, bindings),
                then_branch: Box::new(self.substitute_stmt(then_branch, bindings)),
                else_branch: Box::new(self.substitute_stmt(else_branch, bindings)),
            },
            Stmt::While {
                keyword,
                condition,
                body,
            } => Stmt::While {
                keyword: keyword.clone(),
                condition: self.substitute_expr(condition, bindings),
                body: Box::new(self.substitute_stmt(body, bindings)),
            },
            Stmt::For {
                var,
                start,
                end,
                is_inclusive,
                body,
            } => Stmt::For {
                var: var.clone(),
                start: self.substitute_expr(start, bindings),
                end: self.substitute_expr(end, bindings),
                is_inclusive: *is_inclusive,
                body: Box::new(self.substitute_stmt(body, bindings)),
            },
            Stmt::Return { keyword, exprs } => Stmt::Return {
                keyword: keyword.clone(),
                exprs: exprs.iter().map(|x| self.substitute_expr(x, bindings)).collect(),
            },
            Stmt::Break(token) => Stmt::Break(token.clone()),
            Stmt::Continue(token) => Stmt::Continue(token.clone()),
            Stmt::Defer { keyword, block } => Stmt::Defer {
                keyword: keyword.clone(),
                block: Box::new(self.substitute_stmt(block, bindings)),
            },
            _ => unreachable!(),
        }
    }

    fn enqueue_type(&mut self, token: &Token) {
        let Some((name, args)) = split_type_name(&token.lexeme) else {
            return;
        };

        let mut name_token = token.clone();
        name_token.lexeme = name.to_string();
        let args = args
            .into_iter()
            .map(|arg| {
                let mut result = token.clone();
                result.lexeme = arg.to_string();
                result
            })
            .collect::<Vec<_>>();

        self.enqueue_struct(name_token, args.clone());
        for arg in args {
            self.enqueue_type(&arg);
        }
    }
}

pub fn split_type_args(input: &str) -> Vec<&str> {
    let mut result = Vec::new();
    let mut depth = 0;
    let mut start = 0;

    for (i, c) in input.char_indices() {
        match c {
            '<' => depth += 1,
            '>' => depth -= 1,
            ',' if depth == 0 => {
                result.push(input[start..i].trim());
                start = i + 1;
            }
            _ => {}
        }
    }

    if start < input.len() {
        result.push(input[start..].trim());
    }

    result
}

fn split_type_name(input: &str) -> Option<(&str, Vec<&str>)> {
    let open = input.find('<')?;
    let inner = input.strip_suffix('>')?;
    let name = input[..open].trim();
    if name.is_empty() {
        return None;
    }

    Some((name, split_type_args(&inner[open + 1..])))
}

fn substitute_token(t: &Token, bindings: &HashMap<String, Token>) -> Token {
    let mut result = t.clone();
    let mut output = String::new();
    let mut word = String::new();

    for c in t.lexeme.chars().chain(std::iter::once(' ')) {
        if c.is_ascii_alphanumeric() || c == '_' {
            word.push(c);
        } else {
            if let Some(bound) = bindings.get(&word) {
                output.push_str(&bound.lexeme);
            } else {
                output.push_str(&word);
            }

            word.clear();
            output.push(c);
        }
    }

    result.lexeme = output.trim_end().to_string();
    result
}

pub fn type_name(name: &str, types: &[Token]) -> String {
    format!(
        "{}<{}>",
        name,
        types.iter().map(|t| t.lexeme.as_str()).collect::<Vec<_>>().join(", ")
    )
}

pub fn method_owner(receiver_type: &str) -> &str {
    receiver_type.split("<").next().unwrap_or(receiver_type)
}

pub fn infer_method_type_args(receiver_type: &str, method: &Stmt) -> Vec<Token> {
    let Stmt::Function {
        type_vars,
        params: Params::Normal(params),
        ..
    } = method
    else {
        return vec![];
    };

    if params.is_empty() || type_vars.is_empty() {
        return vec![];
    }

    let Some(open) = receiver_type.find('<') else {
        return vec![];
    };

    let Some(inner) = receiver_type.strip_suffix('>') else {
        return vec![];
    };

    let args = split_type_args(&inner[open + 1..]);

    if args.len() != type_vars.len() {
        return vec![];
    }

    type_vars
        .iter()
        .zip(args)
        .map(|(var, arg)| {
            let mut token = var.clone();
            token.lexeme = arg.to_string();
            token
        })
        .collect()
}
