use std::{
    collections::{HashMap, HashSet},
    sync::atomic::Ordering,
};

use crate::{
    parser::{Expr, ExprKind, NEXT_EXPR_ID, Param, Params, Stmt},
    symbol_table::SymbolTable,
    tokenizer::{Token, ZernError, error},
};

pub fn monomorphize(mut statements: Vec<Stmt>, symbol_table: &mut SymbolTable) -> Result<Vec<Stmt>, ZernError> {
    let mut instantiations: Vec<(&Token, &Vec<Token>)> = vec![];
    for stmt in &statements {
        if let Stmt::Instantiation { name, types } = stmt {
            instantiations.push((name, types));
        }
    }

    let mut new_fns = vec![];
    let mut already_done: HashSet<String> = HashSet::new();

    for (_, f) in &symbol_table.generic_functions {
        let Stmt::Function {
            name,
            params,
            return_types,
            type_vars,
            body,
            exported,
        } = f
        else {
            unreachable!()
        };

        for (insta_name, types) in &instantiations {
            if insta_name.lexeme != name.lexeme {
                continue;
            }

            let mangled_name = mangle(name.lexeme.clone(), types);
            let mut mangled_name_token = name.clone();
            mangled_name_token.lexeme = mangled_name.clone();

            if already_done.contains(&mangled_name) {
                continue;
            }
            already_done.insert(mangled_name);

            if types.len() != type_vars.len() {
                return error!(
                    insta_name.loc,
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

            let substituted_params = substitute_params(params, &bindings);
            let substituted_return_types: Vec<Token> =
                return_types.iter().map(|t| substitute_token(t, &bindings)).collect();
            let substituted_body = Box::new(substitute_stmt(body, &bindings));

            new_fns.push(Stmt::Function {
                name: mangled_name_token,
                params: substituted_params,
                return_types: substituted_return_types,
                type_vars: vec![],
                body: substituted_body,
                exported: exported.clone(),
            });
        }
    }

    for f in &new_fns {
        symbol_table.register_declaration(f)?;
    }

    statements.retain(|s| !matches!(s, Stmt::Function { type_vars, .. } if !type_vars.is_empty()));
    statements.extend(new_fns);

    Ok(statements)
}

pub fn mangle(name: String, types: &Vec<Token>) -> String {
    format!(
        "{}${}",
        name,
        types
            .iter()
            .map(|t| t.lexeme.clone())
            .collect::<Vec<String>>()
            .join("$")
    )
}

fn substitute_token(t: &Token, bindings: &HashMap<String, Token>) -> Token {
    match bindings.get(&t.lexeme) {
        Some(bound) => bound.clone(),
        None => t.clone(),
    }
}

fn substitute_params(params: &Params, bindings: &HashMap<String, Token>) -> Params {
    match params {
        Params::Normal(ps) => Params::Normal(
            ps.iter()
                .map(|p| Param {
                    var_type: substitute_token(&p.var_type, bindings),
                    var_name: p.var_name.clone(),
                })
                .collect(),
        ),
        Params::Variadic => Params::Variadic,
    }
}

fn substitute_expr(e: &Expr, bindings: &HashMap<String, Token>) -> Expr {
    let kind = match &e.kind {
        ExprKind::Binary { left, op, right } => ExprKind::Binary {
            left: Box::new(substitute_expr(left, bindings)),
            op: op.clone(),
            right: Box::new(substitute_expr(right, bindings)),
        },
        ExprKind::Logical { left, op, right } => ExprKind::Logical {
            left: Box::new(substitute_expr(left, bindings)),
            op: op.clone(),
            right: Box::new(substitute_expr(right, bindings)),
        },
        ExprKind::Grouping(expr) => ExprKind::Grouping(Box::new(substitute_expr(expr, bindings))),
        ExprKind::Literal(token) => ExprKind::Literal(token.clone()),
        ExprKind::Unary { op, right } => ExprKind::Unary {
            op: op.clone(),
            right: Box::new(substitute_expr(right, bindings)),
        },
        ExprKind::Variable(token) => ExprKind::Variable(token.clone()),
        ExprKind::Call {
            callee,
            paren,
            args,
            type_args,
        } => ExprKind::Call {
            callee: Box::new(substitute_expr(callee, bindings)),
            paren: paren.clone(),
            args: args.iter().map(|a| substitute_expr(a, bindings)).collect(),
            type_args: type_args.iter().map(|t| substitute_token(t, bindings)).collect(),
        },
        ExprKind::ArrayLiteral(exprs) => {
            ExprKind::ArrayLiteral(exprs.iter().map(|x| substitute_expr(x, bindings)).collect())
        }
        ExprKind::Index {
            indexed,
            bracket,
            is_offset,
            index,
        } => ExprKind::Index {
            indexed: Box::new(substitute_expr(indexed, bindings)),
            bracket: bracket.clone(),
            is_offset: *is_offset,
            index: Box::new(substitute_expr(index, bindings)),
        },
        ExprKind::AddrOf { op, expr } => ExprKind::AddrOf {
            op: op.clone(),
            expr: Box::new(substitute_expr(expr, bindings)),
        },
        ExprKind::New { struct_name, use_heap } => ExprKind::New {
            struct_name: substitute_token(struct_name, bindings),
            use_heap: *use_heap,
        },
        ExprKind::MemberAccess { left, field } => ExprKind::MemberAccess {
            left: Box::new(substitute_expr(left, bindings)),
            field: field.clone(),
        },
        ExprKind::Cast { casted, type_name } => ExprKind::Cast {
            casted: Box::new(substitute_expr(casted, bindings)),
            type_name: substitute_token(type_name, bindings),
        },
        ExprKind::MethodCall {
            callee,
            method,
            args,
            type_args,
        } => ExprKind::MethodCall {
            callee: Box::new(substitute_expr(callee, bindings)),
            method: method.clone(),
            args: args.iter().map(|a| substitute_expr(a, bindings)).collect(),
            type_args: type_args.iter().map(|t| substitute_token(t, bindings)).collect(),
        },
    };
    Expr {
        id: NEXT_EXPR_ID.fetch_add(1, Ordering::SeqCst),
        kind,
    }
}

fn substitute_stmt(s: &Stmt, bindings: &HashMap<String, Token>) -> Stmt {
    match s {
        Stmt::Expression(e) => Stmt::Expression(substitute_expr(e, bindings)),
        Stmt::Declare { name, initializer } => Stmt::Declare {
            name: name.clone(),
            initializer: substitute_expr(initializer, bindings),
        },
        Stmt::Assign { left, op, value } => Stmt::Assign {
            left: substitute_expr(left, bindings),
            op: op.clone(),
            value: substitute_expr(value, bindings),
        },
        Stmt::Destructure { targets, op, value } => Stmt::Destructure {
            targets: targets.clone(),
            op: op.clone(),
            value: substitute_expr(value, bindings),
        },
        Stmt::Const { name, value, neg } => Stmt::Const {
            name: name.clone(),
            value: value.clone(),
            neg: *neg,
        },
        Stmt::Block(stmts) => Stmt::Block(stmts.iter().map(|x| substitute_stmt(x, bindings)).collect()),
        Stmt::If {
            keyword,
            condition,
            then_branch,
            else_branch,
        } => Stmt::If {
            keyword: keyword.clone(),
            condition: substitute_expr(condition, bindings),
            then_branch: Box::new(substitute_stmt(then_branch, bindings)),
            else_branch: Box::new(substitute_stmt(else_branch, bindings)),
        },
        Stmt::While {
            keyword,
            condition,
            body,
        } => Stmt::While {
            keyword: keyword.clone(),
            condition: substitute_expr(condition, bindings),
            body: Box::new(substitute_stmt(body, bindings)),
        },
        Stmt::For {
            var,
            start,
            end,
            is_inclusive,
            body,
        } => Stmt::For {
            var: var.clone(),
            start: substitute_expr(start, bindings),
            end: substitute_expr(end, bindings),
            is_inclusive: *is_inclusive,
            body: Box::new(substitute_stmt(body, bindings)),
        },
        Stmt::Function {
            name,
            params,
            return_types,
            type_vars,
            body,
            exported,
        } => {
            let shadowed: HashSet<&str> = type_vars.iter().map(|t| t.lexeme.as_str()).collect();
            let filtered: HashMap<String, Token> = bindings
                .iter()
                .filter(|(k, _)| !shadowed.contains(k.as_str()))
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect();
            Stmt::Function {
                name: name.clone(),
                params: substitute_params(params, &filtered),
                return_types: return_types.iter().map(|t| substitute_token(t, &filtered)).collect(),
                type_vars: type_vars.clone(),
                body: Box::new(substitute_stmt(body, &filtered)),
                exported: *exported,
            }
        }
        Stmt::Return { keyword, exprs } => Stmt::Return {
            keyword: keyword.clone(),
            exprs: exprs.iter().map(|x| substitute_expr(x, bindings)).collect(),
        },
        Stmt::Break(token) => Stmt::Break(token.clone()),
        Stmt::Continue(token) => Stmt::Continue(token.clone()),
        Stmt::Extern {
            name,
            params,
            return_type,
        } => Stmt::Extern {
            name: name.clone(),
            params: substitute_params(params, bindings),
            return_type: substitute_token(return_type, bindings),
        },
        Stmt::Struct { name, fields } => Stmt::Struct {
            name: name.clone(),
            fields: fields
                .iter()
                .map(|f| Param {
                    var_type: substitute_token(&f.var_type, bindings),
                    var_name: f.var_name.clone(),
                })
                .collect(),
        },
        Stmt::GlobalVariable(token) => Stmt::GlobalVariable(token.clone()),
        Stmt::Defer { keyword, block } => Stmt::Defer {
            keyword: keyword.clone(),
            block: Box::new(substitute_stmt(block, bindings)),
        },
        Stmt::Instantiation { name, types } => Stmt::Instantiation {
            name: name.clone(),
            types: types.iter().map(|t| substitute_token(t, bindings)).collect(),
        },
    }
}
