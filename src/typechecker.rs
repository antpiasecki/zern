use std::collections::HashMap;

use crate::{
    parser::{Expr, ExprKind, Params, ScopeCall, Stmt, recursion_guard},
    symbol_table::{FnParams, SymbolTable},
    tokenizer::{Loc, TokenType, ZernError, error},
};

macro_rules! expect_type {
    ($expr_type:expr, $expected:expr, $loc:expr) => {{
        let actual = $expr_type;
        if $expected != "$" && actual != "$" && actual != $expected {
            return error!($loc, format!("expected type {}, got {}", $expected, actual));
        }
    }};
}

macro_rules! expect_types {
    ($expr_type:expr, [$( $expected:expr ),+], $loc:expr) => {
        if $expr_type != "$" && $( $expr_type != $expected )&&+ {
            return error!(
                $loc,
                format!(
                    "expected one of [{}], got {}",
                    [$( $expected ),+].join(", "),
                    $expr_type
                )
            );
        }
    };
}

static BUILTIN_TYPES: [&str; 8] = ["void", "u8", "i64", "f64", "str", "bool", "ptr", "opaque"];

pub struct Env {
    scopes: Vec<HashMap<String, String>>,
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
        assert!(!self.scopes.is_empty());
        self.scopes.pop();
    }

    pub fn define_var(&mut self, name: String, var_type: String) {
        self.scopes.last_mut().unwrap().insert(name, var_type);
    }

    fn get_var_type(&self, name: &str) -> Option<&String> {
        for scope in self.scopes.iter().rev() {
            if let Some(var) = scope.get(name) {
                return Some(var);
            }
        }
        None
    }
}

pub struct TypeChecker<'a> {
    symbol_table: &'a SymbolTable,
    pub expr_types: HashMap<usize, String>,
    current_function_return_type: String,
    depth: usize,
}

impl<'a> TypeChecker<'a> {
    pub fn new(symbol_table: &'a SymbolTable) -> TypeChecker<'a> {
        TypeChecker {
            symbol_table,
            expr_types: HashMap::new(),
            current_function_return_type: String::new(),
            depth: 0,
        }
    }

    pub fn typecheck_stmt(&mut self, env: &mut Env, stmt: &Stmt) -> Result<(), ZernError> {
        match stmt {
            Stmt::Expression(expr) => {
                self.typecheck_expr(env, expr)?;
            }
            Stmt::Declare { name, initializer } => {
                let actual_type = self.typecheck_expr(env, initializer)?;
                if actual_type.contains(',') {
                    return error!(
                        &name.loc,
                        "cannot assign multi-return call to a single variable"
                    );
                }

                env.define_var(name.lexeme.clone(), actual_type);
            }
            Stmt::Assign { left, op, value } => {
                let value_type = self.typecheck_expr(env, value)?;
                if value_type.contains(',') {
                    return error!(
                        &op.loc,
                        "cannot assign multi-return call to a single variable"
                    );
                }

                match &left.kind {
                    ExprKind::Variable(name) => {
                        let existing_var_type = match env.get_var_type(&name.lexeme) {
                            Some(x) => x,
                            None => {
                                return error!(
                                    name.loc,
                                    format!("undefined variable: {}", &name.lexeme)
                                );
                            }
                        };
                        expect_type!(value_type.clone(), *existing_var_type, name.loc);
                    }
                    ExprKind::Index {
                        expr,
                        bracket,
                        index,
                    } => {
                        expect_types!(self.typecheck_expr(env, expr)?, ["ptr", "str"], bracket.loc);
                        expect_types!(self.typecheck_expr(env, index)?, ["i64", "u8"], bracket.loc);
                        expect_types!(value_type.clone(), ["u8", "i64"], bracket.loc);
                    }
                    ExprKind::MemberAccess { left, field } => {
                        let left_type = self.typecheck_expr(env, left)?;
                        let (base_name, generic_args) = parse_generic_type(&left_type);

                        let fields = match self.symbol_table.structs.get(base_name) {
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

                        let field_type = if let Some(args) = generic_args {
                            substitute_type(&f.field_type, args[0])
                        } else {
                            f.field_type.clone()
                        };

                        expect_type!(value_type.clone(), field_type, field.loc);
                    }
                    _ => return error!(&op.loc, "invalid assignment target"),
                }
            }
            Stmt::Destructure { targets, op, value } => {
                let value_type = self.typecheck_expr(env, value)?;
                let types: Vec<&str> = value_type.split(',').collect();
                if types.len() != targets.len() {
                    return error!(
                        &op.loc,
                        "destructure target count does not match return count"
                    );
                }
                for (target, ty) in targets.iter().zip(types.iter()) {
                    match env.get_var_type(&target.lexeme) {
                        Some(existing) => {
                            expect_type!(ty.to_string(), *existing, target.loc);
                        }
                        None => {
                            env.define_var(target.lexeme.clone(), ty.to_string());
                        }
                    }
                }
            }
            Stmt::Const { .. } => {
                // handled in SymbolTable
            }
            Stmt::Block(stmts) => {
                env.push_scope();
                for stmt in stmts {
                    self.typecheck_stmt(env, stmt)?;
                }
                env.pop_scope();
            }
            Stmt::If {
                keyword,
                condition,
                then_branch,
                else_branch,
            } => {
                expect_types!(
                    self.typecheck_expr(env, condition)?,
                    ["i64", "u8", "ptr", "bool", "opaque"],
                    keyword.loc
                );
                self.typecheck_stmt(env, then_branch)?;
                self.typecheck_stmt(env, else_branch)?;
            }
            Stmt::While {
                keyword,
                condition,
                body,
            } => {
                expect_types!(
                    self.typecheck_expr(env, condition)?,
                    ["i64", "u8", "ptr", "bool"],
                    keyword.loc
                );
                self.typecheck_stmt(env, body)?;
            }
            Stmt::For {
                var,
                start,
                end,
                body,
            } => {
                expect_type!(self.typecheck_expr(env, start)?, "i64", var.loc);
                expect_type!(self.typecheck_expr(env, end)?, "i64", var.loc);

                env.push_scope();
                env.define_var(var.lexeme.clone(), "i64".into());
                self.typecheck_stmt(env, body)?;
                env.pop_scope();
            }
            Stmt::Function {
                name,
                params,
                return_types,
                body,
                exported: _,
            } => {
                let return_type = return_types
                    .iter()
                    .map(|t| t.lexeme.clone())
                    .collect::<Vec<_>>()
                    .join(",");

                if name.lexeme == "main" {
                    if return_type != "i64" {
                        return error!(&name.loc, "main function must return i64");
                    }

                    match params {
                        Params::Normal(params) => {
                            if !params.is_empty() && params.len() != 2 {
                                return error!(
                                    &name.loc,
                                    "main function must accept 0 or 2 parameters"
                                );
                            }

                            if params.len() == 2 {
                                if params[0].var_type.lexeme != "i64" {
                                    return error!(
                                        &name.loc,
                                        "first parameter of the main function must be an i64"
                                    );
                                }
                                if params[1].var_type.lexeme != "ptr" {
                                    return error!(
                                        &name.loc,
                                        "second parameter of the main function must be a ptr"
                                    );
                                }
                            }
                        }
                        Params::Variadic => {
                            return error!(&name.loc, "main function cannot be variadic");
                        }
                    }
                }

                if !self.is_valid_type_name(&return_type) {
                    return error!(
                        &return_types[0].loc,
                        "unrecognized type: ".to_owned() + &return_type
                    );
                }

                self.current_function_return_type = return_type.clone();

                env.push_scope();

                match params {
                    Params::Normal(params) => {
                        for param in params {
                            if !self.is_valid_type_name(&param.var_type.lexeme) {
                                return error!(
                                    &param.var_name.loc,
                                    "unrecognized type: ".to_owned() + &param.var_type.lexeme
                                );
                            }
                            env.define_var(
                                param.var_name.lexeme.clone(),
                                param.var_type.lexeme.clone(),
                            );
                        }
                    }
                    Params::Variadic => {}
                }

                self.typecheck_stmt(env, body)?;

                env.pop_scope();
            }
            Stmt::Return { keyword, exprs } => {
                let joined_type = if exprs.is_empty() {
                    "void".into()
                } else {
                    exprs
                        .iter()
                        .map(|e| self.typecheck_expr(env, e))
                        .collect::<Result<Vec<String>, _>>()?
                        .join(",")
                };
                expect_type!(joined_type, self.current_function_return_type, keyword.loc);
            }
            Stmt::Break => {}
            Stmt::Continue => {}
            Stmt::Extern(_) => {
                // handled in the SymbolTable
            }
            Stmt::Struct { name: _, fields } => {
                for field in fields {
                    if !self.is_valid_type_name(&field.var_type.lexeme) {
                        return error!(
                            &field.var_type.loc,
                            format!("unknown type: {}", &field.var_type.lexeme)
                        );
                    }
                }
            }
        }
        Ok(())
    }

    pub fn typecheck_expr(&mut self, env: &mut Env, expr: &Expr) -> Result<String, ZernError> {
        recursion_guard!(self);

        let expr_type = match &expr.kind {
            ExprKind::Binary { left, op, right } => {
                let left_type = self.typecheck_expr(env, left)?;

                match op.token_type {
                    TokenType::Plus | TokenType::Minus => {
                        expect_types!(left_type, ["i64", "ptr", "u8"], op.loc);
                        expect_types!(
                            self.typecheck_expr(env, right)?,
                            ["i64", "ptr", "u8"],
                            op.loc
                        );
                        Ok(left_type)
                    }
                    TokenType::Star
                    | TokenType::Slash
                    | TokenType::Mod
                    | TokenType::Xor
                    | TokenType::BitAnd
                    | TokenType::BitOr
                    | TokenType::ShiftLeft
                    | TokenType::ShiftRight => {
                        expect_types!(left_type, ["i64", "u8"], op.loc);
                        expect_types!(self.typecheck_expr(env, right)?, ["i64", "u8"], op.loc);
                        Ok(left_type)
                    }
                    TokenType::DoubleEqual
                    | TokenType::NotEqual
                    | TokenType::Greater
                    | TokenType::GreaterEqual
                    | TokenType::Less
                    | TokenType::LessEqual => {
                        expect_types!(left_type, ["i64", "ptr", "u8"], op.loc);
                        expect_types!(
                            self.typecheck_expr(env, right)?,
                            ["i64", "ptr", "u8"],
                            op.loc
                        );
                        Ok("bool".into())
                    }
                    _ => unreachable!(),
                }
            }
            ExprKind::Logical { left, op, right } => {
                expect_types!(
                    self.typecheck_expr(env, left)?,
                    ["bool", "i64", "ptr"],
                    op.loc
                );
                expect_types!(
                    self.typecheck_expr(env, right)?,
                    ["bool", "i64", "ptr"],
                    op.loc
                );
                Ok("bool".into())
            }
            ExprKind::Grouping(expr) => self.typecheck_expr(env, expr),
            ExprKind::Literal(token) => match token.token_type {
                TokenType::IntLiteral => Ok("i64".into()),
                TokenType::FloatLiteral => Ok("f64".into()),
                TokenType::CharLiteral => Ok("u8".into()),
                TokenType::StringLiteral => Ok("str".into()),
                TokenType::KeywordTrue => Ok("bool".into()),
                TokenType::KeywordFalse => Ok("bool".into()),
                _ => unreachable!(),
            },
            ExprKind::Unary { op, right } => {
                let right_type = self.typecheck_expr(env, right)?;
                match op.token_type {
                    TokenType::Minus => {
                        expect_type!(right_type.clone(), "i64", op.loc);
                        Ok(right_type)
                    }
                    TokenType::Bang => {
                        expect_types!(right_type, ["bool", "i64", "ptr", "u8"], op.loc);
                        Ok("bool".into())
                    }
                    _ => unreachable!(),
                }
            }
            ExprKind::Variable(name) => {
                if self.symbol_table.constants.contains_key(&name.lexeme) {
                    Ok("i64".into())
                } else {
                    match env.get_var_type(&name.lexeme) {
                        Some(x) => Ok(x.clone()),
                        None => error!(name.loc, format!("undefined variable: {}", &name.lexeme)),
                    }
                }
            }
            ExprKind::Call {
                callee,
                paren,
                args,
            } => {
                if let ExprKind::Variable(callee_name) = &callee.kind {
                    if let Some(fn_type) = self.symbol_table.functions.get(&callee_name.lexeme) {
                        // its a function (defined/builtin/extern)
                        match &fn_type.params {
                            FnParams::Normal(params) => {
                                if params.len() != args.len() {
                                    return error!(
                                        &paren.loc,
                                        format!(
                                            "expected {} arguments, got {}",
                                            params.len(),
                                            args.len()
                                        )
                                    );
                                }
                                for (i, arg) in args.iter().enumerate() {
                                    expect_type!(
                                        self.typecheck_expr(env, arg)?,
                                        params[i],
                                        paren.loc
                                    );
                                }
                            }
                            FnParams::Variadic => {
                                // cant check arg types
                                for arg in args {
                                    let arg_type = self.typecheck_expr(env, arg)?;
                                    if arg_type == "f64" {
                                        return error!(
                                            &paren.loc,
                                            "f64 arguments not supported in variadic calls; cast to opaque to preserve bit pattern"
                                        );
                                    }
                                }
                            }
                        }
                        Ok(fn_type.return_type.clone())
                    } else {
                        // its a variable containing function address
                        expect_type!(self.typecheck_expr(env, callee)?, "ptr", paren.loc);

                        for arg in args {
                            self.typecheck_expr(env, arg)?;
                        }
                        Ok("opaque".into())
                    }
                } else {
                    // its an expression that evalutes to function address
                    expect_type!(self.typecheck_expr(env, callee)?, "ptr", paren.loc);

                    for arg in args {
                        self.typecheck_expr(env, arg)?;
                    }
                    Ok("opaque".into())
                }
            }
            ExprKind::ArrayLiteral(exprs) => {
                for expr in exprs {
                    self.typecheck_expr(env, expr)?;
                }
                if exprs.is_empty() {
                    Ok("Array".into())
                } else {
                    let first_item_type = self.typecheck_expr(env, &exprs[0])?;
                    Ok(format!("Array<{}>", first_item_type))
                }
            }
            ExprKind::Index {
                expr,
                bracket,
                index,
            } => {
                expect_types!(self.typecheck_expr(env, expr)?, ["ptr", "str"], bracket.loc);
                expect_types!(self.typecheck_expr(env, index)?, ["i64", "u8"], bracket.loc);
                Ok("u8".into())
            }
            ExprKind::AddrOf { op, expr } => match &expr.kind {
                ExprKind::Variable(_) => Ok("ptr".into()),
                _ => {
                    error!(&op.loc, "can only take address of variables and functions")
                }
            },
            ExprKind::New {
                struct_name,
                use_heap: _,
            } => {
                let (base_name, _) = parse_generic_type(&struct_name.lexeme);
                if !self.symbol_table.structs.contains_key(base_name) {
                    return error!(
                        &struct_name.loc,
                        format!("unknown struct name: {}", base_name)
                    );
                }
                Ok(struct_name.lexeme.clone())
            }
            ExprKind::MemberAccess { left, field } => {
                let left_type = self.typecheck_expr(env, left)?;
                let (base_name, generic_args) = parse_generic_type(&left_type);

                let fields = match self.symbol_table.structs.get(base_name) {
                    Some(f) => f,
                    None => {
                        return error!(&field.loc, format!("unknown struct type: {}", base_name));
                    }
                };

                let field_info = match fields.get(&field.lexeme) {
                    Some(o) => o,
                    None => return error!(&field.loc, format!("unknown field: {}", &field.lexeme)),
                };

                let field_type = if let Some(args) = generic_args {
                    substitute_type(&field_info.field_type, args[0])
                } else {
                    field_info.field_type.clone()
                };

                Ok(field_type)
            }
            ExprKind::Cast { expr, type_name } => {
                let expr_type = self.typecheck_expr(env, expr)?;
                if (expr_type != "opaque" && type_name.lexeme == "f64")
                    || (expr_type == "f64" && type_name.lexeme != "opaque")
                {
                    return error!(
                        &type_name.loc,
                        "direct casting between numeric types and f64 disallowed; use _builtin_cvtsi2sd and _builtin_cvttsd2si for actual conversion or cast to opaque first to preserve the bit pattern"
                    );
                }
                if !self.is_valid_type_name(&type_name.lexeme) {
                    return error!(
                        &type_name.loc,
                        format!("unknown type: {}", &type_name.lexeme)
                    );
                }
                Ok(type_name.lexeme.clone())
            }
            ExprKind::MethodCall { expr, method, args } => {
                let receiver_type = self.typecheck_expr(env, expr)?;
                let (base_name, generic_args) = parse_generic_type(&receiver_type);
                let func_name = format!("{}.{}", base_name, method.lexeme);

                let func_type = match self.symbol_table.functions.get(&func_name) {
                    Some(f) => f,
                    None => {
                        return error!(
                            method.loc,
                            format!(
                                "method {} not found on on type {}",
                                method.lexeme, base_name
                            )
                        );
                    }
                };

                let substitute = |s: &str| -> String {
                    if let Some(ref args) = generic_args {
                        substitute_type(s, args[0])
                    } else {
                        s.to_string()
                    }
                };

                match &func_type.params {
                    FnParams::Normal(params) => {
                        let substituted_params: Vec<String> =
                            params.iter().map(|p| substitute(p)).collect();

                        if substituted_params.is_empty() || substituted_params[0] != receiver_type {
                            return error!(
                                method.loc,
                                format!(
                                    "first parameter of the method must be of type {}",
                                    receiver_type
                                )
                            );
                        }
                        if substituted_params.len() != args.len() + 1 {
                            return error!(
                                method.loc,
                                format!(
                                    "expected {} arguments, got {}",
                                    substituted_params.len() - 1,
                                    args.len()
                                )
                            );
                        }
                        for (i, arg) in args.iter().enumerate() {
                            expect_type!(
                                self.typecheck_expr(env, arg)?,
                                substituted_params[i + 1].as_str(),
                                method.loc
                            );
                        }
                        Ok(substitute(&func_type.return_type))
                    }
                    FnParams::Variadic => {
                        for arg in args {
                            self.typecheck_expr(env, arg)?;
                        }
                        Ok(substitute(&func_type.return_type))
                    }
                }
            }
        }?;

        self.expr_types.insert(expr.id, expr_type.clone());
        Ok(expr_type)
    }

    fn is_valid_type_name(&self, name: &str) -> bool {
        if name == "$" {
            return true;
        }
        if name.contains(',') {
            return name.split(',').all(|part| self.is_valid_type_name(part));
        }
        if name.contains('<') {
            let (base, inner) = parse_generic_type(name);
            if !self.symbol_table.structs.contains_key(base) {
                return false;
            }
            if let Some(args) = inner {
                return args.iter().all(|arg| self.is_valid_type_name(arg));
            }
            unreachable!();
        }
        if BUILTIN_TYPES.contains(&name) {
            return true;
        }
        if self.symbol_table.structs.contains_key(name) {
            return true;
        }
        false
    }
}

fn parse_generic_type(s: &str) -> (&str, Option<Vec<&str>>) {
    if let Some(lt_pos) = s.find('<') {
        let base = &s[..lt_pos];
        let close_pos = s.rfind('>').unwrap_or(s.len());
        let inner = &s[lt_pos + 1..close_pos];
        let mut args = Vec::new();
        let mut depth = 0;
        let mut start = 0;
        for (i, ch) in inner.char_indices() {
            match ch {
                '<' => depth += 1,
                '>' => depth -= 1,
                ',' if depth == 0 => {
                    args.push(&inner[start..i]);
                    start = i + 1;
                }
                _ => {}
            }
        }
        args.push(&inner[start..]);
        (base, Some(args))
    } else {
        (s, None)
    }
}

fn substitute_type(type_str: &str, dollar_replacement: &str) -> String {
    if type_str.contains('$') {
        type_str.replace('$', dollar_replacement)
    } else {
        type_str.to_string()
    }
}
