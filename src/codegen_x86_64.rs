use std::{collections::HashMap, fmt::Write};

use crate::{
    parser::{Expr, Params, Stmt},
    symbol_table::SymbolTable,
    tokenizer::{Token, TokenType, ZernError, error},
};

struct Var {
    pub stack_offset: usize,
    pub var_type: String,
}

pub struct Env {
    scopes: Vec<HashMap<String, Var>>,
    next_offset: usize,
    loop_begin_label: String,
    loop_end_label: String,
    loop_continue_label: String,
}

impl Env {
    pub fn new() -> Env {
        Env {
            scopes: vec![HashMap::new()],
            next_offset: 8,
            loop_begin_label: String::new(),
            loop_end_label: String::new(),
            loop_continue_label: String::new(),
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
        self.next_offset += 8;
        self.scopes.last_mut().unwrap().insert(
            name,
            Var {
                stack_offset: offset,
                var_type,
            },
        );
        offset
    }

    fn get_var(&self, name: &str) -> Option<&Var> {
        for scope in self.scopes.iter().rev() {
            if let Some(var) = scope.get(name) {
                return Some(var);
            }
        }
        None
    }
}

macro_rules! emit {
    ($($arg:tt)*) => {
        let _ = writeln!($($arg)*);
    };
}

static REGISTERS: [&str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

pub struct CodegenX86_64<'a> {
    output: String,
    data_section: String,
    label_counter: usize,
    data_counter: usize,
    pub symbol_table: &'a SymbolTable,
}

impl<'a> CodegenX86_64<'a> {
    pub fn new(symbol_table: &'a SymbolTable) -> CodegenX86_64<'a> {
        CodegenX86_64 {
            output: String::new(),
            data_section: String::new(),
            label_counter: 0,
            data_counter: 1,
            symbol_table,
        }
    }

    fn label(&mut self) -> String {
        self.label_counter += 1;
        format!(".L{}", self.label_counter)
    }

    pub fn get_output(&self) -> String {
        format!("section .data\n{}{}", self.data_section, self.output)
    }

    pub fn emit_prologue(&mut self, use_gcc: bool) -> Result<(), ZernError> {
        emit!(
            &mut self.output,
            "section .note.GNU-stack
    db 0

section .bss
    _heap_head: resq 1
    _heap_tail: resq 1
    _environ: resq 1
    _err_code: resq 1
    _err_msg: resq 1

section .text._builtin_heap_head
_builtin_heap_head:
    lea rax, [rel _heap_head]
    ret

section .text._builtin_heap_tail
_builtin_heap_tail:
    lea rax, [rel _heap_tail]
    ret

section .text._builtin_err_code
_builtin_err_code:
    lea rax, [rel _err_code]
    ret

section .text._builtin_err_msg
_builtin_err_msg:
    lea rax, [rel _err_msg]
    ret

section .text._builtin_read64
_builtin_read64:
    mov rax, QWORD [rdi]
    ret

section .text._builtin_set64
_builtin_set64:
    mov [rdi], rsi
    ret

section .text._builtin_syscall
_builtin_syscall:
    mov rax, rdi
    mov rdi, rsi
    mov rsi, rdx
    mov rdx, rcx
    mov r10, r8
    mov r8, r9
    mov r9, [rsp+8]
    syscall
    ret
"
        );

        if !use_gcc {
            emit!(
                &mut self.output,
                "
section .text._builtin_environ
_builtin_environ:
    lea rax, [rel _environ]
    mov rax, [rax]
    ret

global _start
section .text
_start:
    xor rbp, rbp
    ; setup args
    pop rdi
    mov rsi, rsp
    ; save environ
    lea rdx, [rsi + rdi*8 + 8]
    lea rax, [rel _environ]
    mov [rax], rdx
    ; align stack
    and rsp, -16
    ; main()
    call main
    ; exit
    mov rdi, rax
    mov rax, 60
    syscall
"
            );
        } else {
            emit!(
                &mut self.output,
                "
section .text._builtin_environ
_builtin_environ:
    extern environ
    mov rax, [rel environ]
    ret
"
            );
        }
        Ok(())
    }

    pub fn compile_stmt(&mut self, env: &mut Env, stmt: &Stmt) -> Result<(), ZernError> {
        match stmt {
            Stmt::Expression(expr) => self.compile_expr(env, expr)?,
            Stmt::Let {
                name,
                var_type,
                initializer,
            } => {
                // TODO: move to typechecker?
                if env.get_var(&name.lexeme).is_some() {
                    return error!(
                        name.loc,
                        format!("variable already defined: {}", &name.lexeme)
                    );
                }

                let var_type: String = match var_type {
                    Some(t) => t.lexeme.clone(),
                    None => match &initializer {
                        Expr::Literal(token) => {
                            if token.token_type == TokenType::Number {
                                "i64".into()
                            } else {
                                return error!(&name.loc, "unable to infer variable type");
                            }
                        }
                        _ => return error!(&name.loc, "unable to infer variable type"),
                    },
                };

                self.compile_expr(env, initializer)?;
                let offset = env.define_var(name.lexeme.clone(), var_type);
                emit!(&mut self.output, "    mov QWORD [rbp-{}], rax", offset);
            }
            Stmt::Const { name: _, value: _ } => {
                // handled in SymbolTable
            }
            Stmt::Block(statements) => {
                env.push_scope();
                for stmt in statements {
                    self.compile_stmt(env, stmt)?;
                }
                env.pop_scope();
            }
            Stmt::If {
                keyword: _,
                condition,
                then_branch,
                else_branch,
            } => {
                let else_label = self.label();
                let end_label = self.label();

                self.compile_expr(env, condition)?;
                emit!(&mut self.output, "    test rax, rax");
                emit!(&mut self.output, "    je {}", else_label);
                self.compile_stmt(env, then_branch)?;
                emit!(&mut self.output, "    jmp {}", end_label);
                emit!(&mut self.output, "{}:", else_label);
                self.compile_stmt(env, else_branch)?;
                emit!(&mut self.output, "{}:", end_label);
            }
            Stmt::While {
                keyword: _,
                condition,
                body,
            } => {
                let old_loop_begin_label = env.loop_begin_label.clone();
                let old_loop_end_label = env.loop_end_label.clone();
                let old_loop_continue_label = env.loop_continue_label.clone();
                env.loop_begin_label = self.label();
                env.loop_end_label = self.label();
                env.loop_continue_label = env.loop_begin_label.clone();

                emit!(&mut self.output, "{}:", env.loop_begin_label);
                self.compile_expr(env, condition)?;
                emit!(&mut self.output, "    test rax, rax");
                emit!(&mut self.output, "    je {}", env.loop_end_label);
                self.compile_stmt(env, body)?;
                emit!(&mut self.output, "    jmp {}", env.loop_begin_label);
                emit!(&mut self.output, "{}:", env.loop_end_label);

                env.loop_begin_label = old_loop_begin_label;
                env.loop_end_label = old_loop_end_label;
                env.loop_continue_label = old_loop_continue_label;
            }
            Stmt::Function {
                name,
                params,
                return_type: _,
                body,
                exported,
            } => {
                if *exported || name.lexeme == "main" {
                    emit!(&mut self.output, "global {}", name.lexeme);
                }
                emit!(&mut self.output, "section .text.{}", name.lexeme);
                emit!(&mut self.output, "{}:", name.lexeme);
                emit!(&mut self.output, "    push rbp");
                emit!(&mut self.output, "    mov rbp, rsp");
                emit!(&mut self.output, "    sub rsp, 256"); // TODO: eww

                match params {
                    Params::Normal(params) => {
                        for (i, param) in params.iter().enumerate() {
                            let offset = env.define_var(
                                param.var_name.lexeme.clone(),
                                param.var_type.lexeme.clone(),
                            );
                            if let Some(reg) = REGISTERS.get(i) {
                                emit!(&mut self.output, "    mov QWORD [rbp-{}], {}", offset, reg);
                            } else {
                                let stack_offset = 16 + 8 * (i - REGISTERS.len());
                                emit!(
                                    &mut self.output,
                                    "    mov rax, QWORD [rbp+{}]",
                                    stack_offset
                                );
                                emit!(&mut self.output, "    mov QWORD [rbp-{}], rax", offset);
                            }
                        }
                    }
                    Params::Variadic => {
                        emit!(&mut self.output, "    sub rsp, 48");
                        emit!(&mut self.output, "    mov [rbp - 8], rdi");
                        emit!(&mut self.output, "    mov [rbp - 16], rsi");
                        emit!(&mut self.output, "    mov [rbp - 24], rdx");
                        emit!(&mut self.output, "    mov [rbp - 32], rcx");
                        emit!(&mut self.output, "    mov [rbp - 40], r8");
                        emit!(&mut self.output, "    mov [rbp - 48], r9");
                        env.next_offset += 48;
                    }
                }

                self.compile_stmt(env, body)?;

                // fallback to null
                // very hacky but works
                if !self.output.trim_end().ends_with("    ret") {
                    emit!(&mut self.output, "    mov rax, 0");
                    emit!(&mut self.output, "    mov rsp, rbp");
                    emit!(&mut self.output, "    pop rbp");
                    emit!(&mut self.output, "    ret");
                }
            }
            Stmt::Return { expr, keyword: _ } => {
                self.compile_expr(env, expr)?;
                emit!(&mut self.output, "    mov rsp, rbp");
                emit!(&mut self.output, "    pop rbp");
                emit!(&mut self.output, "    ret");
            }
            Stmt::For {
                var,
                start,
                end,
                body,
            } => {
                let old_loop_begin_label = env.loop_begin_label.clone();
                let old_loop_end_label = env.loop_end_label.clone();
                let old_loop_continue_label = env.loop_continue_label.clone();
                env.loop_begin_label = self.label();
                env.loop_end_label = self.label();
                env.loop_continue_label = self.label();

                env.push_scope();
                let offset = env.define_var(var.lexeme.clone(), "i64".into());

                self.compile_expr(env, start)?;
                emit!(&mut self.output, "    mov QWORD [rbp-{}], rax", offset);
                emit!(&mut self.output, "{}:", env.loop_begin_label);
                emit!(&mut self.output, "    mov rax, QWORD [rbp-{}]", offset);
                emit!(&mut self.output, "    push rax");
                self.compile_expr(env, end)?;
                emit!(&mut self.output, "    pop rcx");
                emit!(&mut self.output, "    cmp rcx, rax");
                emit!(&mut self.output, "    jge {}", env.loop_end_label);
                self.compile_stmt(env, body)?;
                emit!(&mut self.output, "{}:", env.loop_continue_label);
                emit!(&mut self.output, "    mov rax, QWORD [rbp-{}]", offset);
                emit!(&mut self.output, "    add rax, 1");
                emit!(&mut self.output, "    mov QWORD [rbp-{}], rax", offset);
                emit!(&mut self.output, "    jmp {}", env.loop_begin_label);
                emit!(&mut self.output, "{}:", env.loop_end_label);
                env.pop_scope();

                env.loop_begin_label = old_loop_begin_label;
                env.loop_end_label = old_loop_end_label;
                env.loop_continue_label = old_loop_continue_label;
            }
            Stmt::Break => {
                emit!(&mut self.output, "    jmp {}", env.loop_end_label);
            }
            Stmt::Continue => {
                emit!(&mut self.output, "    jmp {}", env.loop_continue_label);
            }
            Stmt::Extern(name) => {
                emit!(&mut self.output, "extern {}", name.lexeme);
            }
            Stmt::Struct { name: _, fields: _ } => {
                // handled in SymbolTable
            }
        }
        Ok(())
    }

    pub fn compile_expr(&mut self, env: &mut Env, expr: &Expr) -> Result<(), ZernError> {
        match expr {
            Expr::Binary { left, op, right } => {
                self.compile_expr(env, left)?;
                emit!(&mut self.output, "    push rax");
                self.compile_expr(env, right)?;
                emit!(&mut self.output, "    mov rbx, rax");
                emit!(&mut self.output, "    pop rax");

                match op.token_type {
                    TokenType::Plus => {
                        emit!(&mut self.output, "    add rax, rbx");
                    }
                    TokenType::Minus => {
                        emit!(&mut self.output, "    sub rax, rbx");
                    }
                    TokenType::Star => {
                        emit!(&mut self.output, "    imul rax, rbx");
                    }
                    TokenType::Slash => {
                        emit!(&mut self.output, "    cqo");
                        emit!(&mut self.output, "    idiv rbx");
                    }
                    TokenType::Mod => {
                        emit!(&mut self.output, "    cqo");
                        emit!(&mut self.output, "    idiv rbx");
                        emit!(&mut self.output, "    mov rax, rdx");
                    }
                    TokenType::Xor => {
                        emit!(&mut self.output, "    xor rax, rbx");
                    }
                    TokenType::BitAnd => {
                        emit!(&mut self.output, "    and rax, rbx");
                    }
                    TokenType::BitOr => {
                        emit!(&mut self.output, "    or rax, rbx");
                    }
                    TokenType::DoubleEqual => {
                        emit!(&mut self.output, "    cmp rax, rbx");
                        emit!(&mut self.output, "    sete al");
                        emit!(&mut self.output, "    movzx rax, al");
                    }
                    TokenType::NotEqual => {
                        emit!(&mut self.output, "    cmp rax, rbx");
                        emit!(&mut self.output, "    setne al");
                        emit!(&mut self.output, "    movzx rax, al");
                    }
                    TokenType::Greater => {
                        emit!(&mut self.output, "    cmp rax, rbx");
                        emit!(&mut self.output, "    setg al");
                        emit!(&mut self.output, "    movzx rax, al");
                    }
                    TokenType::GreaterEqual => {
                        emit!(&mut self.output, "    cmp rax, rbx");
                        emit!(&mut self.output, "    setge al");
                        emit!(&mut self.output, "    movzx rax, al");
                    }
                    TokenType::Less => {
                        emit!(&mut self.output, "    cmp rax, rbx");
                        emit!(&mut self.output, "    setl al");
                        emit!(&mut self.output, "    movzx rax, al");
                    }
                    TokenType::LessEqual => {
                        emit!(&mut self.output, "    cmp rax, rbx");
                        emit!(&mut self.output, "    setle al");
                        emit!(&mut self.output, "    movzx rax, al");
                    }
                    TokenType::ShiftLeft => {
                        emit!(&mut self.output, "    mov rcx, rbx");
                        emit!(&mut self.output, "    sal rax, cl");
                    }
                    TokenType::ShiftRight => {
                        emit!(&mut self.output, "    mov rcx, rbx");
                        emit!(&mut self.output, "    sar rax, cl");
                    }
                    _ => unreachable!(),
                }
            }
            Expr::Logical { left, op, right } => {
                let end_label = self.label();
                match op.token_type {
                    TokenType::LogicalAnd => {
                        self.compile_expr(env, left)?;
                        emit!(&mut self.output, "    test rax, rax");
                        emit!(&mut self.output, "    je {}", end_label);
                        self.compile_expr(env, right)?;
                    }
                    TokenType::LogicalOr => {
                        self.compile_expr(env, left)?;
                        emit!(&mut self.output, "    test rax, rax");
                        emit!(&mut self.output, "    jne {}", end_label);
                        self.compile_expr(env, right)?;
                    }
                    _ => unreachable!(),
                }
                emit!(&mut self.output, "{}:", end_label);
            }
            Expr::Grouping(expr) => self.compile_expr(env, expr)?,
            Expr::Literal(token) => match token.token_type {
                TokenType::Number => {
                    emit!(&mut self.output, "    mov rax, {}", token.lexeme);
                }
                TokenType::Char => {
                    emit!(
                        &mut self.output,
                        "    mov rax, {}",
                        token.lexeme.chars().nth(1).unwrap() as u8
                    );
                }
                TokenType::String => {
                    // TODO: actual string parsing in the tokenizer
                    let value = &token.lexeme[1..token.lexeme.len() - 1];

                    let label = format!("str_{:03}", self.data_counter);

                    if value.is_empty() {
                        emit!(&mut self.data_section, "    {} db 0", label);
                    } else {
                        let charcodes = value
                            .chars()
                            .map(|x| (x as u8).to_string())
                            .collect::<Vec<String>>()
                            .join(",");
                        emit!(&mut self.data_section, "    {} db {},0", label, charcodes);
                    }
                    emit!(&mut self.output, "    mov rax, {}", label);
                    self.data_counter += 1;
                }
                TokenType::True => {
                    emit!(&mut self.output, "    mov rax, 1");
                }
                TokenType::False => {
                    emit!(&mut self.output, "    mov rax, 0");
                }
                _ => unreachable!(),
            },
            Expr::Unary { op, right } => {
                self.compile_expr(env, right)?;
                match op.token_type {
                    TokenType::Minus => {
                        emit!(&mut self.output, "    neg rax");
                    }
                    TokenType::Bang => {
                        emit!(&mut self.output, "    test rax, rax");
                        emit!(&mut self.output, "    sete al");
                        emit!(&mut self.output, "    movzx rax, al");
                    }
                    _ => unreachable!(),
                }
            }
            Expr::Variable(name) => {
                if self.symbol_table.constants.contains_key(&name.lexeme) {
                    emit!(
                        &mut self.output,
                        "    mov rax, {}",
                        self.symbol_table.constants[&name.lexeme]
                    );
                } else {
                    let var = match env.get_var(&name.lexeme) {
                        Some(x) => x,
                        None => unreachable!("this should be caught in the typechecker"),
                    };
                    emit!(
                        &mut self.output,
                        "    mov rax, QWORD [rbp-{}]",
                        var.stack_offset,
                    );
                }
            }
            Expr::Assign { left, op, value } => {
                self.compile_expr(env, value)?;

                match left.as_ref() {
                    Expr::Variable(name) => {
                        let var = match env.get_var(&name.lexeme) {
                            Some(x) => x,
                            None => unreachable!(),
                        };
                        emit!(
                            &mut self.output,
                            "    mov QWORD [rbp-{}], rax",
                            var.stack_offset,
                        );
                    }
                    Expr::Index {
                        expr,
                        bracket: _,
                        index,
                    } => {
                        emit!(&mut self.output, "    push rax");
                        self.compile_expr(env, expr)?;
                        emit!(&mut self.output, "    push rax");
                        self.compile_expr(env, index)?;
                        emit!(&mut self.output, "    pop rbx");
                        emit!(&mut self.output, "    add rbx, rax");
                        emit!(&mut self.output, "    pop rax");
                        emit!(&mut self.output, "    mov BYTE [rbx], al");
                    }
                    Expr::MemberAccess { left, field } => {
                        emit!(&mut self.output, "    push rax");

                        let offset = self.get_field_offset(env, left, field)?;

                        self.compile_expr(env, left)?;
                        emit!(&mut self.output, "    pop rbx");
                        emit!(&mut self.output, "    mov QWORD [rax+{}], rbx", offset);
                    }
                    _ => return error!(&op.loc, "invalid assignment target"),
                };
            }
            Expr::Call {
                callee,
                paren: _,
                args,
            } => {
                if let Expr::Variable(callee_name) = &**callee
                    && callee_name.lexeme == "_var_arg"
                {
                    return self.emit_var_arg(env, &args[0]);
                }

                for arg in args {
                    self.compile_expr(env, arg)?;
                    emit!(&mut self.output, "    push rax");
                }

                let arg_count = args.len();
                if arg_count <= 6 {
                    for i in (0..arg_count).rev() {
                        emit!(&mut self.output, "    pop {}", REGISTERS[i]);
                    }
                } else {
                    for (i, reg) in REGISTERS.iter().enumerate() {
                        let offset = 8 * (arg_count - 1 - i);
                        emit!(
                            &mut self.output,
                            "    mov {}, QWORD [rsp + {}]",
                            reg,
                            offset
                        );
                    }
                    // TODO: since all zern values are 64bit large we currently cannot call
                    // external functions that expect a non-64bit value past the 6th argument
                    let num_stack = arg_count - 6;
                    for i in 0..num_stack {
                        let arg_idx = arg_count - 1 - i;
                        let offset = 8 * (arg_count - 1 - arg_idx);
                        emit!(
                            &mut self.output,
                            "    mov rax, QWORD [rsp + {}]",
                            offset + 8 * i
                        );
                        emit!(&mut self.output, "    push rax");
                    }
                }

                if let Expr::Variable(callee_name) = &**callee {
                    if self
                        .symbol_table
                        .functions
                        .contains_key(&callee_name.lexeme)
                    {
                        // its a function (defined/builtin/extern)
                        emit!(&mut self.output, "    call {}", callee_name.lexeme);
                    } else {
                        // its a variable containing function address
                        self.compile_expr(env, callee)?;
                        emit!(&mut self.output, "    call rax");
                    }
                } else {
                    // its an expression that evalutes to function address
                    self.compile_expr(env, callee)?;
                    emit!(&mut self.output, "    call rax");
                }

                if arg_count > 6 {
                    let num_stack = arg_count - 6;
                    emit!(&mut self.output, "    add rsp, {}", 8 * num_stack);
                    emit!(&mut self.output, "    add rsp, {}", 8 * arg_count);
                }
            }
            Expr::ArrayLiteral(exprs) => {
                emit!(&mut self.output, "    call array.new");
                emit!(&mut self.output, "    push rax");

                for expr in exprs {
                    self.compile_expr(env, expr)?;
                    emit!(&mut self.output, "    mov rsi, rax");
                    emit!(&mut self.output, "    pop rdi");
                    emit!(&mut self.output, "    push rdi");
                    emit!(&mut self.output, "    call array.push");
                }
                emit!(&mut self.output, "    pop rax");
            }
            Expr::Index {
                expr,
                bracket: _,
                index,
            } => {
                self.compile_expr(env, expr)?;
                emit!(&mut self.output, "    push rax");
                self.compile_expr(env, index)?;
                emit!(&mut self.output, "    pop rbx");
                emit!(&mut self.output, "    add rax, rbx");
                emit!(&mut self.output, "    movzx rax, BYTE [rax]");
            }
            Expr::AddrOf { op, expr } => match *expr.clone() {
                Expr::Variable(name) => {
                    if self.symbol_table.functions.contains_key(&name.lexeme) {
                        emit!(&mut self.output, "    mov rax, {}", name.lexeme);
                    } else {
                        let var = match env.get_var(&name.lexeme) {
                            Some(x) => x,
                            None => {
                                return error!(
                                    name.loc,
                                    format!("undefined variable: {}", &name.lexeme)
                                );
                            }
                        };
                        emit!(
                            &mut self.output,
                            "    lea rax, QWORD [rbp-{}]",
                            var.stack_offset,
                        );
                    }
                }
                _ => {
                    return error!(&op.loc, "can only take address of variables and functions");
                }
            },
            Expr::New(struct_name) => {
                let struct_fields = &self.symbol_table.structs[&struct_name.lexeme];

                let memory_size = struct_fields.len() * 8;
                emit!(&mut self.output, "    mov rdi, {}", memory_size);
                emit!(&mut self.output, "    call mem.alloc");
                emit!(&mut self.output, "    push rax");
                emit!(&mut self.output, "    mov rdi, rax");
                emit!(&mut self.output, "    mov rsi, {}", memory_size);
                emit!(&mut self.output, "    call mem.zero");
                emit!(&mut self.output, "    pop rax");
            }
            Expr::MemberAccess { left, field } => {
                let offset = self.get_field_offset(env, left, field)?;
                self.compile_expr(env, left)?;
                emit!(&mut self.output, "    mov rax, QWORD [rax+{}]", offset);
            }
            Expr::Cast { expr, type_name: _ } => {
                self.compile_expr(env, expr)?;
            }
        }
        Ok(())
    }

    fn get_field_offset(
        &self,
        env: &mut Env,
        left: &Expr,
        field: &Token,
    ) -> Result<usize, ZernError> {
        let struct_name = match left {
            Expr::Variable(name) => match env.get_var(&name.lexeme) {
                Some(v) => v.var_type.clone(),
                None => {
                    return error!(name.loc, format!("undefined variable: {}", &name.lexeme));
                }
            },
            _ => {
                return error!(
                    &field.loc,
                    "cannot determine struct type for member assignment"
                );
            }
        };

        let fields = match self.symbol_table.structs.get(&struct_name) {
            Some(f) => f,
            None => {
                return error!(&field.loc, format!("unknown struct type: {}", struct_name));
            }
        };

        let field = match fields.get(&field.lexeme) {
            Some(o) => o,
            None => return error!(&field.loc, format!("unknown field: {}", &field.lexeme)),
        };

        Ok(field.offset)
    }

    fn emit_var_arg(&mut self, env: &mut Env, index_expr: &Expr) -> Result<(), ZernError> {
        self.compile_expr(env, index_expr)?;
        emit!(&mut self.output, " mov r10, rax");

        let stack_label = self.label();
        let done_label = self.label();

        emit!(&mut self.output, " cmp r10, 6");
        emit!(&mut self.output, " jge {}", stack_label);

        // < 6
        emit!(&mut self.output, " mov rax, r10");
        emit!(&mut self.output, " inc rax");
        emit!(&mut self.output, " shl rax, 3");
        emit!(&mut self.output, " neg rax");
        emit!(&mut self.output, " mov rax, [rbp + rax]");
        emit!(&mut self.output, " jmp {}", done_label);

        // >= 6
        emit!(&mut self.output, "{}:", stack_label);
        emit!(&mut self.output, " mov rax, r10");
        emit!(&mut self.output, " sub rax, 6");
        emit!(&mut self.output, " shl rax, 3");
        emit!(&mut self.output, " mov rax, [rbp + 16 + rax]");

        emit!(&mut self.output, "{}:", done_label);
        Ok(())
    }
}
