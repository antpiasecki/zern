use std::{collections::HashMap, fmt::Write};

use crate::{
    Args,
    parser::{Expr, ExprKind, Params, Stmt},
    symbol_table::SymbolTable,
    tokenizer::{Token, TokenType, ZernError, error},
};

// defers have 1 in 18 quintillion chance to fail
const DEFER_MAGIC: u64 = 0xae64f8d23c556e8c;

struct Var {
    pub stack_offset: usize,
    #[allow(unused)]
    pub var_type: String,
}

pub struct Env {
    scopes: Vec<HashMap<String, Var>>,
    next_offset: usize,
    are_we_returning_f64: bool,
    defers: Vec<(usize, Stmt)>,
    loop_begin_label: String,
    loop_end_label: String,
    loop_continue_label: String,
}

impl Env {
    pub fn new() -> Env {
        Env {
            scopes: vec![HashMap::new()],
            next_offset: 16,
            are_we_returning_f64: false,
            defers: Vec::new(),
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

pub struct CodegenX86_64<'a> {
    output: String,
    rodata: String,
    bss: String,
    label_counter: usize,
    rodata_counter: usize,
    pub args: &'a Args,
    pub symbol_table: &'a SymbolTable,
    pub expr_types: &'a HashMap<usize, String>,
}

impl<'a> CodegenX86_64<'a> {
    pub fn new(
        args: &'a Args,
        symbol_table: &'a SymbolTable,
        expr_types: &'a HashMap<usize, String>,
    ) -> CodegenX86_64<'a> {
        CodegenX86_64 {
            output: String::new(),
            rodata: String::new(),
            bss: String::new(),
            label_counter: 1,
            rodata_counter: 1,
            args,
            symbol_table,
            expr_types,
        }
    }

    fn label(&mut self) -> String {
        self.label_counter += 1;
        format!(".L{}", self.label_counter)
    }

    pub fn get_output(&self) -> String {
        format!(
            ".section .rodata
{}
.section .bss
.align 8
{}
{}",
            self.rodata, self.bss, self.output
        )
    }

    pub fn emit_prologue(&mut self) -> Result<(), ZernError> {
        if !self.args.target_windows {
            emit!(
                &mut self.output,
                ".section .note.GNU-stack
    .byte 0
"
            );
        }

        emit!(
            &mut self.output,
            ".intel_syntax noprefix

.section .text._builtin_f64_to_f32
_builtin_f64_to_f32:
    cvtsd2ss xmm0, xmm0
    movd eax, xmm0
    ret

.section .text._builtin_f32_to_f64
_builtin_f32_to_f64:
    cvtss2sd xmm0, xmm0
    ret
"
        );

        if self.args.target_windows {
            emit!(
                &mut self.output,
                ".section .text._builtin_read64
_builtin_read64:
    mov rax, QWORD PTR [rcx]
    ret

.section .text._builtin_write64
_builtin_write64:
    mov [rcx], rdx
    ret
"
            );
        } else {
            emit!(
                &mut self.output,
                ".section .text._builtin_read64
_builtin_read64:
    mov rax, QWORD PTR [rdi]
    ret

.section .text._builtin_write64
_builtin_write64:
    mov [rdi], rsi
    ret

.section .text._builtin_syscall
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
        }

        if !self.args.use_crt {
            // Linux without CRT
            emit!(
                &mut self.output,
                "
.globl _start
.section .text
_start:
    xor rbp, rbp
    // setup args
    pop rdi
    mov rsi, rsp
    // save environ
    lea rdx, [rsi + rdi*8 + 8]
    lea rax, [rip + _builtin_environ]
    mov [rax], rdx
    // align stack
    and rsp, -16
    // exit(main())
    call main
    mov rdi, rax
    mov rax, 60
    syscall
"
            );
            emit!(&mut self.bss, "    _builtin_environ: .zero 8");
        } else if !self.args.target_windows {
            // Linux with CRT
            emit!(
                &mut self.output,
                "
.extern environ
.set _builtin_environ, environ
"
            );
        }
        Ok(())
    }

    pub fn compile_stmt(&mut self, env: &mut Env, stmt: &Stmt) -> Result<(), ZernError> {
        match stmt {
            Stmt::Expression(expr) => self.compile_expr(env, expr)?,
            Stmt::Declare { name, initializer } => {
                let var_type: String = match self.expr_types[&initializer.id].as_str() {
                    "opaque" => match &initializer.kind {
                        ExprKind::Cast { .. } => "opaque".into(),
                        _ => return error!(name.loc, "cannot infer type from opaque"),
                    },
                    t => t.into(),
                };

                self.compile_expr(env, initializer)?;
                let offset = env.define_var(name.lexeme.clone(), var_type);
                emit!(&mut self.output, "    mov QWORD PTR [rbp-{}], rax", offset);
            }
            Stmt::Assign { left, op, value } => {
                self.compile_expr(env, value)?;

                match &left.kind {
                    ExprKind::Variable(name) => {
                        if let Some(var) = env.get_var(&name.lexeme) {
                            emit!(&mut self.output, "    mov QWORD PTR [rbp-{}], rax", var.stack_offset);
                        } else if self.symbol_table.globals.contains_key(&name.lexeme) {
                            emit!(
                                &mut self.output,
                                "    mov [{}+rip], rax",
                                self.symbol_table.globals[&name.lexeme],
                            );
                        } else {
                            unreachable!();
                        }
                    }
                    ExprKind::Index {
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
                        emit!(&mut self.output, "    mov BYTE PTR [rbx], al");
                    }
                    ExprKind::MemberAccess { left, field } => {
                        emit!(&mut self.output, "    push rax");

                        let offset = self.get_field_offset(left, field)?;

                        self.compile_expr(env, left)?;
                        emit!(&mut self.output, "    pop rbx");
                        emit!(&mut self.output, "    mov QWORD PTR [rax+{}], rbx", offset);
                    }
                    _ => return error!(&op.loc, "invalid assignment target"),
                };
            }
            Stmt::Destructure { targets, op, value } => {
                self.compile_expr(env, value)?;

                for (i, target) in targets.iter().enumerate() {
                    let reg = match i {
                        0 => "rax",
                        1 => "rdx",
                        _ => {
                            return error!(&op.loc, "destructuring more than 2 values not implemented yet");
                        }
                    };

                    let offset = match env.get_var(&target.lexeme) {
                        Some(var) => var.stack_offset,
                        None => {
                            let types: Vec<&str> = self.expr_types[&value.id].split(',').collect();
                            env.define_var(target.lexeme.clone(), types[i].to_string())
                        }
                    };
                    emit!(&mut self.output, "    mov QWORD PTR [rbp-{}], {}", offset, reg);
                }
            }
            Stmt::Const { .. } => {
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
                return_types,
                body,
                exported,
            } => {
                if return_types.len() == 1 && return_types[0].lexeme == "f64" {
                    env.are_we_returning_f64 = true;
                }

                let name = &name.lexeme;
                if *exported || name == "main" {
                    emit!(&mut self.output, ".globl {0}", name);
                }
                if !self.args.target_windows {
                    emit!(&mut self.output, ".type {0}, @function", name);
                }
                emit!(&mut self.output, ".section .text.{}", name);
                emit!(&mut self.output, "{}:", name);
                emit!(&mut self.output, "    push rbp");
                emit!(&mut self.output, "    mov rbp, rsp");
                emit!(&mut self.output, "    push rbx");

                let prologue_offset = self.output.len();
                emit!(&mut self.output, "    sub rsp, {:<10}", 0);

                match params {
                    Params::Normal(params) => {
                        let max_reg = if self.args.target_windows { 4 } else { 6 };
                        let stack_base = if self.args.target_windows { 48 } else { 16 };
                        let mut int_reg = 0;
                        let mut fp_reg = 0;
                        let mut stack_count = 0;
                        for param in params {
                            let offset = env.define_var(param.var_name.lexeme.clone(), param.var_type.lexeme.clone());
                            if param.var_type.lexeme == "f64" {
                                if fp_reg < max_reg {
                                    emit!(&mut self.output, "    movq QWORD PTR [rbp-{}], xmm{}", offset, fp_reg);
                                } else {
                                    emit!(
                                        &mut self.output,
                                        "    mov rax, QWORD PTR [rbp+{}]",
                                        stack_base + 8 * stack_count
                                    );
                                    emit!(&mut self.output, "    mov QWORD PTR [rbp-{}], rax", offset);
                                    stack_count += 1;
                                }
                                fp_reg += 1;
                            } else {
                                if int_reg < max_reg {
                                    let registers = self.registers();
                                    emit!(
                                        &mut self.output,
                                        "    mov QWORD PTR [rbp-{}], {}",
                                        offset,
                                        registers[int_reg]
                                    );
                                } else {
                                    emit!(
                                        &mut self.output,
                                        "    mov rax, QWORD PTR [rbp+{}]",
                                        stack_base + 8 * stack_count
                                    );
                                    emit!(&mut self.output, "    mov QWORD PTR [rbp-{}], rax", offset);
                                    stack_count += 1;
                                }
                                int_reg += 1;
                            }
                        }
                    }
                    Params::Variadic => {
                        if self.args.target_windows {
                            emit!(&mut self.output, "    sub rsp, 32");
                            emit!(&mut self.output, "    mov [rbp - 16], rcx");
                            emit!(&mut self.output, "    mov [rbp - 24], rdx");
                            emit!(&mut self.output, "    mov [rbp - 32], r8");
                            emit!(&mut self.output, "    mov [rbp - 40], r9");
                            env.next_offset += 32;
                        } else {
                            emit!(&mut self.output, "    sub rsp, 48");
                            emit!(&mut self.output, "    mov [rbp - 16], rdi");
                            emit!(&mut self.output, "    mov [rbp - 24], rsi");
                            emit!(&mut self.output, "    mov [rbp - 32], rdx");
                            emit!(&mut self.output, "    mov [rbp - 40], rcx");
                            emit!(&mut self.output, "    mov [rbp - 48], r8");
                            emit!(&mut self.output, "    mov [rbp - 56], r9");
                            env.next_offset += 48;
                        }
                    }
                }

                match &**body {
                    Stmt::Block(stmts) => {
                        for stmt in stmts {
                            self.compile_stmt(env, stmt)?;
                        }
                    }
                    _ => self.compile_stmt(env, body)?,
                }

                // fallback to null
                // very hacky but works
                if !self.output.trim_end().ends_with("    ret") {
                    self.emit_defers(env)?;
                    emit!(&mut self.output, "    mov rax, 0");
                    emit!(&mut self.output, "    mov rsp, rbp");
                    emit!(&mut self.output, "    sub rsp, 8");
                    emit!(&mut self.output, "    pop rbx");
                    emit!(&mut self.output, "    pop rbp");
                    emit!(&mut self.output, "    ret");
                }

                if !self.args.target_windows {
                    emit!(&mut self.output, ".size {0}, . - {0}", name);
                }

                // patch the stack size after we know how much we actually need
                let patch = format!("    sub rsp, {:<10}", ((env.next_offset + 15) & !15) - 8);
                self.output
                    .replace_range(prologue_offset..prologue_offset + patch.len(), &patch);
            }
            Stmt::Return { keyword: _, exprs } => {
                self.emit_defers(env)?;
                match exprs.len() {
                    2 => {
                        self.compile_expr(env, &exprs[1])?;
                        emit!(&mut self.output, "    push rax");
                        self.compile_expr(env, &exprs[0])?;
                        emit!(&mut self.output, "    pop rdx");
                    }
                    1 => {
                        self.compile_expr(env, &exprs[0])?;
                    }
                    0 => {}
                    _ => unreachable!(), // guaranteed by typechecker
                }
                if env.are_we_returning_f64 {
                    emit!(&mut self.output, "    movq xmm0, rax");
                }
                emit!(&mut self.output, "    mov rsp, rbp");
                emit!(&mut self.output, "    sub rsp, 8");
                emit!(&mut self.output, "    pop rbx");
                emit!(&mut self.output, "    pop rbp");
                emit!(&mut self.output, "    ret");
            }
            Stmt::For { var, start, end, body } => {
                let old_loop_begin_label = env.loop_begin_label.clone();
                let old_loop_end_label = env.loop_end_label.clone();
                let old_loop_continue_label = env.loop_continue_label.clone();
                env.loop_begin_label = self.label();
                env.loop_end_label = self.label();
                env.loop_continue_label = self.label();

                env.push_scope();
                let offset = env.define_var(var.lexeme.clone(), "i64".into());

                self.compile_expr(env, start)?;
                emit!(&mut self.output, "    mov QWORD PTR [rbp-{}], rax", offset);
                self.compile_expr(env, end)?;
                let end_offset = env.next_offset;
                env.next_offset += 8;
                emit!(&mut self.output, "    mov QWORD PTR [rbp-{}], rax", end_offset);
                emit!(&mut self.output, "{}:", env.loop_begin_label);
                emit!(&mut self.output, "    mov rax, QWORD PTR [rbp-{}]", offset);
                emit!(&mut self.output, "    mov rcx, QWORD PTR [rbp-{}]", end_offset);
                emit!(&mut self.output, "    cmp rax, rcx");
                emit!(&mut self.output, "    jge {}", env.loop_end_label);
                self.compile_stmt(env, body)?;
                emit!(&mut self.output, "{}:", env.loop_continue_label);
                emit!(&mut self.output, "    mov rax, QWORD PTR [rbp-{}]", offset);
                emit!(&mut self.output, "    add rax, 1");
                emit!(&mut self.output, "    mov QWORD PTR [rbp-{}], rax", offset);
                emit!(&mut self.output, "    jmp {}", env.loop_begin_label);
                emit!(&mut self.output, "{}:", env.loop_end_label);
                env.pop_scope();

                env.loop_begin_label = old_loop_begin_label;
                env.loop_end_label = old_loop_end_label;
                env.loop_continue_label = old_loop_continue_label;
            }
            Stmt::Break(keyword) => {
                if env.loop_end_label == "" {
                    return error!(keyword.loc, "break not allowed outside loops");
                }
                emit!(&mut self.output, "    jmp {}", env.loop_end_label);
            }
            Stmt::Continue(keyword) => {
                if env.loop_continue_label == "" {
                    return error!(keyword.loc, "continue not allowed outside loops");
                }
                emit!(&mut self.output, "    jmp {}", env.loop_continue_label);
            }
            Stmt::Extern { name, .. } => {
                emit!(&mut self.output, ".extern {}", name.lexeme);
            }
            Stmt::Struct { .. } => {
                // handled in SymbolTable
            }
            Stmt::GlobalVariable(name) => {
                emit!(
                    &mut self.bss,
                    "    {}: .skip 8",
                    self.symbol_table.globals[&name.lexeme]
                );
            }
            Stmt::Defer { keyword, block } => {
                if env.loop_begin_label != "" {
                    return error!(keyword.loc, "defers in loops not implemented yet");
                }
                let offset = env.define_var(format!("_defer_{}", env.defers.len()), "bool".into());
                env.defers.push((offset, *block.clone()));
                emit!(&mut self.output, "    movabs rax, {}", DEFER_MAGIC);
                emit!(&mut self.output, "    mov QWORD PTR [rbp-{}], rax", offset);
            }
        }
        Ok(())
    }

    pub fn compile_expr(&mut self, env: &mut Env, expr: &Expr) -> Result<(), ZernError> {
        match &expr.kind {
            ExprKind::Binary { left, op, right } => {
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
            ExprKind::Logical { left, op, right } => {
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
            ExprKind::Grouping(expr) => self.compile_expr(env, expr)?,
            ExprKind::Literal(token) => match token.token_type {
                TokenType::IntLiteral => {
                    emit!(&mut self.output, "    mov rax, {}", token.lexeme);
                }
                TokenType::FloatLiteral => {
                    let value: f64 = token.lexeme.parse().unwrap();
                    emit!(&mut self.output, "    mov rax, {}", value.to_bits());
                }
                TokenType::CharLiteral => {
                    emit!(
                        &mut self.output,
                        "    mov rax, {}",
                        token.lexeme.chars().nth(1).unwrap() as u8
                    );
                }
                TokenType::StringLiteral => {
                    let value = &token.lexeme[1..token.lexeme.len() - 1];

                    let label = format!("str_{:03}", self.rodata_counter);

                    let charcodes = value
                        .chars()
                        .map(|x| (x as u8).to_string())
                        .chain(std::iter::once("0".into()))
                        .collect::<Vec<_>>()
                        .join(",");
                    emit!(&mut self.rodata, "    {}: .byte {}", label, charcodes);
                    self.rodata_counter += 1;

                    emit!(&mut self.output, "    lea rax, [rip + {}]", label);
                }
                TokenType::KeywordTrue => {
                    emit!(&mut self.output, "    mov rax, 1");
                }
                TokenType::KeywordFalse => {
                    emit!(&mut self.output, "    mov rax, 0");
                }
                _ => unreachable!(),
            },
            ExprKind::Unary { op, right } => {
                self.compile_expr(env, right)?;
                match op.token_type {
                    TokenType::Minus => {
                        if self.expr_types[&expr.id] == "f64" {
                            emit!(&mut self.output, "    movq xmm0, rax");
                            emit!(&mut self.output, "    xorpd xmm1, xmm1");
                            emit!(&mut self.output, "    subsd xmm1, xmm0");
                            emit!(&mut self.output, "    movq rax, xmm1");
                        } else {
                            emit!(&mut self.output, "    neg rax");
                        }
                    }
                    TokenType::Bang => {
                        emit!(&mut self.output, "    test rax, rax");
                        emit!(&mut self.output, "    sete al");
                        emit!(&mut self.output, "    movzx rax, al");
                    }
                    _ => unreachable!(),
                }
            }
            ExprKind::Variable(name) => {
                if self.symbol_table.constants.contains_key(&name.lexeme) {
                    emit!(
                        &mut self.output,
                        "    mov rax, {}",
                        self.symbol_table.constants[&name.lexeme]
                    );
                } else {
                    if let Some(var) = env.get_var(&name.lexeme) {
                        emit!(&mut self.output, "    mov rax, QWORD PTR [rbp-{}]", var.stack_offset);
                    } else if self.symbol_table.globals.contains_key(&name.lexeme) {
                        emit!(
                            &mut self.output,
                            "    mov rax, [{}+rip]",
                            self.symbol_table.globals[&name.lexeme],
                        );
                    } else {
                        unreachable!();
                    }
                }
            }
            ExprKind::Call { callee, paren: _, args } => {
                if let ExprKind::Variable(callee_name) = &callee.kind
                    && callee_name.lexeme == "_var_arg"
                {
                    return self.emit_var_arg(env, &args[0]);
                }

                if let ExprKind::Variable(callee_name) = &callee.kind
                    && callee_name.lexeme == "_stackalloc"
                {
                    self.compile_expr(env, &args[0])?;
                    emit!(&mut self.output, "    add rax, 15");
                    emit!(&mut self.output, "    and rax, -16");
                    emit!(&mut self.output, "    sub rsp, rax");
                    emit!(&mut self.output, "    mov rax, rsp");
                    return Ok(());
                }

                if self.args.target_windows {
                    if args.len() % 2 == 1 {
                        emit!(&mut self.output, "    sub rsp, 8");
                    }
                    for arg in args.iter().rev() {
                        self.compile_expr(env, arg)?;
                        emit!(&mut self.output, "    push rax");
                    }
                } else {
                    for arg in args {
                        self.compile_expr(env, arg)?;
                        emit!(&mut self.output, "    push rax");
                    }
                }

                let arg_types: Vec<String> = args.iter().map(|a| self.expr_types[&a.id].clone()).collect();
                self.emit_call_setup(&arg_types);

                if let ExprKind::Variable(callee_name) = &callee.kind {
                    if self.symbol_table.functions.contains_key(&callee_name.lexeme) {
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

                self.emit_call_cleanup(args.len());
                if self.expr_types[&expr.id] == "f64" {
                    emit!(&mut self.output, "    movq rax, xmm0");
                }
            }
            ExprKind::ArrayLiteral(exprs) => {
                if self.args.target_windows {
                    emit!(&mut self.output, "    mov rcx, 24");
                    emit!(&mut self.output, "    .extern malloc");
                    emit!(&mut self.output, "    call malloc");
                } else {
                    emit!(&mut self.output, "    mov rdi, 24");
                    emit!(&mut self.output, "    call mem.alloc");
                }
                emit!(&mut self.output, "    push rax");
                emit!(&mut self.output, "    mov rdi, rax");
                emit!(&mut self.output, "    mov rsi, 24");
                emit!(&mut self.output, "    call mem.zero");
                emit!(&mut self.output, "    pop rax");
                emit!(&mut self.output, "    push rax");

                for expr in exprs {
                    self.compile_expr(env, expr)?;
                    emit!(&mut self.output, "    mov rsi, rax");
                    emit!(&mut self.output, "    pop rdi");
                    emit!(&mut self.output, "    push rdi");
                    emit!(&mut self.output, "    call Array.push");
                }
                emit!(&mut self.output, "    pop rax");
            }
            ExprKind::Index {
                expr,
                bracket: _,
                index,
            } => {
                self.compile_expr(env, expr)?;
                emit!(&mut self.output, "    push rax");
                self.compile_expr(env, index)?;
                emit!(&mut self.output, "    pop rbx");
                emit!(&mut self.output, "    add rax, rbx");
                emit!(&mut self.output, "    movzx rax, BYTE PTR [rax]");
            }
            ExprKind::AddrOf { op, expr } => match &expr.kind {
                ExprKind::Variable(name) => {
                    if self.symbol_table.functions.contains_key(&name.lexeme) {
                        emit!(&mut self.output, "    lea rax, [rip + {}]", name.lexeme);
                    } else if self.symbol_table.globals.contains_key(&name.lexeme) {
                        emit!(
                            &mut self.output,
                            "    lea rax, [rip + {}]",
                            self.symbol_table.globals[&name.lexeme]
                        );
                    } else if let Some(var) = env.get_var(&name.lexeme) {
                        emit!(&mut self.output, "    lea rax, QWORD PTR [rbp-{}]", var.stack_offset);
                    } else {
                        return error!(name.loc, format!("undefined variable: {}", &name.lexeme));
                    }
                }
                _ => {
                    return error!(&op.loc, "can only take address of variables and functions");
                }
            },
            ExprKind::New { struct_name, use_heap } => {
                let struct_fields = &self.symbol_table.structs[self.strip_generic(&struct_name.lexeme)];
                let memory_size = struct_fields.len() * 8;

                if *use_heap {
                    if self.args.target_windows {
                        emit!(&mut self.output, "    mov rcx, {}", memory_size);
                        emit!(&mut self.output, "    .extern malloc");
                        emit!(&mut self.output, "    call malloc");
                    } else {
                        emit!(&mut self.output, "    mov rdi, {}", memory_size);
                        emit!(&mut self.output, "    call mem.alloc");
                    }
                } else {
                    let aligned_size = (memory_size + 15) & !15;
                    emit!(&mut self.output, "    sub rsp, {}", aligned_size);
                    emit!(&mut self.output, "    mov rax, rsp");
                }
                emit!(&mut self.output, "    push rax");
                emit!(&mut self.output, "    sub rsp, 8");
                emit!(&mut self.output, "    mov rdi, rax");
                emit!(&mut self.output, "    mov rsi, {}", memory_size);
                emit!(&mut self.output, "    call mem.zero");
                emit!(&mut self.output, "    add rsp, 8");
                emit!(&mut self.output, "    pop rax");
            }
            ExprKind::MemberAccess { left, field } => {
                let offset = self.get_field_offset(left, field)?;
                self.compile_expr(env, left)?;
                emit!(&mut self.output, "    mov rax, QWORD PTR [rax+{}]", offset);
            }
            ExprKind::Cast { expr, type_name } => {
                self.compile_expr(env, expr)?;
                match (self.expr_types[&expr.id].as_str(), type_name.lexeme.as_str()) {
                    ("i64", "f64") => {
                        emit!(&mut self.output, "    cvtsi2sd xmm0, rax");
                        emit!(&mut self.output, "    movq rax, xmm0");
                    }
                    ("f64", "i64") => {
                        emit!(&mut self.output, "    movq xmm0, rax");
                        emit!(&mut self.output, "    cvttsd2si rax, xmm0");
                    }
                    ("f64", _) => return error!(type_name.loc, "f64 can be only casted to i64"),
                    (_, "f64") => return error!(type_name.loc, "only i64 can be casted to f64"),
                    _ => {}
                }
            }
            ExprKind::MethodCall { expr, method, args } => {
                let receiver_type = &self.expr_types[&expr.id];
                let base_type = self.strip_generic(receiver_type);
                let func_name = format!("{}.{}", base_type, method.lexeme);

                if self.args.target_windows {
                    if (1 + args.len()) % 2 == 1 {
                        emit!(&mut self.output, "    sub rsp, 8");
                    }
                    for arg in args.iter().rev() {
                        self.compile_expr(env, arg)?;
                        emit!(&mut self.output, "    push rax");
                    }
                    self.compile_expr(env, expr)?;
                    emit!(&mut self.output, "    push rax");
                } else {
                    self.compile_expr(env, expr)?;
                    emit!(&mut self.output, "    push rax");
                    for arg in args {
                        self.compile_expr(env, arg)?;
                        emit!(&mut self.output, "    push rax");
                    }
                }

                let mut arg_types = vec![];
                arg_types.push(receiver_type.clone());
                arg_types.extend(args.iter().map(|a| self.expr_types[&a.id].clone()));

                self.emit_call_setup(&arg_types);
                emit!(&mut self.output, "    call {}", func_name);
                self.emit_call_cleanup(1 + args.len());

                if self.expr_types[&expr.id] == "f64" {
                    emit!(&mut self.output, "    movq rax, xmm0");
                }
            }
        }
        Ok(())
    }

    fn emit_call_setup(&mut self, arg_types: &[String]) {
        let arg_count = arg_types.len();

        let registers = self.registers();
        let mut fp_idx = 0;
        let mut int_idx = 0;

        if self.args.target_windows {
            let to_register = arg_count.min(4);

            emit!(&mut self.output, "    sub rsp, 32");
            for (i, arg_type) in arg_types.iter().enumerate().take(to_register) {
                emit!(&mut self.output, "    mov rax, QWORD PTR [rsp + {}]", 32 + 8 * i);
                if arg_type == "f64" {
                    emit!(&mut self.output, "    movq xmm{}, rax", fp_idx);
                    fp_idx += 1;
                } else {
                    emit!(&mut self.output, "    mov {}, rax", registers[int_idx]);
                    int_idx += 1;
                }
            }

            emit!(&mut self.output, "    mov al, {}", fp_idx);
            emit!(&mut self.output, "    add rsp, 32");
            return;
        }

        let to_register = arg_count.min(6);
        for (i, arg_type) in arg_types.iter().enumerate().take(to_register) {
            let offset = 8 * (arg_count - 1 - i);
            emit!(&mut self.output, "    mov rax, QWORD PTR [rsp + {}]", offset);
            if arg_type == "f64" {
                emit!(&mut self.output, "    movq xmm{}, rax", fp_idx);
                fp_idx += 1;
            } else {
                emit!(&mut self.output, "    mov {}, rax", registers[int_idx]);
                int_idx += 1;
            }
        }

        // TODO: since all zern values are 64bit large we currently cannot call
        // external functions that expect a non-64bit value past the 6th argument
        let num_stack = arg_count.saturating_sub(6);
        for i in 0..num_stack {
            let arg_idx = arg_count - 1 - i;
            let offset = 8 * (arg_count - 1 - arg_idx);
            emit!(&mut self.output, "    mov rax, QWORD PTR [rsp + {}]", offset + 8 * i);
            emit!(&mut self.output, "    push rax");
        }

        emit!(&mut self.output, "    mov al, {}", fp_idx);

        if num_stack == 0 {
            emit!(&mut self.output, "    add rsp, {}", 8 * to_register);
        }
    }

    fn emit_call_cleanup(&mut self, arg_count: usize) {
        if self.args.target_windows {
            let pad = if arg_count % 2 == 1 { 8 } else { 0 };
            emit!(&mut self.output, "    add rsp, {}", 8 * arg_count + pad);
            return;
        }

        let num_stack = arg_count.saturating_sub(6);
        if num_stack > 0 {
            emit!(&mut self.output, "    add rsp, {}", 8 * (arg_count + num_stack));
        }
    }

    fn emit_defers(&mut self, env: &mut Env) -> Result<(), ZernError> {
        for (offset, stmt) in env.defers.clone().iter().rev() {
            let skip_label = self.label();
            emit!(&mut self.output, "    mov rax, QWORD PTR [rbp-{}]", offset);
            emit!(&mut self.output, "    movabs rbx, {}", DEFER_MAGIC);
            emit!(&mut self.output, "    cmp rax, rbx");
            emit!(&mut self.output, "    jne {}", skip_label);
            self.compile_stmt(env, stmt)?;
            emit!(&mut self.output, "    mov QWORD PTR [rbp-{}], 0", offset);
            emit!(&mut self.output, "{}:", skip_label);
        }
        Ok(())
    }

    fn strip_generic<'b>(&self, type_name: &'b str) -> &'b str {
        type_name.split('<').next().unwrap_or(type_name)
    }

    fn get_field_offset(&self, left: &Expr, field: &Token) -> Result<usize, ZernError> {
        let struct_name = self.strip_generic(&self.expr_types[&left.id]);

        let Some(fields) = self.symbol_table.structs.get(struct_name) else {
            return error!(&field.loc, format!("unknown struct type: {}", struct_name));
        };

        let Some(field) = fields.get(&field.lexeme) else {
            return error!(&field.loc, format!("unknown field: {}", &field.lexeme));
        };

        Ok(field.offset)
    }

    fn emit_var_arg(&mut self, env: &mut Env, index_expr: &Expr) -> Result<(), ZernError> {
        self.compile_expr(env, index_expr)?;
        emit!(&mut self.output, "    mov r10, rax");

        let stack_label = self.label();
        let done_label = self.label();
        let register_count = if self.args.target_windows { 4 } else { 6 };
        let stack_base = if self.args.target_windows { 48 } else { 16 };

        emit!(&mut self.output, "    cmp r10, {}", register_count);
        emit!(&mut self.output, "    jge {}", stack_label);

        // register-backed variadic arguments
        emit!(&mut self.output, "    mov rax, r10");
        emit!(&mut self.output, "    inc rax");
        emit!(&mut self.output, "    shl rax, 3");
        emit!(&mut self.output, "    neg rax");
        emit!(&mut self.output, "    sub rax, 8");
        emit!(&mut self.output, "    mov rax, [rbp + rax]");
        emit!(&mut self.output, "    jmp {}", done_label);

        // stack-backed variadic arguments
        emit!(&mut self.output, "{}:", stack_label);
        emit!(&mut self.output, "    mov rax, r10");
        emit!(&mut self.output, "    sub rax, {}", register_count);
        emit!(&mut self.output, "    shl rax, 3");
        emit!(&mut self.output, "    mov rax, [rbp + rax + {}]", stack_base);

        emit!(&mut self.output, "{}:", done_label);
        Ok(())
    }

    fn registers(&self) -> &'static [&'static str] {
        if self.args.target_windows {
            &["rcx", "rdx", "r8", "r9"]
        } else {
            &["rdi", "rsi", "rdx", "rcx", "r8", "r9"]
        }
    }
}
