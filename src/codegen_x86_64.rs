use std::{collections::HashMap, fmt::Write};

use crate::{
    parser::{Expr, ExprKind, Params, Stmt},
    symbol_table::SymbolTable,
    tokenizer::{Token, TokenType, ZernError, error},
};

struct Var {
    pub stack_offset: usize,
    #[allow(unused)]
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
    pub expr_types: &'a HashMap<usize, String>,
    emit_debug: bool,
}

impl<'a> CodegenX86_64<'a> {
    pub fn new(
        symbol_table: &'a SymbolTable,
        expr_types: &'a HashMap<usize, String>,
        emit_debug: bool,
    ) -> CodegenX86_64<'a> {
        CodegenX86_64 {
            output: String::new(),
            data_section: String::new(),
            label_counter: 0,
            data_counter: 1,
            symbol_table,
            expr_types,
            emit_debug,
        }
    }

    fn label(&mut self) -> String {
        self.label_counter += 1;
        format!(".L{}", self.label_counter)
    }

    pub fn get_output(&self) -> String {
        format!(".section .data\n{}{}", self.data_section, self.output)
    }

    pub fn emit_prologue(&mut self, use_crt: bool) -> Result<(), ZernError> {
        emit!(
            &mut self.output,
            ".intel_syntax noprefix

.section .note.GNU-stack
    .byte 0

.section .bss
    _heap_head: .zero 8
    _heap_tail: .zero 8
    _environ: .zero 8

.section .text._builtin_heap_head
_builtin_heap_head:
    lea rax, [rip + _heap_head]
    ret

.section .text._builtin_heap_tail
_builtin_heap_tail:
    lea rax, [rip + _heap_tail]
    ret

.section .text._builtin_read64
_builtin_read64:
    mov rax, QWORD PTR [rdi]
    ret

.section .text._builtin_set64
_builtin_set64:
    mov [rdi], rsi
    ret

.section .text._builtin_cvtsi2sd
_builtin_cvtsi2sd:
    cvtsi2sd xmm0, rdi
    movq rax, xmm0
    ret

.section .text._builtin_cvttsd2si
_builtin_cvttsd2si:
    cvttsd2si rax, xmm0
    ret

.section .text._builtin_f64_to_float
_builtin_f64_to_float:
    cvtsd2ss xmm0, xmm0
    movd eax, xmm0
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

        if !use_crt {
            emit!(
                &mut self.output,
                "
.section .text._builtin_environ
_builtin_environ:
    lea rax, [rip + _environ]
    mov rax, [rax]
    ret

.globl _start
.section .text
_start:
    xor rbp, rbp
    // setup args
    pop rdi
    mov rsi, rsp
    // save environ
    lea rdx, [rsi + rdi*8 + 8]
    lea rax, [rip + _environ]
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
        } else {
            emit!(
                &mut self.output,
                "
.section .text._builtin_environ
_builtin_environ:
    .extern environ
    mov rax, [rip + environ]
    ret
"
            );
        }
        Ok(())
    }

    pub fn compile_stmt(&mut self, env: &mut Env, stmt: &Stmt) -> Result<(), ZernError> {
        match stmt {
            Stmt::Expression(expr) => self.compile_expr(env, expr)?,
            Stmt::Declare {
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
                    None => match self.expr_types[&initializer.id].as_str() {
                        "any" => return error!(name.loc, "cannot infer type from any"),
                        t => t.into(),
                    },
                };

                self.compile_expr(env, initializer)?;
                let offset = env.define_var(name.lexeme.clone(), var_type);
                emit!(&mut self.output, "    mov QWORD PTR [rbp-{}], rax", offset);
            }
            Stmt::Assign { left, op, value } => {
                self.compile_expr(env, value)?;

                match &left.kind {
                    ExprKind::Variable(name) => {
                        // already ensured by the typechecker
                        let var = env.get_var(&name.lexeme).unwrap();
                        emit!(
                            &mut self.output,
                            "    mov QWORD PTR [rbp-{}], rax",
                            var.stack_offset,
                        );
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
                            return error!(
                                &op.loc,
                                "destructuring more than 2 values not implemented yet"
                            );
                        }
                    };

                    let offset = match env.get_var(&target.lexeme) {
                        Some(var) => var.stack_offset,
                        None => {
                            let types: Vec<&str> = self.expr_types[&value.id].split(',').collect();
                            env.define_var(target.lexeme.clone(), types[i].to_string())
                        }
                    };
                    emit!(
                        &mut self.output,
                        "    mov QWORD PTR [rbp-{}], {}",
                        offset,
                        reg
                    );
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
                return_types: _,
                body,
                exported,
            } => {
                let name = &name.lexeme;
                if self.emit_debug || *exported || name == "main" {
                    emit!(&mut self.output, ".globl {0}", name);
                    emit!(&mut self.output, ".type {0}, @function", name);
                }
                emit!(&mut self.output, ".section .text.{}", name);
                emit!(&mut self.output, "{}:", name);
                emit!(&mut self.output, "    push rbp");
                emit!(&mut self.output, "    mov rbp, rsp");

                let prologue_offset = self.output.len();
                emit!(&mut self.output, "    sub rsp, {:<10}", 0);

                match params {
                    Params::Normal(params) => {
                        for (i, param) in params.iter().enumerate() {
                            let offset = env.define_var(
                                param.var_name.lexeme.clone(),
                                param.var_type.lexeme.clone(),
                            );
                            if let Some(reg) = REGISTERS.get(i) {
                                emit!(
                                    &mut self.output,
                                    "    mov QWORD PTR [rbp-{}], {}",
                                    offset,
                                    reg
                                );
                            } else {
                                let stack_offset = 16 + 8 * (i - REGISTERS.len());
                                emit!(
                                    &mut self.output,
                                    "    mov rax, QWORD PTR [rbp+{}]",
                                    stack_offset
                                );
                                emit!(&mut self.output, "    mov QWORD PTR [rbp-{}], rax", offset);
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

                emit!(&mut self.output, ".size {0}, . - {0}", name);

                // patch the stack size after we know how much we actually need
                let patch = format!("    sub rsp, {:<10}", (env.next_offset + 15) & !15);
                self.output
                    .replace_range(prologue_offset..prologue_offset + patch.len(), &patch);
            }
            Stmt::Return { keyword: _, exprs } => {
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
                    _ => todo!(),
                }
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
                emit!(&mut self.output, "    mov QWORD PTR [rbp-{}], rax", offset);
                self.compile_expr(env, end)?;
                let end_offset = env.next_offset;
                env.next_offset += 8;
                emit!(
                    &mut self.output,
                    "    mov QWORD PTR [rbp-{}], rax",
                    end_offset
                );
                emit!(&mut self.output, "{}:", env.loop_begin_label);
                emit!(&mut self.output, "    mov rax, QWORD PTR [rbp-{}]", offset);
                emit!(
                    &mut self.output,
                    "    mov rcx, QWORD PTR [rbp-{}]",
                    end_offset
                );
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
            Stmt::Break => {
                emit!(&mut self.output, "    jmp {}", env.loop_end_label);
            }
            Stmt::Continue => {
                emit!(&mut self.output, "    jmp {}", env.loop_continue_label);
            }
            Stmt::Extern(name) => {
                emit!(&mut self.output, ".extern {}", name.lexeme);
            }
            Stmt::Struct { .. } => {
                // handled in SymbolTable
            }
            Stmt::Enum { .. } => {
                // handled in SymbolTable
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
                    // TODO: actual string parsing in the tokenizer
                    let value = &token.lexeme[1..token.lexeme.len() - 1];

                    let label = format!("str_{:03}", self.data_counter);

                    let charcodes = value
                        .chars()
                        .map(|x| (x as u8).to_string())
                        .chain(std::iter::once("0".into()))
                        .collect::<Vec<_>>()
                        .join(",");
                    emit!(&mut self.data_section, "    {}: .byte {}", label, charcodes);
                    self.data_counter += 1;

                    emit!(&mut self.output, "    lea rax, [rip + {}]", label);
                }
                TokenType::True => {
                    emit!(&mut self.output, "    mov rax, 1");
                }
                TokenType::False => {
                    emit!(&mut self.output, "    mov rax, 0");
                }
                _ => unreachable!(),
            },
            ExprKind::Unary { op, right } => {
                self.compile_expr(env, right)?;
                match op.token_type {
                    TokenType::Minus => {
                        if self.expr_types[&right.id] == "f64" {
                            emit!(&mut self.output, "    mov rbx, 0x8000000000000000");
                            emit!(&mut self.output, "    xor rax, rbx");
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
                    // already ensured by the typechecker
                    let var = env.get_var(&name.lexeme).unwrap();
                    emit!(
                        &mut self.output,
                        "    mov rax, QWORD PTR [rbp-{}]",
                        var.stack_offset,
                    );
                }
            }
            ExprKind::Call {
                callee,
                paren: _,
                args,
            } => {
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

                for arg in args {
                    self.compile_expr(env, arg)?;
                    emit!(&mut self.output, "    push rax");
                }

                let arg_types: Vec<String> = args
                    .iter()
                    .map(|a| self.expr_types[&a.id].clone())
                    .collect();
                self.emit_call_setup(&arg_types);

                if let ExprKind::Variable(callee_name) = &callee.kind {
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

                self.emit_call_cleanup(args.len());
            }
            ExprKind::ArrayLiteral(exprs) => {
                emit!(&mut self.output, "    mov rdi, 24");
                emit!(&mut self.output, "    call mem.alloc");
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
                            "    lea rax, QWORD PTR [rbp-{}]",
                            var.stack_offset,
                        );
                    }
                }
                _ => {
                    return error!(&op.loc, "can only take address of variables and functions");
                }
            },
            ExprKind::New {
                struct_name,
                use_heap,
            } => {
                let struct_fields = &self.symbol_table.structs[&struct_name.lexeme];
                let memory_size = struct_fields.len() * 8;

                if *use_heap {
                    emit!(&mut self.output, "    mov rdi, {}", memory_size);
                    emit!(&mut self.output, "    call mem.alloc");
                    emit!(&mut self.output, "    push rax");
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
            ExprKind::Cast { expr, type_name: _ } => {
                self.compile_expr(env, expr)?;
            }
            ExprKind::MethodCall { expr, method, args } => {
                let receiver_type = &self.expr_types[&expr.id];
                let func_name = format!("{}.{}", receiver_type, method.lexeme);

                self.compile_expr(env, expr)?;
                emit!(&mut self.output, "    push rax");
                for arg in args {
                    self.compile_expr(env, arg)?;
                    emit!(&mut self.output, "    push rax");
                }

                let mut arg_types = vec![];
                arg_types.push(receiver_type.clone());
                arg_types.extend(args.iter().map(|a| self.expr_types[&a.id].clone()));

                self.emit_call_setup(&arg_types);
                emit!(&mut self.output, "    call {}", func_name);
                self.emit_call_cleanup(1 + args.len());
            }
        }
        Ok(())
    }

    fn emit_call_setup(&mut self, arg_types: &[String]) {
        let arg_count = arg_types.len();

        let to_register = arg_count.min(6);
        let mut fp_idx = 0;
        let mut int_idx = 0;
        for (i, arg_type) in arg_types.iter().enumerate().take(to_register) {
            let offset = 8 * (arg_count - 1 - i);
            emit!(
                &mut self.output,
                "    mov rax, QWORD PTR [rsp + {}]",
                offset
            );
            if arg_type == "f64" {
                emit!(&mut self.output, "    movq xmm{}, rax", fp_idx);
                fp_idx += 1;
            } else {
                emit!(&mut self.output, "    mov {}, rax", REGISTERS[int_idx]);
                int_idx += 1;
            }
        }

        // TODO: since all zern values are 64bit large we currently cannot call
        // external functions that expect a non-64bit value past the 6th argument
        let num_stack = arg_count.saturating_sub(6);
        for i in 0..num_stack {
            let arg_idx = arg_count - 1 - i;
            let offset = 8 * (arg_count - 1 - arg_idx);
            emit!(
                &mut self.output,
                "    mov rax, QWORD PTR [rsp + {}]",
                offset + 8 * i
            );
            emit!(&mut self.output, "    push rax");
        }

        emit!(&mut self.output, "    mov al, {}", fp_idx);

        if num_stack == 0 {
            emit!(&mut self.output, "    add rsp, {}", 8 * to_register);
        }
    }

    fn emit_call_cleanup(&mut self, arg_count: usize) {
        let num_stack = arg_count.saturating_sub(6);
        if num_stack > 0 {
            emit!(
                &mut self.output,
                "    add rsp, {}",
                8 * (arg_count + num_stack)
            );
        }
    }

    fn get_field_offset(&self, left: &Expr, field: &Token) -> Result<usize, ZernError> {
        let struct_name = &self.expr_types[&left.id];

        let fields = match self.symbol_table.structs.get(struct_name) {
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
        emit!(&mut self.output, "    mov r10, rax");

        let stack_label = self.label();
        let done_label = self.label();

        emit!(&mut self.output, "    cmp r10, 6");
        emit!(&mut self.output, "    jge {}", stack_label);

        // < 6
        emit!(&mut self.output, "    mov rax, r10");
        emit!(&mut self.output, "    inc rax");
        emit!(&mut self.output, "    shl rax, 3");
        emit!(&mut self.output, "    neg rax");
        emit!(&mut self.output, "    mov rax, [rbp + rax]");
        emit!(&mut self.output, "    jmp {}", done_label);

        // >= 6
        emit!(&mut self.output, "{}:", stack_label);
        emit!(&mut self.output, "    mov rax, r10");
        emit!(&mut self.output, "    sub rax, 6");
        emit!(&mut self.output, "    shl rax, 3");
        emit!(&mut self.output, "    mov rax, [rbp + 16 + rax]");

        emit!(&mut self.output, "{}:", done_label);
        Ok(())
    }
}
