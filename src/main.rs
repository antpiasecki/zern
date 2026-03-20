mod codegen_x86_64;
mod parser;
mod symbol_table;
mod tokenizer;
mod typechecker;

use std::{
    fs,
    path::Path,
    process::{self, Command},
};

use tokenizer::ZernError;

macro_rules! parse_std_file {
    ($statements:expr, $filename:expr) => {
        let source: String = include_str!($filename).into();
        let tokenizer = tokenizer::Tokenizer::new($filename.to_owned(), source);
        let parser = parser::Parser::new(tokenizer.tokenize()?);
        $statements.extend(parser.parse()?);
    };
}

fn compile_file(args: Args) -> Result<(), ZernError> {
    let source = match fs::read_to_string(&args.path) {
        Ok(x) => x,
        Err(_) => {
            eprintln!("\x1b[91mERROR\x1b[0m: failed to open {}", args.path);
            process::exit(1);
        }
    };

    let filename = Path::new(&args.path).file_name().unwrap().to_str().unwrap();

    let mut statements = Vec::new();

    parse_std_file!(statements, "std/syscalls.zr");
    parse_std_file!(statements, "std/std.zr");
    parse_std_file!(statements, "std/net.zr");

    let tokenizer = tokenizer::Tokenizer::new(filename.to_owned(), source);
    let parser = parser::Parser::new(tokenizer.tokenize()?);
    statements.extend(parser.parse()?);

    let mut symbol_table = symbol_table::SymbolTable::new();
    for stmt in &statements {
        symbol_table.register_declaration(stmt)?;
    }

    let mut typechecker = typechecker::TypeChecker::new(&symbol_table);
    for stmt in &statements {
        typechecker.typecheck_stmt(&mut typechecker::Env::new(), stmt)?;
    }

    let mut codegen = codegen_x86_64::CodegenX86_64::new(&symbol_table);
    codegen.emit_prologue(args.use_gcc)?;
    for stmt in statements {
        codegen.compile_stmt(&mut codegen_x86_64::Env::new(), &stmt)?;
    }

    if !args.output_asm {
        let out = args.out.unwrap_or_else(|| "out".into());

        fs::write(format!("{}.s", out), codegen.get_output()).unwrap();

        run_command(format!("nasm -f elf64 -o {}.o {}.s", out, out));

        if args.use_gcc {
            run_command(format!(
                "gcc -no-pie -o {} {}.o -flto -Wl,--gc-sections {}",
                out, out, args.cflags
            ));
        } else {
            run_command(format!(
                "ld -static -o {} {}.o --gc-sections -e _start",
                out, out
            ));
        }

        if args.run_exe {
            run_command(
                std::fs::canonicalize(out)
                    .unwrap()
                    .to_string_lossy()
                    .into_owned(),
            );
        }
    } else {
        fs::write(
            args.out.unwrap_or_else(|| "out.s".into()),
            codegen.get_output(),
        )
        .unwrap();
    }

    Ok(())
}

fn run_command(cmd: String) {
    if !Command::new("sh")
        .args(["-c", &cmd])
        .status()
        .unwrap()
        .success()
    {
        process::exit(1);
    }
}

struct Args {
    path: String,
    out: Option<String>,
    output_asm: bool,
    run_exe: bool,
    use_gcc: bool,
    cflags: String,
}

impl Args {
    fn parse(mut args: std::env::Args) -> Args {
        let mut out = Args {
            path: String::new(),
            out: None,
            output_asm: false,
            run_exe: false,
            use_gcc: false,
            cflags: String::new(),
        };

        while let Some(arg) = args.next() {
            if arg == "-o" {
                match args.next() {
                    Some(s) => out.out = Some(s),
                    None => {
                        eprintln!("\x1b[91mERROR\x1b[0m: -o option requires a name");
                        process::exit(1);
                    }
                }
            } else if arg == "-S" {
                out.output_asm = true;
            } else if arg == "-r" {
                out.run_exe = true;
            } else if arg == "-m" {
                out.use_gcc = true;
            } else if arg == "-C" {
                match args.next() {
                    Some(s) => out.cflags = s,
                    None => {
                        eprintln!("\x1b[91mERROR\x1b[0m: -C option requires a name");
                        process::exit(1);
                    }
                }
            } else if arg == "-h" || arg == "--help" {
                println!("Usage: zern [-o path] [-S] [-r] [-m] [-C cflags] path");
                process::exit(0);
            } else if arg.starts_with('-') {
                eprintln!("\x1b[91mERROR\x1b[0m: unrecognized option: {}", arg);
                process::exit(1);
            } else if out.path.is_empty() {
                out.path = arg
            } else {
                eprintln!("\x1b[91mERROR\x1b[0m: unrecognized argument: {}", arg);
                process::exit(1);
            }
        }

        if out.path.is_empty() {
            eprintln!("\x1b[91mERROR\x1b[0m: you must provide a path");
            process::exit(1);
        }

        if !out.use_gcc && !out.cflags.is_empty() {
            // no "ERROR:" since its not an error
            eprintln!("You can't set CFLAGS if you're not using gcc. Add the -m flag.");
            process::exit(1);
        }

        out
    }
}

fn main() {
    let mut raw_args = std::env::args();
    _ = raw_args.next();
    let args = Args::parse(raw_args);

    if let Err(err) = compile_file(args) {
        eprintln!("{}", err);
        process::exit(1);
    }
}
