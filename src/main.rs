mod codegen_x86_64;
mod parser;
mod symbol_table;
mod tokenizer;
mod typechecker;

use std::{
    collections::HashSet,
    fs,
    process::{Command, exit},
};

use tokenizer::ZernError;

fn compile_file(args: Args) -> Result<(), ZernError> {
    let source = match fs::read_to_string(&args.path) {
        Ok(x) => x,
        Err(e) => {
            eprintln!("ERROR: failed to open {}: {e}", args.path);
            exit(1);
        }
    };

    let mut included_paths = HashSet::new();
    let tokenizer = tokenizer::Tokenizer::new(args.path.clone(), source, &mut included_paths);
    let parser = parser::Parser::new(tokenizer.tokenize()?);
    let statements = parser.parse()?;

    let mut symbol_table = symbol_table::SymbolTable::new();
    for stmt in &statements {
        symbol_table.register_declaration(stmt)?;
    }

    symbol_table
        .constants
        .insert("_WINDOWS".into(), args.target_windows as i64);

    let mut typechecker = typechecker::TypeChecker::new(&symbol_table);
    for stmt in &statements {
        typechecker.typecheck_stmt(&mut typechecker::Env::new(), stmt)?;
    }

    let mut codegen = codegen_x86_64::CodegenX86_64::new(&args, &symbol_table, &typechecker.expr_types);
    codegen.emit_prologue()?;
    for stmt in statements {
        codegen.compile_stmt(&mut codegen_x86_64::Env::new(), &stmt)?;
    }

    if !args.emit_only {
        let out = args.out.clone().unwrap_or_else(|| "out".into());

        fs::write(format!("{out}.s"), codegen.get_output()).unwrap();

        let debug_flag = if args.emit_debug { "-g" } else { "" };

        if args.target_windows {
            run_command(format!("x86_64-w64-mingw32-as {debug_flag} -o {out}.o {out}.s"));
        } else {
            run_command(format!("as --64 {debug_flag} -o {out}.o {out}.s"));
        }

        if args.target_windows {
            run_command(format!(
                "x86_64-w64-mingw32-gcc -o {out} {out}.o -flto -Wl,--gc-sections {}",
                args.cflags
            ));
        } else if args.use_crt {
            run_command(format!(
                "cc -no-pie -o {out} {out}.o -flto -Wl,--gc-sections {}",
                args.cflags
            ));
        } else {
            run_command(format!("ld -static -o {out} {out}.o --gc-sections -e _start"));
        }

        if args.run_exe {
            run_command(std::fs::canonicalize(out).unwrap().to_string_lossy().into_owned());
        }
    } else {
        fs::write(args.out.clone().unwrap_or_else(|| "out.s".into()), codegen.get_output()).unwrap();
    }

    Ok(())
}

fn run_command(cmd: String) {
    #[cfg(not(windows))]
    let result = Command::new("sh").args(["-c", &cmd]).status();
    #[cfg(windows)]
    let result = Command::new("cmd").args(["/c", &cmd]).status();

    match result {
        Ok(status) if status.success() => {}
        Ok(status) => exit(status.code().unwrap_or(1)),
        Err(e) => {
            eprintln!("failed to run command '{cmd}': {e}");
            exit(1);
        }
    }
}

struct Args {
    path: String,
    out: Option<String>,
    emit_only: bool,
    emit_debug: bool,
    run_exe: bool,
    use_crt: bool,
    target_windows: bool,
    cflags: String,
}

impl Args {
    fn parse(mut args: std::env::Args) -> Args {
        _ = args.next(); // skip the program name

        let mut out = Args {
            path: String::new(),
            out: None,
            emit_only: false,
            emit_debug: false,
            run_exe: false,
            use_crt: false,
            target_windows: false,
            cflags: String::new(),
        };

        while let Some(arg) = args.next() {
            if arg == "-o" {
                match args.next() {
                    Some(s) => out.out = Some(s),
                    None => {
                        eprintln!("ERROR: -o option requires a path");
                        print_usage();
                        exit(1);
                    }
                }
            } else if arg == "--emit-only" {
                out.emit_only = true;
            } else if arg == "-r" {
                out.run_exe = true;
            } else if arg == "-m" {
                out.use_crt = true;
            } else if arg == "-g" {
                out.emit_debug = true;
            } else if arg == "-w" {
                out.target_windows = true;
            } else if arg == "-C" {
                match args.next() {
                    Some(s) => out.cflags = s,
                    None => {
                        eprintln!("ERROR: -C option requires a value");
                        print_usage();
                        exit(1);
                    }
                }
            } else if arg == "-h" || arg == "--help" {
                print_usage();
                exit(0);
            } else if arg.starts_with('-') {
                eprintln!("ERROR: unrecognized option: {arg}");
                print_usage();
                exit(1);
            } else if out.path.is_empty() {
                out.path = arg
            } else {
                eprintln!("ERROR: unrecognized argument: {arg}");
                print_usage();
                exit(1);
            }
        }

        if out.path.is_empty() {
            eprintln!("ERROR: You must provide a path");
            print_usage();
            exit(1);
        }

        if !out.use_crt && !out.cflags.is_empty() {
            eprintln!("ERROR: You can't set CFLAGS if you're not using the C runtime. Add the -m flag.");
            exit(1);
        }

        if !out.use_crt && out.target_windows {
            eprintln!("ERROR: Using the -w flag without -m is not implemented yet. Add -m to flags.");
            exit(1);
        }

        out
    }
}

fn print_usage() {
    println!("Usage: zern [-o path] [-r] [-m] [-g] [-w] [-C cflags] [--emit-only] path");
    println!();
    println!("  -o <path>   - specifies the output path");
    println!("  -r          - runs the output executable after compilation");
    println!("  -m          - link against the C runtime");
    println!("  -w          - build a Windows executable");
    println!("  -g          - emit debug information in the binary");
    println!("  -C <flags>  - flags to pass to the C compiler");
    println!("  --emit-only - only emit the assembly");
}

fn print_error(e: ZernError) {
    eprintln!("{}: \x1b[91mERROR\x1b[0m: {}", e.loc, e.message);
    if e.loc.filename != "<unknown>" && e.loc.line > 0 {
        if let Ok(src) = fs::read_to_string(&e.loc.filename) {
            if let Some(line) = src.lines().nth(e.loc.line - 1) {
                let line_num_str = e.loc.line.to_string();
                eprintln!("{} | {}", line_num_str, line);

                let col0 = e.loc.column.saturating_sub(1);
                let underline_len = e.loc.length.max(1);
                let caret_line = format!("{}{}", " ".repeat(col0), "^".repeat(underline_len));

                eprintln!(
                    "{:>width$} | \x1b[91m{}\x1b[0m",
                    "",
                    caret_line,
                    width = line_num_str.len()
                );
            }
        }
    }
}

fn main() {
    let args = Args::parse(std::env::args());

    if let Err(err) = compile_file(args) {
        print_error(err);
        exit(1);
    }
}
