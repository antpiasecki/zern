mod codegen_x86_64;
mod monomorphizer;
mod parser;
mod symbol_table;
mod tokenizer;
mod typechecker;

use std::{
    collections::{HashMap, HashSet},
    fs,
    process::{Command, exit},
};

use nanoserde::SerJson as _;
use tokenizer::ZernError;

fn main() {
    let args = Args::parse(std::env::args());

    if let Err(err) = compile_file(&args) {
        print_error(&args, err);
        exit(1);
    }
}

fn compile_file(args: &Args) -> Result<(), ZernError> {
    let source = if args.stdin_input {
        std::io::read_to_string(std::io::stdin()).unwrap_or_else(|_| {
            eprintln!("ERROR: failed to read from stdin");
            exit(1);
        })
    } else {
        fs::read_to_string(&args.path).unwrap_or_else(|e| {
            eprintln!("ERROR: failed to open {}: {e}", args.path);
            exit(1);
        })
    };

    let mut included_paths = HashSet::new();
    if let Ok(root) = fs::canonicalize(&args.path) {
        included_paths.insert(root);
    }
    // atp we dont know yet how many files we are including so a 15k estimate must do
    let mut tokens = Vec::with_capacity(15000);
    let tokenizer = tokenizer::Tokenizer::new(&mut tokens, args.path.clone(), source, &mut included_paths);
    tokenizer.tokenize()?;

    let parser = parser::Parser::new(tokens);
    let statements = parser.parse()?;

    let mut symbol_table = symbol_table::SymbolTable::new();
    for stmt in &statements {
        symbol_table.register_declaration(stmt)?;
    }

    let mut monomorphizer = monomorphizer::Monomorphizer::new(&mut symbol_table);
    let statements = monomorphizer.monomorphize(statements)?;

    if args.dump {
        println!("{}", statements.serialize_json());
    }

    symbol_table
        .constants
        .insert("_WINDOWS".into(), (args.mode == CompileMode::MinGW) as i64);

    let mut typechecker = typechecker::TypeChecker::new(&symbol_table);
    for stmt in &statements {
        typechecker.typecheck_stmt(&mut typechecker::Env::new(), stmt)?;
    }

    if args.dump {
        println!(
            "{}",
            typechecker
                .expr_types
                .iter()
                .map(|(k, v)| (k.to_string(), v.clone()))
                .collect::<HashMap<String, String>>()
                .serialize_json()
        );
        return Ok(());
    }

    let mut codegen = codegen_x86_64::CodegenX86_64::new(&args, &symbol_table, &typechecker.expr_types);
    codegen.emit_prologue()?;
    for stmt in statements {
        codegen.compile_stmt(&mut codegen_x86_64::Env::new(), &stmt)?;
    }

    if !args.skip_build {
        let out = args.out.clone().unwrap_or_else(|| "out".into());

        fs::write(format!("{out}.s"), codegen.get_output()).unwrap();

        let debug_flag = if args.emit_debug { "-g" } else { "" };

        match args.mode {
            CompileMode::MinGW => run_command(format!("x86_64-w64-mingw32-as {debug_flag} -o {out}.o {out}.s")),
            _ => run_command(format!("as --64 {debug_flag} -o {out}.o {out}.s")),
        }

        match args.mode {
            CompileMode::StaticLD => run_command(format!(
                "ld -static -o {out} {out}.o --gc-sections -e _start {}",
                args.link_flags
            )),
            CompileMode::DynamicLD => run_command(format!(
                "ld --dynamic-linker /lib64/ld-linux-x86-64.so.2 -o {out} {out}.o --gc-sections -e _start {}",
                args.link_flags
            )),
            CompileMode::CC => run_command(format!(
                "cc -no-pie -o {out} {out}.o -flto -Wl,--gc-sections {}",
                args.link_flags
            )),
            CompileMode::MinGW => run_command(format!(
                "x86_64-w64-mingw32-gcc -o {out} {out}.o -flto -Wl,--gc-sections {}",
                args.link_flags
            )),
        }

        if args.run_exe {
            run_command(std::fs::canonicalize(out).unwrap().to_string_lossy().into_owned());
        }
    } else {
        fs::write(args.out.clone().unwrap_or_else(|| "out.s".into()), codegen.get_output()).unwrap();
    }

    Ok(())
}

fn print_error(args: &Args, e: ZernError) {
    if args.json_errors {
        eprintln!("{}", e.serialize_json());
        return;
    }

    eprintln!("{}: \x1b[91mERROR\x1b[0m: {}", e.loc, e.message);
    if *e.loc.filename != *"<unknown>" && e.loc.line > 0 {
        if let Ok(src) = fs::read_to_string(e.loc.filename.as_ref()) {
            if let Some(line) = src.lines().nth(e.loc.line - 1) {
                if e.loc.line > 1 {
                    if let Some(previous_line) = src.lines().nth(e.loc.line - 2) {
                        eprintln!("{} | \x1b[90m{}\x1b[0m", e.loc.line - 1, previous_line);
                    }
                }

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

#[derive(PartialEq)]
enum CompileMode {
    StaticLD,
    DynamicLD,
    CC,
    MinGW,
}

struct Args {
    path: String,
    out: Option<String>,
    emit_debug: bool,
    run_exe: bool,
    skip_build: bool,
    mode: CompileMode,
    link_flags: String,
    json_errors: bool,
    dump: bool,
    stdin_input: bool,
}

impl Args {
    fn parse(mut args: std::env::Args) -> Args {
        _ = args.next(); // skip the program name

        let mut out = Args {
            path: String::new(),
            out: None,
            emit_debug: false,
            run_exe: false,
            skip_build: false,
            mode: CompileMode::StaticLD,
            link_flags: String::new(),
            json_errors: false,
            dump: false,
            stdin_input: false,
        };

        while let Some(arg) = args.next() {
            if arg == "-o" {
                match args.next() {
                    Some(s) => out.out = Some(s),
                    None => usage_error("-o option requires a value"),
                }
            } else if arg == "-r" {
                out.run_exe = true;
            } else if arg == "-g" {
                out.emit_debug = true;
            } else if arg == "-S" {
                out.skip_build = true;
            } else if arg == "-m" {
                match args.next().as_deref() {
                    Some(s) => match s {
                        "static" => out.mode = CompileMode::StaticLD,
                        "dyn" => out.mode = CompileMode::DynamicLD,
                        "cc" => out.mode = CompileMode::CC,
                        "mingw" => out.mode = CompileMode::MinGW,
                        _ => usage_error("unrecognized mode"),
                    },
                    _ => usage_error("-m option requires a value"),
                }
            } else if arg == "-L" {
                if !out.link_flags.is_empty() {
                    usage_error("-L option can be used only once");
                }
                match args.next() {
                    Some(s) => out.link_flags = s,
                    None => usage_error("-L option requires a value"),
                }
            } else if arg == "--json-errors" {
                out.json_errors = true
            } else if arg == "--dump" {
                out.dump = true
            } else if arg == "--stdin-input" {
                out.stdin_input = true
            } else if arg == "-h" || arg == "--help" {
                print_usage();
                exit(0);
            } else if arg.starts_with('-') {
                usage_error(&format!("unrecognized argument: {arg}"));
            } else if out.path.is_empty() {
                out.path = arg
            } else {
                usage_error(&format!("unrecognized argument: {arg}"));
            }
        }

        if out.path.is_empty() {
            usage_error("you must provide a path");
        }

        if out.skip_build && (out.run_exe || !out.link_flags.is_empty()) {
            usage_error("-S doesn't make sense with -r or -L");
        }

        out
    }
}

fn usage_error(msg: &str) -> ! {
    eprintln!("\x1b[91mERROR\x1b[0m: {msg}");
    print_usage();
    exit(1);
}

fn print_usage() {
    eprintln!(
        r#"
Usage: zern [-o <path>] [-r] [-g] [-S] [-m <static|dyn|cc|mingw>] [-L <flags>] <path>

  -o <path>                - specifies the output path
  -r                       - runs the output executable after compilation
  -g                       - emit debug information into the binary
  -S                       - skip the build, only emit .s
  -m <static|dyn|cc|mingw> - specify the build mode
  -L <flags>               - specify flags to pass to your linker of choice

Dev flags
  --json-errors            - print errors as JSON
  --dump                   - dumps AST and types as JSON
  --stdin-input            - read the source code from stdin
"#
    );
}
