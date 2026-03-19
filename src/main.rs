mod analyzer;
mod codegen_x86_64;
mod parser;
mod tokenizer;
mod typechecker;

use std::{
    fs,
    path::Path,
    process::{self, Command},
};

use tokenizer::ZernError;

use clap::Parser;

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

    let mut analyzer = analyzer::Analyzer::new();
    for stmt in &statements {
        analyzer.register_declaration(stmt)?;
    }

    let mut typechecker = typechecker::TypeChecker::new(&analyzer);
    for stmt in &statements {
        typechecker.typecheck_stmt(&mut typechecker::Env::new(), stmt)?;
    }

    let mut codegen = codegen_x86_64::CodegenX86_64::new(&analyzer);
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

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    path: String,

    #[arg(short, help = "Output path")]
    out: Option<String>,

    #[arg(short = 'S', help = "Only generate assembly")]
    output_asm: bool,

    #[arg(short = 'r', help = "Run the compiled executable")]
    run_exe: bool,

    #[arg(short = 'm', help = "Use gcc")]
    use_gcc: bool,

    #[arg(short = 'C', default_value = "", help = "Extra flags to pass to gcc")]
    cflags: String,
}

fn main() {
    let args = Args::parse();

    if !args.use_gcc && !args.cflags.is_empty() {
        eprintln!("You can't set CFLAGS if you're not using gcc. Add the -m flag.");
        process::exit(1);
    }

    if let Err(err) = compile_file(args) {
        eprintln!("{}", err);
        process::exit(1);
    }
}
