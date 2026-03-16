mod analyzer;
mod codegen_x86_64;
mod parser;
mod tokenizer;

use std::{
    fs,
    path::Path,
    process::{self, Command},
};

use tokenizer::ZernError;

use clap::Parser;

macro_rules! compile_std {
    ($codegen:expr, $( $name:literal ),* $(,)? ) => {
        $(
            compile_file_to(
                $codegen,
                $name,
                include_str!(concat!("std/", $name)).into()
            )?;
        )*
    };
}

fn compile_file_to(
    codegen: &mut codegen_x86_64::CodegenX86_64,
    filename: &str,
    source: String,
) -> Result<(), ZernError> {
    let tokenizer = tokenizer::Tokenizer::new(filename.to_owned(), source);
    let tokens = tokenizer.tokenize()?;

    let parser = parser::Parser::new(tokens);
    let statements = parser.parse()?;

    for stmt in &statements {
        codegen.analyzer.register_function(stmt)?;
    }

    for stmt in &statements {
        codegen.analyzer.analyze_stmt(stmt)?;
    }

    for stmt in statements {
        // top level statements are all function/const/extern declarations so a new env for each
        codegen.compile_stmt(&mut codegen_x86_64::Env::new(), &stmt)?;
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

fn compile_file(args: Args) -> Result<(), ZernError> {
    let source = match fs::read_to_string(&args.path) {
        Ok(x) => x,
        Err(_) => {
            eprintln!("\x1b[91mERROR\x1b[0m: failed to open {}", args.path);
            process::exit(1);
        }
    };

    let filename = Path::new(&args.path).file_name().unwrap().to_str().unwrap();

    let mut codegen = codegen_x86_64::CodegenX86_64::new();
    codegen.emit_prologue(args.use_gcc)?;
    compile_std!(&mut codegen, "syscalls.zr", "std.zr", "net.zr");
    compile_file_to(&mut codegen, filename, source)?;

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
