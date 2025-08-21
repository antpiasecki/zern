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

fn compile_file_to(
    analyzer: &mut analyzer::Analyzer,
    codegen: &mut codegen_x86_64::CodegenX86_64,
    filename: &str,
    source: String,
) -> Result<(), ZernError> {
    let tokenizer = tokenizer::Tokenizer::new(filename.to_owned(), source);
    let tokens = tokenizer.tokenize()?;

    let parser = parser::Parser::new(tokens);
    let statements = parser.parse()?;

    for stmt in &statements {
        analyzer.register_function(stmt)?;
    }

    for stmt in &statements {
        analyzer.analyze_stmt(stmt)?;
    }

    for stmt in statements {
        codegen.compile_stmt(&mut codegen_x86_64::Env::new(), stmt)?;
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

    let mut analyzer = analyzer::Analyzer::new();
    let mut codegen = codegen_x86_64::CodegenX86_64::new();
    codegen.emit_prologue()?;
    compile_file_to(
        &mut analyzer,
        &mut codegen,
        "std.zr",
        include_str!("std.zr").into(),
    )?;
    compile_file_to(&mut analyzer, &mut codegen, filename, source)?;

    if !args.output_asm {
        fs::write(format!("{}.s", args.out), codegen.get_output()).unwrap();

        run_command(format!("nasm -f elf64 -o {}.o {}.s", args.out, args.out));

        // TODO: drop libc entirely
        run_command(format!(
            "./musl-1.2.4/root/bin/musl-gcc -static -o {} {}.o -flto -Wl,--gc-sections {}",
            args.out, args.out, args.cflags
        ));

        if args.run_exe {
            run_command(
                std::fs::canonicalize(args.out)
                    .unwrap()
                    .to_string_lossy()
                    .into_owned(),
            );
        }
    } else {
        fs::write(&args.out, codegen.get_output()).unwrap();
    }

    Ok(())
}

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    path: String,

    #[arg(short, default_value = "out", help = "Output path")]
    out: String,

    #[arg(short = 'S', help = "Only generate assembly")]
    output_asm: bool,

    #[arg(short = 'r', help = "Run the compiled executable")]
    run_exe: bool,

    #[arg(short = 'C', default_value = "", help = "Extra flags to pass to gcc")]
    cflags: String,
}

fn main() {
    let args = Args::parse();

    if let Err(err) = compile_file(args) {
        eprintln!("{}", err);
        process::exit(1);
    }
}
