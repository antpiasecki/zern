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
    codegen: &mut codegen_x86_64::CodegenX86_64,
    filename: &str,
    source: String,
) -> Result<(), ZernError> {
    let tokenizer = tokenizer::Tokenizer::new(filename.to_owned(), source);
    let tokens = tokenizer.tokenize()?;

    let parser = parser::Parser::new(tokens);
    let statements = parser.parse()?;

    for stmt in statements {
        codegen.compile_stmt(&mut codegen_x86_64::Env::new(), stmt)?;
    }
    Ok(())
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
    codegen.emit_prologue()?;
    compile_file_to(&mut codegen, "std.zr", include_str!("std.zr").into())?;
    compile_file_to(&mut codegen, filename, source)?;

    if !args.output_asm {
        if fs::write(format!("{}.s", args.out), codegen.get_output()).is_err() {
            eprintln!("\x1b[91mERROR\x1b[0m: failed to write to {}.s", args.out);
            process::exit(1);
        }

        if !Command::new("sh")
            .args([
                "-c",
                &format!("nasm -f elf64 -o {}.o {}.s", args.out, args.out),
            ])
            .status()
            .unwrap()
            .success()
        {
            process::exit(1);
        }

        // TODO: drop libc entirely
        if !Command::new("sh")
            .args([
                "-c",
                &format!(
                    "./musl-1.2.4/root/bin/musl-gcc -static -o {} {}.o -flto -Wl,--gc-sections {}",
                    args.out, args.out, args.cflags
                ),
            ])
            .status()
            .unwrap()
            .success()
        {
            process::exit(1);
        }
    } else if fs::write(&args.out, codegen.get_output()).is_err() {
        eprintln!("\x1b[91mERROR\x1b[0m: failed to write to {}", args.out);
        process::exit(1);
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
