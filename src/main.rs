mod codegen_x86_64;
mod parser;
mod tokenizer;

use std::{
    env, fs,
    path::Path,
    process::{self, Command},
};

use tokenizer::ZernError;

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

fn compile_file(path: String) -> Result<(), ZernError> {
    let source = match fs::read_to_string(path.clone()) {
        Ok(x) => x,
        Err(_) => {
            eprintln!("\x1b[91mERROR\x1b[0m: failed to open {}", path);
            process::exit(1);
        }
    };

    let filename = Path::new(&path).file_name().unwrap().to_str().unwrap();

    let mut codegen = codegen_x86_64::CodegenX86_64::new();
    codegen.emit_prologue()?;
    compile_file_to(&mut codegen, "std.zr", include_str!("std.zr").into())?;
    compile_file_to(&mut codegen, filename, source)?;

    // TODO
    if fs::write("out.s", codegen.get_output()).is_err() {
        eprintln!("\x1b[91mERROR\x1b[0m: failed to write to out.s");
        process::exit(1);
    }

    if !Command::new("nasm")
        .args(["-f", "elf64", "-w+all", "-o", "out.o", "out.s"])
        .status()
        .unwrap()
        .success()
    {
        process::exit(1);
    }

    // TODO: drop libc entirely
    if !Command::new("./musl-1.2.4/root/bin/musl-gcc")
        .args(["-static", "-o", "out", "out.o"])
        .status()
        .unwrap()
        .success()
    {
        process::exit(1);
    }

    Ok(())
}

fn main() {
    let mut args = env::args();
    let _ = args.next();

    let path = match args.next() {
        Some(x) => x,
        None => {
            eprintln!("\x1b[91mERROR\x1b[0m: expected an argument");
            process::exit(1);
        }
    };

    if let Err(err) = compile_file(path) {
        eprintln!("{}", err);
        process::exit(1);
    }
}
