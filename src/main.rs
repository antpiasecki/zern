mod codegen;
mod codegen_x86_64;
mod parser;
mod tokenizer;

use std::{
    env,
    error::Error,
    fs,
    path::Path,
    process::{self, Command},
};

fn compile_file(path: String) -> Result<(), Box<dyn Error>> {
    let source = fs::read_to_string(path.clone())?;

    let filename = Path::new(&path).file_name().unwrap().to_str().unwrap();

    let tokenizer = tokenizer::Tokenizer::new(filename.to_owned(), source);
    let tokens = tokenizer.tokenize()?;

    let parser = parser::Parser::new(tokens);
    let statements = parser.parse()?;

    let mut codegen = codegen_x86_64::CodegenX86_64::new_boxed();
    codegen.emit_prologue()?;

    let mut env = codegen::Env::new();
    for stmt in statements {
        codegen.compile_stmt(&mut env, stmt)?;
    }

    codegen.emit_epilogue()?;

    fs::write("out.s", codegen.get_output())?;
    Command::new("nasm")
        .args(["-f", "elf64", "-o", "out.o", "out.s"])
        .status()?;

    // TODO: drop libc entirely
    Command::new("./musl-1.2.4/root/bin/musl-gcc")
        .args(["-static", "-o", "out", "out.o"])
        .status()?;

    Ok(())
}

fn main() -> Result<(), Box<dyn Error>> {
    let mut args = env::args();
    let path = args.nth(1).unwrap();

    if let Err(err) = compile_file(path) {
        eprintln!("{}", err);
        process::exit(1);
    }

    Ok(())
}
