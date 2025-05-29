mod codegen;
mod parser;
mod tokenizer;

use std::{
    env,
    error::Error,
    fs,
    process::{self, Command},
};

fn compile_file(path: String) -> Result<(), Box<dyn Error>> {
    let source = fs::read_to_string(path.clone())?;

    // TODO: basename
    let tokenizer = tokenizer::Tokenizer::new(path, source);
    let tokens = tokenizer.tokenize()?;

    let parser = parser::Parser::new(tokens);
    let expr = parser.parse()?;

    let mut codegen = codegen::Codegen::new();
    codegen.emit_prologue()?;
    codegen.compile_expr(expr)?;
    codegen.emit_epilogue()?;

    fs::write("out.s", codegen.get_output())?;
    Command::new("nasm")
        .args(["-f", "elf64", "-o", "out.o", "out.s"])
        .output()?;

    Command::new("ld")
        .args([
            "-dynamic-linker",
            "/lib64/ld-linux-x86-64.so.2",
            "-lc",
            "/usr/lib/x86_64-linux-gnu/crt1.o",
            "-o",
            "out",
            "out.o",
        ])
        .output()?;

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
