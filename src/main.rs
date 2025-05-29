mod parser;
mod tokenizer;

use std::{env, error::Error, fs, process};

fn compile_file(path: String) -> Result<(), Box<dyn Error>> {
    let source = fs::read_to_string(path.clone())?;

    // TODO: basename
    let tokenizer = tokenizer::Tokenizer::new(path, source);
    let tokens = tokenizer.tokenize()?;
    let parser = parser::Parser::new(tokens);

    println!("{:#?}", parser.parse());

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
