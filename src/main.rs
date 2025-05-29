mod tokenizer;

use std::{env, error::Error, fs};

fn main() -> Result<(), Box<dyn Error>> {
    let mut args = env::args();
    let path = args.nth(1).unwrap();

    let source = fs::read_to_string(path.clone())?;

    // TODO: basename
    let tokenizer = tokenizer::Tokenizer::new(path, source);
    println!("{:#?}", tokenizer.tokenize()?);

    Ok(())
}
