use anyhow::Result;
use std::io::{Stdin, Stdout, Write};

use crate::{lexer::Lexer, parser::Parser, token::Token};

const PROMPT: &str = ">> ";

pub fn start(in_: Stdin, out: &mut Stdout) -> Result<()> {
    let mut input = String::new();
    loop {
        write!(out, "{}", PROMPT)?;
        out.flush()?;

        let _read = in_.read_line(&mut input)?;

        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        if !parser.errors.is_empty() {
            print_parser_errors(out, &parser.errors)?;
        }

        writeln!(out, "{}", program.string())?;

        input.clear();
    }
}

const MONKEY_FACE: &str = r#"            __,__
   .--.  .-"     "-.  .--.
  / .. \/  .-. .-.  \/ .. \
 | |  '|  /   Y   \  |'  | |
 | \   \  \ 0 | 0 /  /   / |
  \ '- ,\.-"""""""-./, -' /
   ''-' /_   ^ ^   _\ '-''
       |  \._   _./  |
       \   \ '~' /   /
        '._ '-=-' _.'
           '----'
"#;

fn print_parser_errors(out: &mut Stdout, errors: &Vec<String>) -> Result<()> {
    write!(out, "{}", MONKEY_FACE)?;
    writeln!(out, "Woops! We ran into some monkey business here!")?;
    writeln!(out, " parser errors:")?;
    for msg in errors {
        writeln!(out, "\t{}", msg)?;
    }
    Ok(())
}
