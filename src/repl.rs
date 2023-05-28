use anyhow::Result;
use std::io::{Stdin, Stdout, Write};

use crate::{lexer::Lexer, token::Token};

const PROMPT: &str = ">> ";

pub fn start(in_: Stdin, out: &mut Stdout) -> Result<()> {
    let mut input = String::new();
    loop {
        write!(out, "{}", PROMPT)?;
        out.flush()?;

        let read = in_.read_line(&mut input)?;
        if read == 0 {
            // TODO - do/say sth - moliva - 2023/05/28
        }

        let mut lexer = Lexer::new(&input);

        let mut token = lexer.next_token();
        while token != Token::EOF {
            writeln!(out, "{:?}", token)?;

            token = lexer.next_token();
        }
    }
}
