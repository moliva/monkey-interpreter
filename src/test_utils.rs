/// Tries to match a given expression to a pattern and return the identifier given in the
/// pattern, failing otherwise.
macro_rules! match_or_fail {
    ($expression:expr, $pattern:pat => $identifier:ident) => {
        match $expression {
            $pattern => $identifier,
            _ => panic!("not expected type from expression {:?}", $expression),
        }
    };
}

pub(crate) use match_or_fail;

use crate::{
    ast::ast::{Node, Program},
    evaluator::evaluator::eval,
    lexer::Lexer,
    object::{Environment, Object},
    parser::Parser,
};

pub fn test_eval(input: &str) -> Object {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program();

    let env = Environment::default().into_shared();

    eval(&Node::Program(program), &env)
}

pub fn test_parse_program(input: &str) -> Program {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    parser.parse_program()
}
