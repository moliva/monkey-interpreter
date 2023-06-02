use crate::{
    ast::{Program, Statement, StatementNode},
    lexer::Lexer,
    token::Token,
};

struct Parser {
    lexer: Lexer,

    current_token: Token,
    peek_token: Token,
}

impl Parser {
    fn new(mut lexer: Lexer) -> Self {
        let current_token = lexer.next_token();
        let peek_token = lexer.next_token();

        Self {
            lexer,
            current_token,
            peek_token,
        }
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn parse_program(&mut self) -> Program {
        let mut statements = vec![];

        while self.current_token != Token::EOF {
            let statement = self.parse_statement();

            if let Some(statement) = statement {
                statements.push(statement);
            }

            self.next_token();
        }

        Program { statements }
    }

    fn parse_statement(&self) -> Option<Box<dyn StatementNode>> {
        match self.current_token {
            Token::Let => Some(self.parset_let_statement()),
            _ => None,
        }
    }

    fn parset_let_statement(&self) -> Box<dyn StatementNode> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use std::any::Any;

    use crate::{
        ast::{LetStatement, Node, Statement, StatementNode},
        lexer::Lexer,
    };

    use super::Parser;

    #[test]
    fn test_let_statements() {
        let input = r#"
let x = 5;
let y = 10;
let foobar = 838383;
"#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        if program.statements.len() != 3 {
            panic!(
                "program.statements does not contain 3 statement. got={}",
                program.statements.len()
            );
        }

        let tests = vec!["x", "y", "foobar"];
        for (i, test) in tests.iter().enumerate() {
            let statement = program.statements[i];
            assert_let_statement(statement, test);
        }
    }

    fn assert_let_statement(statement: Box<dyn StatementNode>, name: &str) {
        let statement_literal = statement.token_literal();
        assert_eq!(statement_literal, "let");

        let let_ = match statement {
            l @ LetStatement { .. } => l,
            _ => panic!("statement not a Statement::Let. got={:?}", statement),
        };

        assert_eq!(let_.name.value, name);

        let name_ = let_.name;
        name_.token_literal();
        assert_eq!(name_.token_literal(), name);

        todo!();
    }
}
