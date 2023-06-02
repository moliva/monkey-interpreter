use crate::{
    ast::{Program, Statement},
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

    fn parse_statement(&self) -> Option<Statement> {
        match self.current_token {
            Token::Let => Some(self.parset_let_statement()),
            _ => None,
        }
    }

    fn parset_let_statement(&self) -> Statement {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use crate::{ast::Statement, lexer::Lexer};

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
            let statement = program.statements[i].clone();
            assert_let_statement(statement, test);
        }
    }

    fn assert_let_statement(statement: Statement, name: &str) {
        let statement_literal = statement.token_literal();
        if statement_literal != "let" {
            panic!(
                "statement.token_literal() not 'let'. got={}",
                statement_literal
            );
        }

        let let_ = match statement {
            Statement::Let(l) => l,
            _ => panic!("statement not a Statement::Let. got={:?}", statement),
        };

        assert_eq!(let_.name.value, name);

        // TODO - add this line - moliva - 2023/05/30
        // assert_eq!(let_.name.token_literal(), name);

        todo!();
    }
}
