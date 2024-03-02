use std::collections::HashMap;

use crate::{
    ast::{Expression, ExpressionStatement, Identifier, Let, Program, Return, Statement},
    lexer::Lexer,
    token::Token,
};

type PrefixParseFn = fn() -> Expression;
type InfixParseFn = fn(Expression) -> Expression;

struct Parser {
    lexer: Lexer,

    current_token: Token,
    peek_token: Token,

    pub errors: Vec<String>,

    prefix_parse_fns: HashMap<Token, PrefixParseFn>,
    infix_parse_fns: HashMap<Token, InfixParseFn>,
}

impl Parser {
    fn new(mut lexer: Lexer) -> Self {
        let current_token = lexer.next_token();
        let peek_token = lexer.next_token();

        let mut prefix_parse_fns = HashMap::default();
        // TODO - review this - moliva - 2023/06/03
        // prefix_parse_fns.insert(Token::Ident("".to_owned()), parse_identifier);

        Self {
            lexer,
            current_token,
            peek_token,
            errors: Vec::default(),
            prefix_parse_fns,
            infix_parse_fns: HashMap::default(),
        }
    }

    pub fn peek_error(&mut self, token: Token) {
        let msg = format!(
            "expected next token to be {:?}, got {:?} instead",
            token, self.peek_token
        );
        self.errors.push(msg);
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

    fn expect_peek(&mut self, expected: Token) -> bool {
        if std::mem::discriminant(&self.peek_token) == std::mem::discriminant(&expected) {
            self.next_token();
            true
        } else {
            self.peek_error(expected);
            false
        }
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        use Token::*;
        match self.current_token {
            Let => self.parse_let_statement(),
            Return => self.parse_return_statement(),
            // If  => self.parse_if_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn register_infix(&mut self, token: Token, infix_parse_fn: InfixParseFn) {
        self.infix_parse_fns.insert(token, infix_parse_fn);
    }

    fn register_prefix(&mut self, token: Token, prefix_parse_fn: PrefixParseFn) {
        self.prefix_parse_fns.insert(token, prefix_parse_fn);
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        let token = self.current_token.clone();

        // TODO - avoid creating empty tokens - moliva - 2024/03/01
        if !self.expect_peek(Token::Ident("".to_owned())) {
            return None;
        }

        let name = Identifier {
            token: self.current_token.clone(),
            value: self.current_token.literal(),
        };

        if !self.expect_peek(Token::Assign) {
            return None;
        }

        // TODO - skipping expressions - moliva - 2023/06/02
        while self.current_token != Token::Semicolon {
            self.next_token();
        }

        let mut statement = Statement::Let(Let {
            token,
            name,
            value: Expression::Identifier(Identifier {
                token: Token::Assign,
                // TODO - result of expresssion parsed above - moliva - 2024/03/02
                value: "".to_owned(),
            }),
        });

        Some(statement)
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        let token = self.current_token.clone();

        self.next_token();

        // TODO - skipping the expressions until semicolon - moliva - 2023/06/03
        while self.current_token != Token::Semicolon {
            self.next_token();
        }

        Some(Statement::Return(Return {
            token,
            return_value: Expression::Identifier(Identifier {
                token: Token::Let,
                value: "".to_owned(),
            }),
        }))
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let token = self.current_token.clone();

        // TODO - review this - moliva - 2023/06/03
        let expression = self
            .parse_expression(Precedence::Lowest)
            .expect("valid expression");

        if self.peek_token == Token::Semicolon {
            self.next_token();
        }

        Some(Statement::Expression(ExpressionStatement {
            token,
            expression,
        }))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let prefix = self.prefix_parse_fns.get(&self.current_token);
        let prefix = match prefix {
            Some(p) => p,
            None => return None,
        };

        let left_expression = prefix();

        Some(left_expression)
    }
}

enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{Expression, Statement},
        lexer::Lexer,
    };

    use super::Parser;

    #[test]
    fn test_return_statements() {
        let input = r#"
return 5;
return 10;
return 993322;
"#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);

        if program.statements.len() != 3 {
            panic!(
                "program.statements does not contain 3 statement. got={}",
                program.statements.len()
            );
        }

        for statement in program.statements.iter() {
            let return_statement = match statement {
                r @ Statement::Return(_) => r,
                _ => panic!("not a return statement"),
            };

            assert_eq!(return_statement.token_literal(), "return");
        }
    }

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
        check_parser_errors(&parser);

        if program.statements.len() != 3 {
            panic!(
                "program.statements does not contain 3 statement. got={}",
                program.statements.len()
            );
        }

        let tests = ["x", "y", "foobar"];
        for (i, test) in tests.iter().enumerate() {
            let statement = program.statements[i].clone();
            assert_let_statement(statement, test);
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(program.statements.len(), 1);

        let statement = &program.statements[0];
        let exp = match statement {
            Statement::Expression(e) => e,
            _ => panic!("statement not a Statement::Expression. got={:?}", statement),
        };

        let ident = match &exp.expression {
            Expression::Identifier(i) => i,
            _ => panic!("expression not a Expression::Identifier. got={:?}", exp),
        };

        assert_eq!(ident.value, "foobar");
        assert_eq!(ident.token_literal(), "foobar");
    }

    fn check_parser_errors(parser: &Parser) {
        let errors = &parser.errors;

        if errors.is_empty() {
            // no errors, hurray!
            return;
        }

        eprintln!("parser has {} errors", errors.len());
        for err in errors.iter() {
            eprintln!("parser error: {err}");
        }
        panic!();
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
        assert_eq!(let_.name.token_literal(), name);
    }
}
