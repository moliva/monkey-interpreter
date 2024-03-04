use std::collections::HashMap;
use std::mem::Discriminant;

use crate::{
    ast::{
        Expression, ExpressionStatement, Identifier, InfixExpression, IntegerLiteral, Let,
        PrefixExpression, Program, Return, Statement,
    },
    lexer::Lexer,
    token::Token,
};

type PrefixParseFn = fn(p: &mut Parser) -> Option<Expression>;
type InfixParseFn = fn(p: &mut Parser, Expression) -> Option<Expression>;

struct Parser {
    lexer: Lexer,

    current_token: Token,
    peek_token: Token,

    pub errors: Vec<String>,

    prefix_parse_fns: HashMap<Discriminant<Token>, PrefixParseFn>,
    infix_parse_fns: HashMap<Discriminant<Token>, InfixParseFn>,
}

// TODO - precedences map - moliva - 2024/03/04

impl Parser {
    fn new(mut lexer: Lexer) -> Self {
        let current_token = lexer.next_token();
        let peek_token = lexer.next_token();

        let mut prefix_parse_fns = HashMap::<Discriminant<Token>, PrefixParseFn, _>::default();
        prefix_parse_fns.insert(
            std::mem::discriminant(&Token::Ident("".to_owned())),
            Self::parse_identifier,
        );
        prefix_parse_fns.insert(
            std::mem::discriminant(&Token::Int("".to_owned())),
            Self::parse_integer_literal,
        );
        prefix_parse_fns.insert(
            std::mem::discriminant(&Token::Bang),
            Self::parse_prefix_expression,
        );
        prefix_parse_fns.insert(
            std::mem::discriminant(&Token::Minus),
            Self::parse_prefix_expression,
        );

        let mut infix_parse_fns = HashMap::<Discriminant<Token>, InfixParseFn, _>::default();
        let infix_operators = [
            Token::Eq,
            Token::NotEq,
            Token::Lt,
            Token::Gt,
            Token::Plus,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
        ];
        for token in infix_operators.iter() {
            infix_parse_fns.insert(std::mem::discriminant(token), Self::parse_infix_expression);
        }

        Self {
            lexer,
            current_token,
            peek_token,
            errors: Vec::default(),
            prefix_parse_fns,
            infix_parse_fns,
        }
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Option<Expression> {
        let token = self.current_token.clone();
        let operator = token.literal();

        let precedence = self.current_precedence();

        self.next_token();

        let right = self.parse_expression(precedence);

        right.map(|right| {
            Expression::InfixExpression(InfixExpression {
                token,
                operator,
                left: Box::new(left),
                right: Box::new(right),
            })
        })
    }

    fn peek_precedence(&self) -> Precedence {
        let precedence = fetch_precedence(&self.peek_token);

        precedence.unwrap_or(Precedence::Lowest)
    }

    fn current_precedence(&self) -> Precedence {
        let precedence = fetch_precedence(&self.current_token);

        precedence.unwrap_or(Precedence::Lowest)
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        let token = self.current_token.clone();
        let operator = token.literal();

        self.next_token();

        let right = self.parse_expression(Precedence::Prefix);

        right.map(|right| {
            let right = Box::new(right);

            Expression::PrefixExpression(PrefixExpression {
                token,
                operator,
                right,
            })
        })
    }

    fn parse_integer_literal(&mut self) -> Option<Expression> {
        let token = self.current_token.clone();
        let literal = token.literal();
        let value = i64::from_str_radix(&literal, 10);
        if value.is_err() {
            self.errors
                .push(format!("could not parse {} as integer", &literal));
            return None;
        }

        let value = value.unwrap();
        Some(Expression::IntegerLiteral(IntegerLiteral { token, value }))
    }

    fn parse_identifier(&mut self) -> Option<Expression> {
        let token = self.current_token.clone();
        let value = self.current_token.literal();

        Some(Expression::Identifier(Identifier { token, value }))
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
            _ => self.parse_expression_statement(),
        }
    }

    // TODO - unused - moliva - 2024/03/04
    fn register_infix(&mut self, token: Token, infix_parse_fn: InfixParseFn) {
        self.infix_parse_fns
            .insert(std::mem::discriminant(&token), infix_parse_fn);
    }

    // TODO - unused - moliva - 2024/03/04
    fn register_prefix(&mut self, token: Token, prefix_parse_fn: PrefixParseFn) {
        self.prefix_parse_fns
            .insert(std::mem::discriminant(&token), prefix_parse_fn);
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
        // let value = self.parse_expression(Precedence::Call);
        while self.current_token != Token::Semicolon {
            self.next_token();
        }

        let statement = Statement::Let(Let {
            token,
            name,
            value: Expression::Identifier(Identifier {
                token: Token::Assign,
                // TODO - result of expresssion parsed above - moliva - 2024/03/02
                // value,
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
        let discriminant = std::mem::discriminant(&self.current_token);
        let prefix = self.prefix_parse_fns.get(&discriminant);

        let prefix = match prefix {
            Some(p) => p,
            None => {
                self.errors.push(format!(
                    "no prefix parse function for {:?}",
                    self.current_token
                ));
                return None;
            }
        };

        let mut left = prefix(self);

        while self.peek_token != Token::Semicolon && precedence < self.peek_precedence() {
            let discriminant = std::mem::discriminant(&self.peek_token);
            let infix = self.infix_parse_fns.get(&discriminant);
            match infix {
                None => return left,
                Some(infix) => {
                    let infix = *infix;
                    self.next_token();

                    left = infix(self, left.unwrap());
                }
            }
        }

        left
    }
}

const fn fetch_precedence(token_type: &Token) -> Option<Precedence> {
    use Precedence::*;
    use Token::*;

    Some(match *token_type {
        Eq | NotEq => Equals,
        Lt | Gt => LessGreater,
        Plus | Minus => Sum,
        Slash | Asterisk => Product,
        _ => return None,
    })
}

#[derive(PartialEq, PartialOrd)]
enum Precedence {
    Lowest,
    Equals,      // ==
    LessGreater, // > or <
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // myFunction(X)
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

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";

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

        let integer_literal = match &exp.expression {
            Expression::IntegerLiteral(i) => i,
            _ => panic!("expression not a Expression::IntegerLiteral. got={:?}", exp),
        };

        assert_eq!(integer_literal.value, 5);
        assert_eq!(integer_literal.token_literal(), "5");
    }

    #[test]
    fn test_parsing_infix_expressions() {
        let infix_tests = [
            ("5 + 5;", 5, "+", 5),
            ("5 - 5;", 5, "-", 5),
            ("5 * 5;", 5, "*", 5),
            ("5 / 5;", 5, "/", 5),
            ("5 > 5;", 5, ">", 5),
            ("5 < 5;", 5, "<", 5),
            ("5 == 5;", 5, "==", 5),
            ("5 != 5;", 5, "!=", 5),
        ];

        for (input, left_value, operator, right_value) in infix_tests.into_iter() {
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

            let exp = match &exp.expression {
                Expression::InfixExpression(e) => e,
                _ => panic!(
                    "expression not a Expression::InfixExpression. got={:?}",
                    exp
                ),
            };

            test_integer_literal(&exp.left, left_value);
            assert_eq!(exp.operator, operator);
            test_integer_literal(&exp.right, right_value);
        }
    }

    #[test]
    fn test_operator_precende_parsing() {
        let tests = [
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            check_parser_errors(&parser);

            let actual = program.string();
            assert_eq!(actual, expected);
        }
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        let prefix_tests = [("!5;", "!", 5i64), ("-15;", "-", 15)];

        for (input, operator, integer_value) in prefix_tests.into_iter() {
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

            let exp = match &exp.expression {
                Expression::PrefixExpression(e) => e,
                _ => panic!(
                    "expression not a Expression::PrefixExpression. got={:?}",
                    exp
                ),
            };

            assert_eq!(exp.operator, operator);
            test_integer_literal(&exp.right, integer_value);
        }
    }

    fn test_integer_literal(exp: &Expression, value: i64) {
        let integer_literal = match exp {
            Expression::IntegerLiteral(i) => i,
            _ => panic!("expression not a Expression::IntegerLiteral. got={:?}", exp),
        };

        assert_eq!(integer_literal.value, value);
        assert_eq!(integer_literal.token_literal(), format!("{value}"));
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
