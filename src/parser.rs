use std::collections::HashMap;
use std::mem::Discriminant;

use crate::{ast::*, lexer::Lexer, token::Token};

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

impl Parser {
    fn new(mut lexer: Lexer) -> Self {
        let current_token = lexer.next_token();
        let peek_token = lexer.next_token();

        let mut parser = Self {
            lexer,
            current_token,
            peek_token,
            errors: Vec::default(),
            prefix_parse_fns: HashMap::default(),
            infix_parse_fns: HashMap::default(),
        };

        parser.register_prefix_fns();
        parser.register_infix_fns();

        parser
    }

    fn parse_function_literal(&mut self) -> Option<Expression> {
        let token = self.current_token.clone();

        if !self.expect_peek(Token::LParen) {
            return None;
        }

        let parameters = self.parse_function_parameters();

        if !self.expect_peek(Token::LBrace) {
            return None;
        }

        let body = self.parse_block_statement();

        Some(Expression::FunctionLiteral(FunctionLiteral {
            token,
            parameters,
            body,
        }))
    }

    fn parse_if_expression(&mut self) -> Option<Expression> {
        let token = self.current_token.clone();

        if !self.expect_peek(Token::LParen) {
            return None;
        }

        self.next_token();

        let condition = self.parse_expression(Precedence::Lowest);
        let condition = match condition {
            Some(c) => Box::new(c),
            None => return None,
        };

        if !self.expect_peek(Token::RParen) {
            return None;
        }

        if !self.expect_peek(Token::LBrace) {
            return None;
        }

        let consequence = self.parse_block_statement();

        let alternative = if self.peek_token == Token::Else {
            self.next_token();

            if !self.expect_peek(Token::LBrace) {
                return None;
            }

            Some(self.parse_block_statement())
        } else {
            None
        };

        Some(Expression::IfExpression(IfExpression {
            token,
            condition,
            consequence,
            alternative,
        }))
    }

    fn parse_call_expression(&mut self, function: Expression) -> Option<Expression> {
        let token = self.current_token.clone();

        let function = Box::new(function);
        let arguments = self.parse_call_arguments();

        Some(Expression::CallExpression(CallExpression {
            token,
            function,
            arguments,
        }))
    }

    fn parse_call_arguments(&mut self) -> Vec<Expression> {
        let mut args = Vec::default();

        if self.peek_token == Token::RParen {
            self.next_token();
            return args;
        }

        self.next_token();
        args.push(
            self.parse_expression(Precedence::Lowest)
                .expect("expression"),
        );

        while self.peek_token == Token::Comma {
            self.next_token();
            self.next_token();
            args.push(
                self.parse_expression(Precedence::Lowest)
                    .expect("expression"),
            );
        }

        if !self.expect_peek(Token::RParen) {
            panic!("expected ')");
        }

        args
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

    fn parse_grouped_expression(&mut self) -> Option<Expression> {
        self.next_token();

        let exp = self.parse_expression(Precedence::Lowest);

        if self.expect_peek(Token::RParen) {
            exp
        } else {
            None
        }
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

    fn parse_boolean(&mut self) -> Option<Expression> {
        let token = self.current_token.clone();
        let literal = token.literal();
        let value = "true" == literal;
        Some(Expression::Boolean(Boolean { token, value }))
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

    fn register_infix(&mut self, token: Token, infix_parse_fn: InfixParseFn) {
        self.infix_parse_fns
            .insert(std::mem::discriminant(&token), infix_parse_fn);
    }

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

        self.next_token();

        let value = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token == Token::Semicolon {
            self.next_token();
        }

        Some(Statement::Let(Let { token, name, value }))
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        let token = self.current_token.clone();

        self.next_token();

        let return_value = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token == Token::Semicolon {
            self.next_token();
        }

        Some(Statement::Return(Return {
            token,
            return_value,
        }))
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let token = self.current_token.clone();

        let expression = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token == Token::Semicolon {
            self.next_token();
        }

        Some(Statement::Expression(ExpressionStatement {
            token,
            expression,
        }))
    }

    fn find_current_prefix_parse_fn(&self) -> Option<PrefixParseFn> {
        let discriminant = std::mem::discriminant(&self.current_token);
        let prefix = self.prefix_parse_fns.get(&discriminant);

        prefix.cloned()
    }

    fn find_peek_infix_parse_fn(&self) -> Option<InfixParseFn> {
        let discriminant = std::mem::discriminant(&self.peek_token);
        let infix = self.infix_parse_fns.get(&discriminant);

        infix.cloned()
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let prefix = match self.find_current_prefix_parse_fn() {
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
            match self.find_peek_infix_parse_fn() {
                None => return left,
                Some(infix) => {
                    self.next_token();

                    left = infix(self, left.unwrap());
                }
            }
        }

        left
    }

    fn parse_block_statement(&mut self) -> BlockStatement {
        let token = self.current_token.clone();

        self.next_token();

        let mut statements = Vec::default();

        while self.current_token != Token::RBrace && self.current_token != Token::EOF {
            let statement = self.parse_statement();

            if let Some(statement) = statement {
                statements.push(statement);
            }

            self.next_token();
        }

        BlockStatement { token, statements }
    }

    fn register_prefix_fns(&mut self) {
        use Token::*;

        self.register_prefix(Ident("".to_owned()), Self::parse_identifier);
        self.register_prefix(Int("".to_owned()), Self::parse_integer_literal);
        self.register_prefix(True, Self::parse_boolean);
        self.register_prefix(False, Self::parse_boolean);
        self.register_prefix(Bang, Self::parse_prefix_expression);
        self.register_prefix(Minus, Self::parse_prefix_expression);
        self.register_prefix(LParen, Self::parse_grouped_expression);
        self.register_prefix(If, Self::parse_if_expression);
        self.register_prefix(Function, Self::parse_function_literal);
    }

    fn parse_function_parameters(&mut self) -> Vec<Identifier> {
        let mut identifiers = Vec::default();

        if self.peek_token == Token::RParen {
            self.next_token();
            return identifiers;
        }

        self.next_token();

        identifiers.push(Identifier {
            token: self.current_token.clone(),
            value: self.current_token.literal(),
        });

        while self.peek_token == Token::Comma {
            // the comma itself
            self.next_token();
            // until next identifier
            self.next_token();

            identifiers.push(Identifier {
                token: self.current_token.clone(),
                value: self.current_token.literal(),
            });
        }

        if !self.expect_peek(Token::RParen) {
            // TODO - this should return an error here - moliva - 2024/03/06
            // return None;
            panic!("expected ')' to close function paramters list");
        }

        identifiers
    }

    fn register_infix_fns(&mut self) {
        use Token::*;
        for token in [Eq, NotEq, Lt, Gt, Plus, Minus, Slash, Asterisk] {
            self.register_infix(token, Self::parse_infix_expression);
        }

        self.register_infix(LParen, Self::parse_call_expression);
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
        LParen => Call,
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
        ast::{Expression, Program, Statement},
        lexer::Lexer,
    };

    use super::Parser;

    #[test]
    fn test_return_statements() {
        // (input, expected_value)
        let tests = [
            ("return 5;", LitVal::Integer(5)),
            ("return true;", LitVal::Boolean(true)),
            ("return asdf;", LitVal::Identifier("asdf".to_owned())),
        ];

        for (input, expected_value) in tests {
            let program = parse_program(input);

            if program.statements.len() != 1 {
                panic!(
                    "program.statements does not contain 1 statement. got={}",
                    program.statements.len()
                );
            }

            let statement = &program.statements[0];

            let return_statement = match statement {
                Statement::Return(r) => r,
                _ => panic!("not a return statement"),
            };

            assert_eq!(return_statement.token.literal(), "return");
            test_literal_expression(&return_statement.return_value, expected_value);
        }
    }

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }";

        let program = parse_program(input);

        assert_eq!(program.statements.len(), 1);

        let statement = &program.statements[0];
        let exp = match statement {
            Statement::Expression(e) => e,
            _ => panic!("statement not a Statement::Expression. got={:?}", statement),
        };

        let exp = match &exp.expression {
            Expression::IfExpression(e) => e,
            _ => panic!(
                "expression not a Expression::IfExpression. got={:?}",
                exp.expression
            ),
        };

        test_infix_expression(
            &exp.condition,
            LitVal::Identifier("x".to_owned()),
            "<",
            LitVal::Identifier("y".to_owned()),
        );
        assert_eq!(exp.consequence.statements.len(), 1);

        let consequence = &exp.consequence.statements[0];
        let consequence = match consequence {
            Statement::Expression(e) => e,
            _ => panic!("statement not a Statement::Expression. got={:?}", statement),
        };

        test_identifier(&consequence.expression, "x");
        assert!(exp.alternative.is_none());
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x } else { y }";

        let program = parse_program(input);

        assert_eq!(program.statements.len(), 1);

        let statement = &program.statements[0];
        let exp = match statement {
            Statement::Expression(e) => e,
            _ => panic!("statement not a Statement::Expression. got={:?}", statement),
        };

        let exp = match &exp.expression {
            Expression::IfExpression(e) => e,
            _ => panic!(
                "expression not a Expression::IfExpression. got={:?}",
                exp.expression
            ),
        };

        test_infix_expression(
            &exp.condition,
            LitVal::Identifier("x".to_owned()),
            "<",
            LitVal::Identifier("y".to_owned()),
        );

        assert_eq!(exp.consequence.statements.len(), 1);
        let consequence = &exp.consequence.statements[0];
        let consequence = match consequence {
            Statement::Expression(e) => e,
            _ => panic!("statement not a Statement::Expression. got={:?}", statement),
        };

        test_identifier(&consequence.expression, "x");

        assert!(exp.alternative.is_some());

        let alternative = exp.alternative.as_ref().unwrap();
        assert_eq!(alternative.statements.len(), 1);

        let alternative = &alternative.statements[0];
        let alternative = match alternative {
            Statement::Expression(e) => e,
            _ => panic!("statement not a Statement::Expression. got={:?}", statement),
        };
        test_identifier(&alternative.expression, "y");
    }

    #[test]
    fn test_let_statements() {
        // (input, expected_identifier, expected_value)
        let tests = [
            ("let x = 5;", "x", LitVal::Integer(5)),
            ("let y = true;", "y", LitVal::Boolean(true)),
            (
                "let foobar = y;",
                "foobar",
                LitVal::Identifier("y".to_owned()),
            ),
        ];

        for (input, expected_identifier, expected_value) in tests {
            let program = parse_program(input);

            if program.statements.len() != 1 {
                panic!(
                    "program.statements does not contain 1 statement. got={}",
                    program.statements.len()
                );
            }

            let statement = &program.statements[0];

            let exp = match statement {
                Statement::Let(e) => e,
                _ => panic!("expected Statement::Let, got {:?}", statement),
            };

            // TODO - ugly thing here - moliva - 2024/03/07
            test_identifier(
                &Expression::Identifier(exp.name.clone()),
                expected_identifier,
            );
            test_literal_expression(&exp.value, expected_value);
        }
    }

    #[test]
    fn test_bool_expression() {
        let input = "true;";

        let program = parse_program(input);

        assert_eq!(program.statements.len(), 1);

        let statement = &program.statements[0];
        let exp = match statement {
            Statement::Expression(e) => e,
            _ => panic!("statement not a Statement::Expression. got={:?}", statement),
        };

        let boolean = match &exp.expression {
            Expression::Boolean(i) => i,
            _ => panic!("expression not a Expression::Boolean. got={:?}", exp),
        };

        assert!(boolean.value);
        assert_eq!(boolean.token_literal(), "true");
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";

        let program = parse_program(input);

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

        let program = parse_program(input);

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
        // (input, left_value, operator, right_value)
        let infix_tests = [
            ("5 + 5;", LitVal::Integer(5), "+", LitVal::Integer(5)),
            ("5 - 5;", LitVal::Integer(5), "-", LitVal::Integer(5)),
            ("5 * 5;", LitVal::Integer(5), "*", LitVal::Integer(5)),
            ("5 / 5;", LitVal::Integer(5), "/", LitVal::Integer(5)),
            ("5 > 5;", LitVal::Integer(5), ">", LitVal::Integer(5)),
            ("5 < 5;", LitVal::Integer(5), "<", LitVal::Integer(5)),
            ("5 == 5;", LitVal::Integer(5), "==", LitVal::Integer(5)),
            ("5 != 5;", LitVal::Integer(5), "!=", LitVal::Integer(5)),
            (
                "true == true",
                LitVal::Boolean(true),
                "==",
                LitVal::Boolean(true),
            ),
            (
                "true != false",
                LitVal::Boolean(true),
                "!=",
                LitVal::Boolean(false),
            ),
            (
                "false == false",
                LitVal::Boolean(false),
                "==",
                LitVal::Boolean(false),
            ),
        ];

        for (input, left_value, operator, right_value) in infix_tests {
            let program = parse_program(input);

            assert_eq!(program.statements.len(), 1);

            let statement = &program.statements[0];
            let exp = match statement {
                Statement::Expression(e) => e,
                _ => panic!("statement not a Statement::Expression. got={:?}", statement),
            };

            test_infix_expression(&exp.expression, left_value, operator, right_value);
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
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            // call expressions
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
        ];

        for (input, expected) in tests {
            let program = parse_program(input);

            let actual = program.string();
            assert_eq!(actual, expected);
        }
    }

    // TODO - copy test call arguments in the go code - moliva - 2024/03/07

    #[test]
    fn test_parsing_prefix_expressions() {
        let prefix_tests = [
            ("!5;", "!", LitVal::Integer(5i64)),
            ("-15;", "-", LitVal::Integer(15)),
            ("!true;", "!", LitVal::Boolean(true)),
            ("!false;", "!", LitVal::Boolean(false)),
        ];

        for (input, operator, value) in prefix_tests.into_iter() {
            let program = parse_program(input);

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
            test_literal_expression(&exp.right, value);
        }
    }

    #[test]
    fn test_function_literal_parsing() {
        let input = "fn(x, y) { x + y; }";

        let program = parse_program(input);

        assert_eq!(program.statements.len(), 1);

        let statement = &program.statements[0];
        let exp = match statement {
            Statement::Expression(e) => e,
            _ => panic!("statement not a Statement::Expression. got={:?}", statement),
        };

        let exp = match &exp.expression {
            Expression::FunctionLiteral(e) => e,
            _ => panic!(
                "expression not a Expression::FunctionLiteral. got={:?}",
                exp
            ),
        };

        assert_eq!(exp.parameters.len(), 2);
        // TODO - omg! - moliva - 2024/03/06
        test_literal_expression(
            &Expression::Identifier(exp.parameters[0].clone()),
            LitVal::Identifier("x".to_owned()),
        );
        test_literal_expression(
            &Expression::Identifier(exp.parameters[1].clone()),
            LitVal::Identifier("y".to_owned()),
        );

        assert_eq!(exp.body.statements.len(), 1);
        let body_statement = match &exp.body.statements[0] {
            Statement::Expression(e) => e,
            _ => panic!("statement not a Statement::Expression. got={:?}", statement),
        };

        test_infix_expression(
            &body_statement.expression,
            LitVal::Identifier("x".to_owned()),
            "+",
            LitVal::Identifier("y".to_owned()),
        );
    }

    #[test]
    fn test_function_parameter_parsing() {
        let tests = [
            ("fn() {};", vec![]),
            ("fn(x) {};", vec!["x"]),
            ("fn(x, y, z) {};", vec!["x", "y", "z"]),
        ];

        for (input, expected_params) in tests {
            let program = parse_program(input);

            assert_eq!(program.statements.len(), 1);

            let statement = &program.statements[0];
            let exp = match statement {
                Statement::Expression(e) => e,
                _ => panic!("statement not a Statement::Expression. got={:?}", statement),
            };

            let exp = match &exp.expression {
                Expression::FunctionLiteral(e) => e,
                _ => panic!(
                    "expression not a Expression::FunctionLiteral. got={:?}",
                    exp
                ),
            };

            assert_eq!(exp.parameters.len(), expected_params.len());

            for (i, identifier) in expected_params.into_iter().enumerate() {
                test_literal_expression(
                    &Expression::Identifier(exp.parameters[i].clone()),
                    LitVal::Identifier(identifier.to_owned()),
                );
            }
        }
    }

    #[test]
    fn test_call_expression_parsing() {
        let input = "add(1, 2 * 3, 4 + 5);";

        let program = parse_program(input);

        assert_eq!(program.statements.len(), 1);

        let statement = &program.statements[0];
        let exp = match statement {
            Statement::Expression(e) => e,
            _ => panic!("statement not a Statement::Expression. got={:?}", statement),
        };

        let exp = match &exp.expression {
            Expression::CallExpression(e) => e,
            _ => panic!("expression not a Expression::CallExpression. got={:?}", exp),
        };

        test_identifier(&exp.function, "add");

        assert_eq!(exp.arguments.len(), 3);
        test_literal_expression(&exp.arguments[0], LitVal::Integer(1));
        test_infix_expression(
            &exp.arguments[1],
            LitVal::Integer(2),
            "*",
            LitVal::Integer(3),
        );
        test_infix_expression(
            &exp.arguments[2],
            LitVal::Integer(4),
            "+",
            LitVal::Integer(5),
        );
    }

    // *****************************************************************************************************
    // *************** Utils ***************
    // *****************************************************************************************************

    fn parse_program(input: &str) -> Program {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);

        program
    }

    /**
     * Used for test helpers below supporting different types
     */
    enum LitVal {
        Integer(i64),
        Identifier(String),
        Boolean(bool),
    }

    fn test_literal_expression(exp: &Expression, expected: LitVal) {
        use LitVal::*;

        match expected {
            Integer(v) => test_integer_literal(exp, v),
            Identifier(v) => test_identifier(exp, &v),
            Boolean(v) => test_boolean_literal(exp, v),
            _ => panic!("type of exp not handle. got={:?}", exp),
        }
    }

    fn test_infix_expression(exp: &Expression, left: LitVal, operator: &str, right: LitVal) {
        let op_exp = match exp {
            Expression::InfixExpression(e) => e,
            _ => panic!(
                "expression not a Expression::InfixExpression. got={:?}",
                exp
            ),
        };

        test_literal_expression(&op_exp.left, left);
        assert_eq!(op_exp.operator, operator);
        test_literal_expression(&op_exp.right, right);
    }

    fn test_identifier(exp: &Expression, value: &str) {
        let identifier = match exp {
            Expression::Identifier(i) => i,
            _ => panic!("expression not a Expression::Identifier. got={:?}", exp),
        };

        assert_eq!(identifier.value, value);
        assert_eq!(identifier.token_literal(), value);
    }

    fn test_integer_literal(exp: &Expression, value: i64) {
        let integer_literal = match exp {
            Expression::IntegerLiteral(i) => i,
            _ => panic!("expression not a Expression::IntegerLiteral. got={:?}", exp),
        };

        assert_eq!(integer_literal.value, value);
        assert_eq!(integer_literal.token_literal(), format!("{value}"));
    }

    fn test_boolean_literal(exp: &Expression, value: bool) {
        let boolean_literal = match exp {
            Expression::Boolean(i) => i,
            _ => panic!("expression not a Expression::Boolean. got={:?}", exp),
        };

        assert_eq!(boolean_literal.value, value);
        assert_eq!(boolean_literal.token_literal(), format!("{value}"));
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
