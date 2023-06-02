use itertools::Itertools;

use crate::token::Token;

pub trait Node {
    fn token_literal(&self) -> String;
}

pub trait Statement {
    fn statement_node(&self);
}

pub trait StatementNode: Node + Statement {}

pub trait Expression {
    fn expression_node(&self);
}

pub trait ExpressionNode: Node + Expression {}

pub struct Program {
    pub statements: Vec<Box<dyn StatementNode>>,
}

impl Node for Program {
    fn token_literal(&self) -> String {
        self.statements.iter().map(|s| s.token_literal()).join("\n")
    }
}

pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Box<dyn ExpressionNode>,
}

impl Statement for LetStatement {
    fn statement_node(&self) {}
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal()
    }
}

pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Expression for Identifier {
    fn expression_node(&self) {}
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal()
    }
}
