use itertools::Itertools;

use crate::token::Token;

// pub enum Node {
//     Statement(String),
//     Expression(String),
// }

#[derive(Debug, Clone)]
pub(crate) struct Let {
    token: Token,
    pub name: Identifier,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub(crate) enum Statement {
    Let(Let),
}

impl Statement {
    pub fn token_literal(&self) -> String {
        match self {
            Statement::Let(Let { token, .. }) => token.literal(),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum Expression {
    Identifier(Identifier),
}
impl Expression {
    fn token_literal(&self) -> String {
        match self {
            Expression::Identifier(Identifier { token, .. }) => token.literal(),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Identifier {
    token: Token,
    pub value: String,
}

pub struct Program {
    pub(crate) statements: Vec<Statement>,
}

impl Program {
    pub fn token_literal(&self) -> String {
        self.statements
            .iter()
            .map(|s| s.token_literal().clone())
            .join("\n")
    }
}
