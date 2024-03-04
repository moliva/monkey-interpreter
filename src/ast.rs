use itertools::Itertools;

use crate::token::Token;

// pub enum Node {
//     Statement(String),
//     Expression(String),
// }

#[derive(Debug, Clone)]
pub(crate) struct Let {
    pub token: Token,
    pub name: Identifier,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub(crate) enum Statement {
    Let(Let),
    Return(Return),
    Expression(ExpressionStatement),
}

impl Statement {
    pub fn token_literal(&self) -> String {
        match self {
            Statement::Let(Let { token, .. }) => token.literal(),
            Statement::Return(Return { token, .. }) => token.literal(),
            Statement::Expression(ExpressionStatement { token, .. }) => token.literal(),
        }
    }

    pub fn string(&self) -> String {
        match self {
            l @ Statement::Let(li) => format!(
                "{} {} = {};",
                l.token_literal(),
                li.name.string(),
                li.value.string()
            ),
            r @ Statement::Return(rs) => {
                format!("{} {};", r.token_literal(), rs.return_value.string())
            }
            Statement::Expression(es) => es.expression.string(),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct ExpressionStatement {
    pub token: Token,
    pub expression: Expression,
}

#[derive(Debug, Clone)]
pub(crate) struct Return {
    pub token: Token,
    pub return_value: Expression,
}

#[derive(Debug, Clone)]
pub(crate) enum Expression {
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    PrefixExpression(PrefixExpression),
}

impl Expression {
    fn token_literal(&self) -> String {
        match self {
            Expression::Identifier(Identifier { token, .. }) => token.literal(),
            Expression::IntegerLiteral(IntegerLiteral { token, .. }) => token.literal(),
            Expression::PrefixExpression(PrefixExpression { token, .. }) => token.literal(),
        }
    }

    fn string(&self) -> String {
        match self {
            Expression::Identifier(i) => i.string(),
            Expression::IntegerLiteral(i) => i.string(),
            Expression::PrefixExpression(i) => i.string(),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<Expression>,
}

impl PrefixExpression {
    pub fn token_literal(&self) -> String {
        self.token.literal()
    }

    pub fn string(&self) -> String {
        format!("({}{})", self.operator, self.right.string())
    }
}

#[derive(Debug, Clone)]
pub(crate) struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl IntegerLiteral {
    pub fn token_literal(&self) -> String {
        self.token.literal()
    }

    pub fn string(&self) -> String {
        self.token.literal()
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Identifier {
    pub fn token_literal(&self) -> String {
        self.token.literal()
    }

    pub fn string(&self) -> String {
        self.value.clone()
    }
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

    pub fn string(&self) -> String {
        self.statements.iter().map(|s| s.string()).join("\n")
    }
}
