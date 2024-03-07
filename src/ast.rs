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
    Block(BlockStatement),
}

impl Statement {
    pub fn token_literal(&self) -> String {
        use Statement::*;

        match self {
            Let(self::Let { token, .. }) => token.literal(),
            Return(self::Return { token, .. }) => token.literal(),
            Expression(ExpressionStatement { token, .. }) => token.literal(),
            Block(BlockStatement { token, .. }) => token.literal(),
        }
    }

    pub fn string(&self) -> String {
        use Statement::*;

        match self {
            l @ Let(li) => format!(
                "{} {} = {};",
                l.token_literal(),
                li.name.string(),
                li.value.string()
            ),
            r @ Return(rs) => {
                format!("{} {};", r.token_literal(), rs.return_value.string())
            }
            Expression(es) => es.expression.string(),
            Block(bs) => bs.string(),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>,
}

impl BlockStatement {
    pub fn string(&self) -> String {
        self.statements.iter().map(|s| s.string()).join("")
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
    Boolean(Boolean),
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
    IfExpression(IfExpression),
    FunctionLiteral(FunctionLiteral),
    CallExpression(CallExpression),
}

impl Expression {
    fn token_literal(&self) -> String {
        match self {
            Expression::Identifier(Identifier { token, .. }) => token.literal(),
            Expression::IntegerLiteral(IntegerLiteral { token, .. }) => token.literal(),
            Expression::Boolean(Boolean { token, .. }) => token.literal(),
            Expression::PrefixExpression(PrefixExpression { token, .. }) => token.literal(),
            Expression::InfixExpression(InfixExpression { token, .. }) => token.literal(),
            Expression::IfExpression(IfExpression { token, .. }) => token.literal(),
            Expression::FunctionLiteral(FunctionLiteral { token, .. }) => token.literal(),
            Expression::CallExpression(CallExpression { token, .. }) => token.literal(),
        }
    }

    fn string(&self) -> String {
        match self {
            Expression::Identifier(i) => i.string(),
            Expression::IntegerLiteral(i) => i.string(),
            Expression::Boolean(i) => i.string(),
            Expression::PrefixExpression(i) => i.string(),
            Expression::InfixExpression(i) => i.string(),
            Expression::IfExpression(i) => i.string(),
            Expression::FunctionLiteral(i) => i.string(),
            Expression::CallExpression(i) => i.string(),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct CallExpression {
    pub token: Token, // the '(' token
    // TODO - should we verify the kind of expression below? - moliva - 2024/03/07
    pub function: Box<Expression>, // Identifier or FunctionLiteral
    pub arguments: Vec<Expression>,
}

impl CallExpression {
    pub fn token_literal(&self) -> String {
        self.token.literal()
    }

    pub fn string(&self) -> String {
        let args = self.arguments.iter().map(Expression::string).join(", ");

        format!("{}({args})", self.function.string())
    }
}

#[derive(Debug, Clone)]
pub(crate) struct IfExpression {
    pub token: Token,
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl IfExpression {
    pub fn token_literal(&self) -> String {
        self.token.literal()
    }

    pub fn string(&self) -> String {
        let mut s = format!(
            "if {} {}",
            self.condition.string(),
            self.consequence.string(),
        );

        if self.alternative.is_some() {
            s.push_str(&format!(
                "else {}",
                self.alternative.as_ref().unwrap().string()
            ));
        }

        s
    }
}

#[derive(Debug, Clone)]
pub(crate) struct InfixExpression {
    pub token: Token,
    pub left: Box<Expression>,
    pub operator: String,
    pub right: Box<Expression>,
}

impl InfixExpression {
    pub fn token_literal(&self) -> String {
        self.token.literal()
    }

    pub fn string(&self) -> String {
        format!(
            "({} {} {})",
            self.left.string(),
            self.operator,
            self.right.string()
        )
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
pub(crate) struct Boolean {
    pub token: Token,
    pub value: bool,
}

impl Boolean {
    pub fn token_literal(&self) -> String {
        self.token.literal()
    }

    pub fn string(&self) -> String {
        self.token.literal()
    }
}

#[derive(Debug, Clone)]
pub(crate) struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
}

impl FunctionLiteral {
    pub fn token_literal(&self) -> String {
        self.token.literal()
    }

    pub fn string(&self) -> String {
        let params = self.parameters.iter().map(Identifier::string).join(", ");
        format!(
            "{}({}) {}",
            self.token_literal(),
            params,
            self.body.string()
        )
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
        // TODO - check this - moliva - 2024/03/04
        // self.statements.iter().map(|s| s.string()).join("\n")
        self.statements.iter().map(|s| s.string()).join("")
    }
}
