use crate::ast::ast::{
    ArrayLiteral, BlockStatement, Expression, ExpressionStatement, FunctionLiteral, HashLiteral,
    IfExpression, IndexOperator, InfixExpression, Let, Node, PrefixExpression, Program, Return,
    Statement,
};

macro_rules! modify_match_err_format {
    () => {
        "could not match result of modify for value {} as {}"
    };
}

pub(crate) use modify_match_err_format;

/// Wraps a node value into the Node enum, applies a modify and unwraps it to the original value
/// type afterwards.
macro_rules! modify_match {
    ($e:ident as Expression, $modifier:expr) => {
        match crate::ast::modify::modify(crate::ast::ast::Node::Expression($e), $modifier) {
            crate::ast::ast::Node::Expression(e) => e,
            _ => panic!(
                crate::ast::modify::modify_match_err_format!(),
                stringify!($e),
                "Expression"
            ),
        }
    };
    ($e:ident as Statement, $modifier:expr) => {
        match crate::ast::modify::modify(crate::ast::ast::Node::Statement($e), $modifier) {
            crate::ast::ast::Node::Statement(e) => e,
            _ => panic!(
                crate::ast::modify::modify_match_err_format!(),
                stringify!($e),
                "Statement"
            ),
        }
    };
    ($e:ident as Program, $modifier:expr) => {
        match crate::ast::modify::modify(crate::ast::ast::Node::Program($e), $modifier) {
            crate::ast::ast::Node::Program(e) => e,
            _ => panic!(
                crate::ast::modify::modify_match_err_format!(),
                stringify!($e),
                "Program"
            ),
        }
    };
    ($e:ident as Block, $modifier:expr) => {
        match crate::ast::modify::modify(
            crate::ast::ast::Node::Statement(crate::ast::ast::Statement::Block($e)),
            $modifier,
        ) {
            crate::ast::ast::Node::Statement(crate::ast::ast::Statement::Block(e)) => e,
            _ => panic!(
                crate::ast::modify::modify_match_err_format!(),
                stringify!($e),
                "Block"
            ),
        }
    };
    ($e:ident as Identifier, $modifier:expr) => {
        match crate::ast::modify::modify(
            crate::ast::ast::Node::Expression(crate::ast::ast::Expression::Identifier($e)),
            $modifier,
        ) {
            crate::ast::ast::Node::Expression(crate::ast::ast::Expression::Identifier(p)) => p,
            _ => panic!(
                crate::ast::modify::modify_match_err_format!(),
                stringify!($e),
                "Identifier"
            ),
        }
    };
}

pub(crate) use modify_match;

use super::ast::CallExpression;

pub(crate) type ModifierFn<'a> = Box<dyn Fn(Node) -> Node + 'a>;

// TODO - check the error handling in code example - moliva - 2024/03/27

pub(crate) fn modify(node: Node, modifier: &ModifierFn) -> Node {
    let node = match node {
        Node::Program(Program { statements }) => Node::Program(Program {
            statements: statements
                .into_iter()
                .map(|s| modify_match!(s as Statement, modifier))
                .collect(),
        }),
        Node::Statement(s) => Node::Statement(match s {
            Statement::Expression(ExpressionStatement {
                expression: e,
                token,
            }) => Statement::Expression(ExpressionStatement {
                token,
                expression: modify_match!(e as Expression, modifier),
            }),
            Statement::Block(BlockStatement { token, statements }) => {
                Statement::Block(BlockStatement {
                    token,
                    statements: statements
                        .into_iter()
                        .map(|s| modify_match!(s as Statement, modifier))
                        .collect(),
                })
            }
            Statement::Return(Return {
                token,
                return_value,
            }) => Statement::Return(Return {
                token,
                return_value: modify_match!(return_value as Expression, modifier),
            }),
            Statement::Let(Let { token, name, value }) => Statement::Let(Let {
                token,
                name,
                value: modify_match!(value as Expression, modifier),
            }),
        }),
        Node::Expression(expression) => Node::Expression(match expression {
            Expression::Infix(InfixExpression {
                token,
                left,
                operator,
                right,
            }) => Expression::Infix(InfixExpression {
                token,
                operator,
                left: Box::new({
                    let l = *left;
                    modify_match!(l as Expression, modifier)
                }),
                right: Box::new({
                    let r = *right;
                    modify_match!(r as Expression, modifier)
                }),
            }),
            Expression::Prefix(PrefixExpression {
                token,
                operator,
                right,
            }) => Expression::Prefix(PrefixExpression {
                token,
                operator,
                right: Box::new({
                    let r = *right;
                    modify_match!(r as Expression, modifier)
                }),
            }),
            Expression::IndexOperator(IndexOperator { token, left, index }) => {
                Expression::IndexOperator(IndexOperator {
                    token,
                    left: Box::new({
                        let l = *left;
                        modify_match!(l as Expression, modifier)
                    }),
                    index: Box::new({
                        let i = *index;
                        modify_match!(i as Expression, modifier)
                    }),
                })
            }
            Expression::If(IfExpression {
                token,
                condition,
                consequence,
                alternative,
            }) => Expression::If(IfExpression {
                token,
                condition: Box::new({
                    let l = *condition;
                    modify_match!(l as Expression, modifier)
                }),
                consequence: modify_match!(consequence as Block, modifier),
                alternative: alternative.map(|a| modify_match!(a as Block, modifier)),
            }),
            Expression::FunctionLiteral(FunctionLiteral {
                token,
                parameters,
                body,
            }) => Expression::FunctionLiteral(FunctionLiteral {
                token,
                parameters: parameters
                    .into_iter()
                    .map(|p| modify_match!(p as Identifier, modifier))
                    .collect(),
                body: modify_match!(body as Block, modifier),
            }),
            Expression::ArrayLiteral(ArrayLiteral { token, elements }) => {
                Expression::ArrayLiteral(ArrayLiteral {
                    token,
                    elements: elements
                        .into_iter()
                        .map(|e| modify_match!(e as Expression, modifier))
                        .collect(),
                })
            }
            Expression::HashLiteral(HashLiteral { token, pairs }) => {
                Expression::HashLiteral(HashLiteral {
                    token,
                    pairs: pairs
                        .into_iter()
                        .map(|(k, v)| {
                            (
                                modify_match!(k as Expression, modifier),
                                modify_match!(v as Expression, modifier),
                            )
                        })
                        .collect(),
                })
            }
            Expression::Call(CallExpression {
                token,
                function,
                arguments,
            }) => Expression::Call(CallExpression {
                token,
                function,
                arguments: arguments
                    .into_iter()
                    .map(|v| modify_match!(v as Expression, modifier))
                    .collect(),
            }),
            s => s,
        }),
    };

    modifier(node)
}

#[cfg(test)]
mod test {
    use crate::{
        ast::ast::{
            ArrayLiteral, BlockStatement, Expression, ExpressionStatement, FunctionLiteral,
            HashLiteral, Identifier, IfExpression, IndexOperator, InfixExpression, IntegerLiteral,
            Let, Node, PrefixExpression, Program, Return, Statement,
        },
        token::Token,
    };

    use super::*;

    #[test]
    fn test_modify() {
        let one = || {
            Expression::IntegerLiteral(IntegerLiteral {
                token: Token::Lt, // shrug
                value: 1,
            })
        };
        let two = || {
            Expression::IntegerLiteral(IntegerLiteral {
                token: Token::Lt, // shrug
                value: 2,
            })
        };

        let turn_one_into_two: ModifierFn = Box::new(|node: Node| {
            let mut integer = match node {
                Node::Expression(Expression::IntegerLiteral(n)) => n,
                n => return n,
            };

            assert_eq!(integer.value, 1);
            integer.value = 2;

            Node::Expression(Expression::IntegerLiteral(integer))
        });

        let tests = [
            (one().into_node(), Node::Expression(two())),
            (
                Node::Program(Program {
                    statements: vec![Statement::Expression(ExpressionStatement {
                        token: Token::Lt, // shrug
                        expression: one(),
                    })],
                }),
                Node::Program(Program {
                    statements: vec![Statement::Expression(ExpressionStatement {
                        token: Token::Lt, // shrug
                        expression: two(),
                    })],
                }),
            ),
            (
                Node::Expression(Expression::Infix(InfixExpression {
                    token: Token::Lt,
                    left: Box::new(one()),
                    operator: "+".to_owned(),
                    right: Box::new(one()),
                })),
                Node::Expression(Expression::Infix(InfixExpression {
                    token: Token::Lt,
                    left: Box::new(two()),
                    operator: "+".to_owned(),
                    right: Box::new(two()),
                })),
            ),
            (
                Node::Expression(Expression::Prefix(PrefixExpression {
                    token: Token::Lt,
                    operator: "-".to_owned(),
                    right: Box::new(one()),
                })),
                Node::Expression(Expression::Prefix(PrefixExpression {
                    token: Token::Lt,
                    operator: "-".to_owned(),
                    right: Box::new(two()),
                })),
            ),
            (
                Node::Expression(Expression::IndexOperator(IndexOperator {
                    token: Token::Lt,
                    left: Box::new(one()),
                    index: Box::new(one()),
                })),
                Node::Expression(Expression::IndexOperator(IndexOperator {
                    token: Token::Lt,
                    left: Box::new(two()),
                    index: Box::new(two()),
                })),
            ),
            (
                Node::Expression(Expression::If(IfExpression {
                    token: Token::Lt,
                    condition: Box::new(one()),
                    consequence: BlockStatement {
                        token: Token::Lt,
                        statements: vec![Statement::Expression(ExpressionStatement {
                            token: Token::Lt,
                            expression: one(),
                        })],
                    },
                    alternative: Some(BlockStatement {
                        token: Token::Lt,
                        statements: vec![Statement::Expression(ExpressionStatement {
                            token: Token::Lt,
                            expression: one(),
                        })],
                    }),
                })),
                Node::Expression(Expression::If(IfExpression {
                    token: Token::Lt,
                    condition: Box::new(two()),
                    consequence: BlockStatement {
                        token: Token::Lt,
                        statements: vec![Statement::Expression(ExpressionStatement {
                            token: Token::Lt,
                            expression: two(),
                        })],
                    },
                    alternative: Some(BlockStatement {
                        token: Token::Lt,
                        statements: vec![Statement::Expression(ExpressionStatement {
                            token: Token::Lt,
                            expression: two(),
                        })],
                    }),
                })),
            ),
            (
                Node::Statement(Statement::Return(Return {
                    token: Token::Lt,
                    return_value: one(),
                })),
                Node::Statement(Statement::Return(Return {
                    token: Token::Lt,
                    return_value: two(),
                })),
            ),
            (
                Node::Statement(Statement::Let(Let {
                    token: Token::Lt,
                    name: Identifier {
                        token: Token::Lt,
                        value: "some".to_owned(),
                    },
                    value: one(),
                })),
                Node::Statement(Statement::Let(Let {
                    token: Token::Lt,
                    name: Identifier {
                        token: Token::Lt,
                        value: "some".to_owned(),
                    },
                    value: two(),
                })),
            ),
            (
                Node::Expression(Expression::FunctionLiteral(FunctionLiteral {
                    token: Token::Lt,
                    parameters: Vec::default(),
                    body: BlockStatement {
                        token: Token::Lt,
                        statements: vec![Statement::Expression(ExpressionStatement {
                            token: Token::Lt,
                            expression: one(),
                        })],
                    },
                })),
                Node::Expression(Expression::FunctionLiteral(FunctionLiteral {
                    token: Token::Lt,
                    parameters: Vec::default(),
                    body: BlockStatement {
                        token: Token::Lt,
                        statements: vec![Statement::Expression(ExpressionStatement {
                            token: Token::Lt,
                            expression: two(),
                        })],
                    },
                })),
            ),
            (
                Node::Expression(Expression::ArrayLiteral(ArrayLiteral {
                    token: Token::Lt,
                    elements: vec![one(), one()],
                })),
                Node::Expression(Expression::ArrayLiteral(ArrayLiteral {
                    token: Token::Lt,
                    elements: vec![two(), two()],
                })),
            ),
            (
                Expression::HashLiteral(HashLiteral {
                    token: Token::Lt,
                    pairs: vec![(one(), one()), (one(), one())],
                })
                .into_node(),
                Expression::HashLiteral(HashLiteral {
                    token: Token::Lt,
                    pairs: vec![(two(), two()), (two(), two())],
                })
                .into_node(),
            ),
        ];

        for (input, expected) in tests {
            let modified = modify(input, &turn_one_into_two);

            assert_eq!(modified, expected);
        }
    }
}
