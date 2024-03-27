use crate::ast::ast::{
    ArrayLiteral, BlockStatement, Expression, ExpressionStatement, FunctionLiteral, HashLiteral,
    IfExpression, IndexOperator, InfixExpression, Let, Node, PrefixExpression, Program, Return,
    Statement,
};

pub(crate) type ModifierFn<'a> = Box<dyn Fn(Node) -> Node + 'a>;

// TODO - check the error handling in code example - moliva - 2024/03/27

pub(crate) fn modify(node: Node, modifier: &ModifierFn) -> Node {
    let node = match node {
        Node::Program(Program { statements }) => Node::Program(Program {
            statements: statements
                .into_iter()
                .map(|s| match modify(Node::Statement(s), modifier) {
                    Node::Statement(e) => e,
                    _ => panic!(),
                })
                .collect(),
        }),
        Node::Statement(s) => Node::Statement(match s {
            Statement::Expression(ExpressionStatement {
                expression: e,
                token,
            }) => Statement::Expression(match modify(Node::Expression(e), modifier) {
                Node::Expression(e) => ExpressionStatement {
                    expression: e,
                    token,
                },
                _ => panic!(),
            }),
            Statement::Block(BlockStatement { token, statements }) => {
                Statement::Block(BlockStatement {
                    token,
                    statements: statements
                        .into_iter()
                        .map(|s| match modify(Node::Statement(s), modifier) {
                            Node::Statement(s) => s,
                            _ => panic!(),
                        })
                        .collect(),
                })
            }
            Statement::Return(Return {
                token,
                return_value,
            }) => Statement::Return(Return {
                token,
                return_value: match modify(Node::Expression(return_value), modifier) {
                    Node::Expression(e) => e,
                    _ => panic!(),
                },
            }),
            Statement::Let(Let { token, name, value }) => Statement::Let(Let {
                token,
                name,
                value: match modify(Node::Expression(value), modifier) {
                    Node::Expression(e) => e,
                    _ => panic!(),
                },
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
                left: Box::new(match modify(Node::Expression(*left), modifier) {
                    Node::Expression(e) => e,
                    _ => panic!(),
                }),
                right: Box::new(match modify(Node::Expression(*right), modifier) {
                    Node::Expression(e) => e,
                    _ => panic!(),
                }),
            }),
            Expression::Prefix(PrefixExpression {
                token,
                operator,
                right,
            }) => Expression::Prefix(PrefixExpression {
                token,
                operator,
                right: Box::new(match modify(Node::Expression(*right), modifier) {
                    Node::Expression(e) => e,
                    _ => panic!(),
                }),
            }),
            Expression::IndexOperator(IndexOperator { token, left, index }) => {
                Expression::IndexOperator(IndexOperator {
                    token,
                    left: Box::new(match modify(Node::Expression(*left), modifier) {
                        Node::Expression(e) => e,
                        _ => panic!(),
                    }),
                    index: Box::new(match modify(Node::Expression(*index), modifier) {
                        Node::Expression(e) => e,
                        _ => panic!(),
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
                condition: Box::new(match modify(Node::Expression(*condition), modifier) {
                    Node::Expression(e) => e,
                    _ => panic!(),
                }),
                consequence: match modify(Node::Statement(Statement::Block(consequence)), modifier)
                {
                    Node::Statement(Statement::Block(s)) => s,
                    _ => panic!(),
                },
                alternative: alternative.map(|alternative| {
                    match modify(Node::Statement(Statement::Block(alternative)), modifier) {
                        Node::Statement(Statement::Block(s)) => s,
                        _ => panic!(),
                    }
                }),
            }),
            Expression::FunctionLiteral(FunctionLiteral {
                token,
                parameters,
                body,
            }) => Expression::FunctionLiteral(FunctionLiteral {
                token,
                parameters: parameters
                    .into_iter()
                    .map(
                        |p| match modify(Node::Expression(Expression::Identifier(p)), modifier) {
                            Node::Expression(Expression::Identifier(p)) => p,
                            _ => panic!(),
                        },
                    )
                    .collect(),
                body: match modify(Node::Statement(Statement::Block(body)), modifier) {
                    Node::Statement(Statement::Block(s)) => s,
                    _ => panic!(),
                },
            }),
            Expression::ArrayLiteral(ArrayLiteral { token, elements }) => {
                Expression::ArrayLiteral(ArrayLiteral {
                    token,
                    elements: elements
                        .into_iter()
                        .map(|e| match modify(Node::Expression(e), modifier) {
                            Node::Expression(e) => e,
                            _ => panic!(),
                        })
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
                                match modify(Node::Expression(k), modifier) {
                                    Node::Expression(e) => e,
                                    _ => panic!(),
                                },
                                match modify(Node::Expression(v), modifier) {
                                    Node::Expression(e) => e,
                                    _ => panic!(),
                                },
                            )
                        })
                        .collect(),
                })
            }
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

            integer.value = 2;

            Node::Expression(Expression::IntegerLiteral(integer))
        });

        let tests = [
            (Node::Expression(one()), Node::Expression(two())),
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
                    right: Box::new(two()),
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
                Node::Expression(Expression::HashLiteral(HashLiteral {
                    token: Token::Lt,
                    pairs: vec![(one(), one()), (one(), one())],
                })),
                Node::Expression(Expression::HashLiteral(HashLiteral {
                    token: Token::Lt,
                    pairs: vec![(two(), two()), (two(), two())],
                })),
            ),
        ];

        for (input, expected) in tests {
            let modified = modify(input, &turn_one_into_two);

            assert_eq!(modified, expected);
        }
    }
}
