use crate::ast::{
    Expression, ExpressionStatement, IndexOperator, InfixExpression, Node, PrefixExpression,
    Program, Statement,
};

pub(crate) type ModifierFn = Box<dyn Fn(&mut Node)>;

pub(crate) fn modify(mut node: Node, modifier: &ModifierFn) -> Node {
    let mut node = match node {
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
            s => s,
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
            s => s,
        }),
    };

    dbg!(&node);
    modifier(&mut node);

    node
}

#[cfg(test)]
mod test {
    use crate::{
        ast::{
            Expression, ExpressionStatement, IndexOperator, InfixExpression, IntegerLiteral, Node,
            PrefixExpression, Program, Statement,
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

        let turn_one_into_two: ModifierFn = Box::new(|node: &mut Node| {
            let mut integer = match node {
                Node::Expression(Expression::IntegerLiteral(n)) => n,
                _ => return,
            };

            integer.value = 2;
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
        ];

        for (input, expected) in tests {
            let modified = modify(input, &turn_one_into_two);

            assert_eq!(modified, expected);
        }
    }
}
