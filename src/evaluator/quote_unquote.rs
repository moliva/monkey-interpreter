use crate::{
    ast::{
        ast::{
            ArrayLiteral, Boolean, Expression, FunctionLiteral, HashLiteral, IntegerLiteral,
            MacroLiteral, Node, StringLiteral,
        },
        modify::{modify, ModifierFn},
    },
    evaluator::evaluator::eval,
    object::{BuiltinFunction, Function, Hash, Macro, Object, Quote, SharedEnvironment},
    token::Token,
};

pub(crate) fn quote(node: Node, env: &SharedEnvironment) -> Object {
    let node = eval_unquote_calls(node, env);

    Object::Quote(Quote(node))
}

fn eval_unquote_calls(node: Node, env: &SharedEnvironment) -> Node {
    let modifier: ModifierFn = Box::new(|n| match n {
        Node::Expression(Expression::Call(mut call))
            if call.function.token_literal() == "unquote" && call.arguments.len() == 1 =>
        {
            let evaluated = eval(&Node::Expression(call.arguments.pop().unwrap()), env);

            convert_object_to_ast_node(evaluated)
        }
        n => n,
    });

    modify(node, &modifier)
}

fn convert_wrapping_to_expression(e: Object) -> Expression {
    match convert_object_to_ast_node(e) {
        Node::Expression(e) => e,
        _ => panic!("expected expression"),
    }
}

fn convert_object_to_ast_node(object: Object) -> Node {
    // TODO - complete the missing object variants - moliva - 2024/03/27
    Node::Expression(match object {
        Object::Integer(value) => Expression::IntegerLiteral(IntegerLiteral {
            token: Token::Int(value.to_string()),
            value,
        }),
        Object::Boolean(value) => Expression::Boolean(Boolean {
            token: Token::Ident(value.to_string()),
            value,
        }),
        Object::String(value) => Expression::StringLiteral(StringLiteral {
            token: Token::String(value.clone()),
            value,
        }),
        Object::Array(array) => Expression::ArrayLiteral(ArrayLiteral {
            token: Token::LBracket,
            elements: array
                .into_iter()
                .map(convert_wrapping_to_expression)
                .collect(),
        }),
        Object::Hash(Hash(elements)) => Expression::HashLiteral(HashLiteral {
            token: Token::LBrace,
            pairs: elements
                .into_iter()
                .map(|(k, v)| {
                    (
                        convert_wrapping_to_expression(k),
                        convert_wrapping_to_expression(v),
                    )
                })
                .collect::<Vec<_>>(),
        }),
        Object::Function(Function {
            parameters, body, ..
        }) => Expression::FunctionLiteral(FunctionLiteral {
            token: Token::Function,
            parameters,
            body,
        }),
        Object::Macro(Macro {
            parameters, body, ..
        }) => Expression::MacroLiteral(MacroLiteral {
            token: Token::Macro,
            parameters,
            body,
        }),
        Object::BuiltinFunction(_builtin) => {
            todo!("this should resolve to the identifier of the builtin function")
        }
        Object::ReturnValue(_) => panic!("only supporting expressions at the moment"),
        Object::Error(_) => panic!("error in macro!"),
        Object::Null => todo!("implement node for null"),
        Object::Quote(Quote(n)) => return n,
    })
}

#[cfg(test)]
mod test {
    use crate::test_utils::{match_or_fail, test_eval};

    use super::*;

    #[test]
    fn test_quote() {
        let tests = [
            ("quote(5)", "5"),
            ("quote(5 + 8)", "(5 + 8)"),
            ("quote(foobar)", "foobar"),
            ("quote(foobar + barfoo)", "(foobar + barfoo)"),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);

            let Quote(node) = match_or_fail!(evaluated, Object::Quote(q) => q);

            assert_eq!(node.string(), expected);
        }
    }

    #[test]
    fn test_quote_unquote() {
        let tests = [
            ("quote(unquote(4))", "4"),
            ("quote(unquote(4 + 4))", "8"),
            ("quote(8 + unquote(4 + 4))", "(8 + 8)"),
            ("quote(unquote(4 + 4) + 8)", "(8 + 8)"),
            (
                r#"let foobar = 8;
             quote(foobar)"#,
                "foobar",
            ),
            (
                r#"let foobar = 8;
             quote(unquote(foobar))"#,
                "8",
            ),
            ("quote(unquote(true))", "true"),
            ("quote(unquote(false))", "false"),
            ("quote(unquote(quote(4 + 4)))", "(4 + 4)"),
            (
                r#"let quotedInfixExpression = quote(4 + 4);
             quote(unquote(4 + 4) + unquote(quotedInfixExpression))"#,
                "(8 + (4 + 4))",
            ),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);

            let Quote(node) = match_or_fail!(evaluated, Object::Quote(q) => q);

            assert_eq!(node.string(), expected);
        }
    }
}
