use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::{
        ast::{
            CallExpression, Expression, Identifier, Let, MacroLiteral, Node, Program, Statement,
        },
        modify::{modify, modify_match, ModifierFn},
    },
    evaluator::evaluator::eval,
    object::{Environment, Macro, Object, Quote, SharedEnvironment},
};

pub(crate) fn define_macros(program: &mut Program, env: &SharedEnvironment) {
    let mut definitions = Vec::default();

    for (i, statement) in program.statements.iter().enumerate() {
        if is_macro_definition(statement) {
            add_macro(statement, env);

            definitions.push(i);
        }
    }

    for index in definitions.into_iter().rev() {
        program.statements.remove(index);
    }
}

fn is_macro_definition(statement: &Statement) -> bool {
    matches!(statement, Statement::Let(Let { value, .. }) if matches!(value, Expression::MacroLiteral(_)))
}

pub fn expand_macros(program: Program, env: &SharedEnvironment) -> Program {
    let modifier: ModifierFn = Box::new(|node| match node {
        Node::Expression(Expression::Call(call)) if option_macro_call(&call, env).is_some() => {
            let Macro {
                parameters,
                body,
                env,
            } = option_macro_call(&call, env).unwrap();

            let args = quote_args(&call);
            let eval_env = extend_macro_env(parameters, env, args);

            let evaluated = eval(&Statement::Block(body).into_node(), &eval_env.into_shared());

            match evaluated {
                Object::Quote(Quote(n)) => n,
                _ => panic!("we only support returning AST-nodes from macros"),
            }
        }
        _ => node,
    });

    modify_match!(program as Program, &modifier)
}

fn extend_macro_env(
    parameters: Vec<Identifier>,
    env: SharedEnvironment,
    args: Vec<Object>,
) -> Environment {
    let mut extended = Environment::enclosed(env);
    for (param, arg) in parameters.iter().zip(args.into_iter()) {
        extended.set(&param.value, arg);
    }

    extended
}

fn quote_args(call: &CallExpression) -> Vec<Object> {
    call.arguments
        .iter()
        .map(|a| Object::Quote(Quote(a.clone().into_node())))
        .collect()
}

fn option_macro_call(expression: &CallExpression, env: &SharedEnvironment) -> Option<Macro> {
    match &*expression.function {
        Expression::Identifier(Identifier { value, .. }) => {
            let object = env.borrow().get(&value);

            match object {
                Some(Object::Macro(m)) => Some(m),
                _ => None,
            }
        }
        _ => None,
    }
}

fn add_macro(statement: &Statement, env: &SharedEnvironment) {
    match statement {
        Statement::Let(Let {
            value:
                Expression::MacroLiteral(MacroLiteral {
                    parameters, body, ..
                }),
            name,
            ..
        }) => {
            let macrou = Object::Macro(Macro {
                parameters: parameters.clone(),
                env: Rc::clone(env),
                body: body.clone(),
            });

            RefCell::borrow_mut(env).set(&name.value, macrou);
        }
        _ => panic!("not a macro!"),
    }
}

#[cfg(test)]
mod tests {
    use crate::{object::Environment, test_utils::test_parse_program};

    use super::*;

    #[test]
    fn test_expand_macros() {
        let tests = [
            (
                r#"
                 let infixExpression = macro() { quote(1 + 2); }

                 infixExpression();
                 "#,
                "(1 + 2)",
            ),
            (
                r#"
                 let reverse = macro(a, b) { quote(unquote(b) - unquote(a)); };
    
                 reverse(2 + 2, 10 - 5);
                 "#,
                "(10 - 5) - (2 + 2)",
            ),
        ];

        for (input, expected) in tests {
            let expected = test_parse_program(expected);
            let mut program = test_parse_program(input);

            let env = Environment::default().into_shared();

            define_macros(&mut program, &env);

            let expanded = expand_macros(program, &env);

            assert_eq!(expanded.string(), expected.string());
        }
    }
}
