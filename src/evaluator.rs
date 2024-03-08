use crate::{
    ast::{Node, Statement},
    object::{Boolean, Integer, Object},
};

pub(crate) static TRUE: Object = Object::Boolean(Boolean { value: true });
pub(crate) static FALSE: Object = Object::Boolean(Boolean { value: false });

pub(crate) fn native_bool_to_boolean_object(input: bool) -> Object {
    // TODO - avoid cloning - moliva - 2024/03/08
    if input {
        TRUE.clone()
    } else {
        FALSE.clone()
    }
}

pub(crate) fn eval(node: &Node) -> Object {
    use Node::*;

    match node {
        Program(p) => eval_statements(&p.statements),
        Statement(s) => match s {
            crate::ast::Statement::Let(_) => todo!(),
            crate::ast::Statement::Return(_) => todo!(),
            crate::ast::Statement::Expression(e) => eval(&Expression(e.expression.clone())),
            crate::ast::Statement::Block(_) => todo!(),
        },
        Expression(e) => match e {
            crate::ast::Expression::Identifier(_) => todo!(),
            crate::ast::Expression::IntegerLiteral(i) => {
                Object::Integer(Integer { value: i.value })
            }
            crate::ast::Expression::Boolean(i) => native_bool_to_boolean_object(i.value),
            crate::ast::Expression::PrefixExpression(e) => {
                let right = eval(&Node::Expression(*e.right.clone()));
                eval_prefix_expression(&e.operator, &right)
            }
            crate::ast::Expression::InfixExpression(e) => {
                let right = eval(&Node::Expression(*e.right.clone()));
                let left = eval(&Node::Expression(*e.left.clone()));
                eval_infix_expression(&e.operator, &left, &right)
            }
            crate::ast::Expression::IfExpression(_) => todo!(),
            crate::ast::Expression::FunctionLiteral(_) => todo!(),
            crate::ast::Expression::CallExpression(_) => todo!(),
        },
    }
}

fn eval_infix_expression(operator: &str, left: &Object, right: &Object) -> Object {
    if let Object::Integer(Integer { value: left }) = left {
        if let Object::Integer(Integer { value: right }) = right {
            return eval_integer_infix_expression(operator, left, right);
        }
    }

    match operator {
        "==" => Object::Boolean(Boolean {
            value: left == right,
        }),
        "!=" => Object::Boolean(Boolean {
            value: left != right,
        }),
        _ => Object::Null,
    }
}

fn eval_integer_infix_expression(operator: &str, left: &i64, right: &i64) -> Object {
    match operator {
        "+" => Object::Integer(Integer {
            value: left + right,
        }),
        "-" => Object::Integer(Integer {
            value: left - right,
        }),
        "*" => Object::Integer(Integer {
            value: left * right,
        }),
        "/" => Object::Integer(Integer {
            value: left / right,
        }),
        "<" => Object::Boolean(Boolean {
            value: left < right,
        }),
        ">" => Object::Boolean(Boolean {
            value: left > right,
        }),
        "==" => Object::Boolean(Boolean {
            value: left == right,
        }),
        "!=" => Object::Boolean(Boolean {
            value: left != right,
        }),
        _ => Object::Null,
    }
}

fn eval_prefix_expression(operator: &str, right: &Object) -> Object {
    match operator {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => Object::Null,
    }
}

fn eval_minus_prefix_operator_expression(right: &Object) -> Object {
    if let Object::Integer(Integer { value }) = right {
        Object::Integer(Integer { value: -value })
    } else {
        Object::Null
    }
}

fn eval_bang_operator_expression(right: &Object) -> Object {
    match right {
        Object::Boolean(Boolean { value: true }) => FALSE.clone(),
        Object::Boolean(Boolean { value: false }) => TRUE.clone(),
        Object::Null => TRUE.clone(),
        _ => FALSE.clone(),
    }
}

fn eval_statements(statements: &[Statement]) -> Object {
    let mut result = None;

    for statement in statements {
        result = Some(eval(&Node::Statement(statement.clone())));
    }

    result.unwrap()
}

#[cfg(test)]
mod test {
    use crate::{lexer::Lexer, parser::Parser};

    use super::*;

    #[test]
    fn test_eval_integer_expression() {
        let tests = [
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);

            test_integer_object(&evaluated, expected);
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
        let tests = [
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);

            test_boolean_object(&evaluated, expected);
        }
    }

    #[test]
    fn test_bang_operator() {
        let tests = [
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
        ];

        for (input, expected_value) in tests {
            let evaluated = test_eval(input);
            test_boolean_object(&evaluated, expected_value);
        }
    }

    // *****************************************************************************************************
    // *************** Helpers ***************
    // *****************************************************************************************************

    fn test_eval(input: &str) -> Object {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        eval(&Node::Program(program))
    }

    fn test_boolean_object(evaluated: &Object, expected: bool) {
        let result = match evaluated {
            Object::Boolean(i) => i,
            _ => panic!("object is not Boolean. got={:?}", evaluated),
        };

        assert_eq!(result.value, expected);
    }

    fn test_integer_object(evaluated: &Object, expected: i64) {
        let result = match evaluated {
            Object::Integer(i) => i,
            _ => panic!("object is not Integer. got={:?}", evaluated),
        };

        assert_eq!(result.value, expected);
    }
}
