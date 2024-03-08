use crate::{
    ast::{IfExpression, Node, Statement},
    object::{Boolean, Integer, Object},
};

pub(crate) fn eval(node: &Node) -> Object {
    use Node::*;

    match node {
        Program(p) => eval_statements(&p.statements),
        Statement(s) => match s {
            crate::ast::Statement::Let(_) => todo!(),
            crate::ast::Statement::Return(_) => todo!(),
            crate::ast::Statement::Expression(e) => eval(&Expression(e.expression.clone())),
            crate::ast::Statement::Block(e) => eval_statements(&e.statements),
        },
        Expression(e) => match e {
            crate::ast::Expression::Identifier(_) => todo!(),
            crate::ast::Expression::IntegerLiteral(i) => Object::Integer(Integer(i.value)),
            crate::ast::Expression::Boolean(i) => Object::Boolean(Boolean(i.value)),

            crate::ast::Expression::PrefixExpression(e) => {
                let right = eval(&Node::Expression(*e.right.clone()));
                eval_prefix_expression(&e.operator, &right)
            }
            crate::ast::Expression::InfixExpression(e) => {
                let right = eval(&Node::Expression(*e.right.clone()));
                let left = eval(&Node::Expression(*e.left.clone()));
                eval_infix_expression(&e.operator, &left, &right)
            }
            crate::ast::Expression::IfExpression(e) => eval_if_expression(e),
            crate::ast::Expression::FunctionLiteral(_) => todo!(),
            crate::ast::Expression::CallExpression(_) => todo!(),
        },
    }
}

fn eval_if_expression(e: &IfExpression) -> Object {
    let condition = eval(&Node::Expression(*e.condition.clone()));

    if is_truthy(condition) {
        eval(&Node::Statement(Statement::Block(e.consequence.clone())))
    } else if let Some(alternative) = &e.alternative {
        eval(&Node::Statement(Statement::Block(alternative.clone())))
    } else {
        Object::Null
    }
}

fn is_truthy(obj: Object) -> bool {
    match obj {
        Object::Null => false,
        Object::Boolean(Boolean(v)) => v,
        _ => true,
    }
}

fn eval_infix_expression(operator: &str, left: &Object, right: &Object) -> Object {
    if let Object::Integer(Integer(left)) = left {
        if let Object::Integer(Integer(right)) = right {
            return eval_integer_infix_expression(operator, left, right);
        }
    }

    match operator {
        "==" => Object::Boolean(Boolean(left == right)),
        "!=" => Object::Boolean(Boolean(left != right)),
        _ => Object::Null,
    }
}

fn eval_integer_infix_expression(operator: &str, left: &i64, right: &i64) -> Object {
    match operator {
        "+" => Object::Integer(Integer(left + right)),
        "-" => Object::Integer(Integer(left - right)),
        "*" => Object::Integer(Integer(left * right)),
        "/" => Object::Integer(Integer(left / right)),
        "<" => Object::Boolean(Boolean(left < right)),
        ">" => Object::Boolean(Boolean(left > right)),
        "==" => Object::Boolean(Boolean(left == right)),
        "!=" => Object::Boolean(Boolean(left != right)),
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
    if let Object::Integer(Integer(value)) = right {
        Object::Integer(Integer(-value))
    } else {
        Object::Null
    }
}

fn eval_bang_operator_expression(right: &Object) -> Object {
    let val = match right {
        Object::Boolean(Boolean(v)) => !v,
        Object::Null => true,
        _ => false,
    };

    Object::Boolean(Boolean(val))
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
    fn test_if_else_expressions() {
        let tests = [
            ("if (true) { 10 }", Object::Integer(Integer(10))),
            ("if (false) { 10 }", Object::Null),
            ("if (1) { 10 }", Object::Integer(Integer(10))),
            ("if (1 < 2) { 10 }", Object::Integer(Integer(10))),
            ("if (1 > 2) { 10 }", Object::Null),
            (
                "if (1 > 2) { 10 } else { 20 }",
                Object::Integer(Integer(20)),
            ),
            (
                "if (1 < 2) { 10 } else { 20 }",
                Object::Integer(Integer(10)),
            ),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);

            assert_eq!(evaluated, expected);
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

        assert_eq!(result.0, expected);
    }

    fn test_integer_object(evaluated: &Object, expected: i64) {
        let result = match evaluated {
            Object::Integer(i) => i,
            _ => panic!("object is not Integer. got={:?}", evaluated),
        };

        assert_eq!(result.0, expected);
    }
}
