use crate::{
    ast::{BlockStatement, IfExpression, Node, Program, Statement},
    object::{Boolean, Integer, Object},
};

pub(crate) fn eval(node: &Node) -> Object {
    use Node::*;

    match node {
        Program(p) => eval_program(p),
        Statement(s) => match s {
            crate::ast::Statement::Let(_) => todo!(),
            crate::ast::Statement::Return(e) => {
                let val = eval(&Node::Expression(e.return_value.clone()));
                if val.is_error() {
                    return val;
                }

                Object::ReturnValue(Box::new(val))
            }
            crate::ast::Statement::Expression(e) => eval(&Expression(e.expression.clone())),
            crate::ast::Statement::Block(e) => eval_block_statement(e),
        },
        Expression(e) => match e {
            crate::ast::Expression::Identifier(_) => todo!(),
            crate::ast::Expression::IntegerLiteral(i) => Object::Integer(Integer(i.value)),
            crate::ast::Expression::Boolean(i) => Object::Boolean(Boolean(i.value)),

            crate::ast::Expression::Prefix(e) => {
                let right = eval(&Node::Expression(*e.right.clone()));

                if right.is_error() {
                    return right;
                }
                eval_prefix_expression(&e.operator, &right)
            }
            crate::ast::Expression::Infix(e) => {
                let left = eval(&Node::Expression(*e.left.clone()));
                if left.is_error() {
                    return left;
                }
                let right = eval(&Node::Expression(*e.right.clone()));
                if right.is_error() {
                    return right;
                }
                eval_infix_expression(&e.operator, &left, &right)
            }
            crate::ast::Expression::If(e) => eval_if_expression(e),
            crate::ast::Expression::FunctionLiteral(_) => todo!(),
            crate::ast::Expression::Call(_) => todo!(),
        },
    }
}

fn eval_if_expression(e: &IfExpression) -> Object {
    let condition = eval(&Node::Expression(*e.condition.clone()));
    if condition.is_error() {
        return condition;
    }

    if is_truthy(&condition) {
        eval(&Node::Statement(Statement::Block(e.consequence.clone())))
    } else if let Some(alternative) = &e.alternative {
        eval(&Node::Statement(Statement::Block(alternative.clone())))
    } else {
        Object::Null
    }
}

fn is_truthy(obj: &Object) -> bool {
    match obj {
        Object::Null => false,
        Object::Boolean(Boolean(v)) => *v,
        _ => true,
    }
}

fn eval_infix_expression(operator: &str, left: &Object, right: &Object) -> Object {
    if let Object::Integer(Integer(left)) = left {
        if let Object::Integer(Integer(right)) = right {
            return eval_integer_infix_expression(operator, left, right);
        }
    }

    if std::mem::discriminant(left) != std::mem::discriminant(right) {
        return Object::Error(format!(
            "type mismatch: {} {operator} {}",
            left.r#type(),
            right.r#type()
        ));
    }

    match operator {
        "==" => Object::Boolean(Boolean(left == right)),
        "!=" => Object::Boolean(Boolean(left != right)),
        _ => Object::Error(format!(
            "unknown operator: {} {operator} {}",
            left.r#type(),
            right.r#type()
        )),
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
        _ => Object::Error(format!("unkown operator: INTEGER {operator} INTEGER")),
    }
}

fn eval_prefix_expression(operator: &str, right: &Object) -> Object {
    match operator {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => Object::Error(format!("unknown operator: {operator}{}", right.r#type())),
    }
}

fn eval_minus_prefix_operator_expression(right: &Object) -> Object {
    if let Object::Integer(Integer(value)) = right {
        Object::Integer(Integer(-value))
    } else {
        Object::Error(format!("unknown operator: -{}", right.r#type()))
    }
}

fn eval_bang_operator_expression(right: &Object) -> Object {
    let val = is_truthy(right);

    Object::Boolean(Boolean(!val))
}

fn eval_block_statement(block: &BlockStatement) -> Object {
    let statements = &block.statements;

    let mut result = None;

    for statement in statements {
        result = Some(eval(&Node::Statement(statement.clone())));

        if let Some(rv @ Object::ReturnValue(_)) = result {
            return rv;
        } else if let Some(e @ Object::Error(_)) = result {
            return e;
        }
    }

    result.unwrap()
}

fn eval_program(program: &Program) -> Object {
    let statements = &program.statements;

    let mut result = None;

    for statement in statements {
        result = Some(eval(&Node::Statement(statement.clone())));

        if let Some(Object::ReturnValue(val)) = &result {
            return *val.clone();
        } else if let Some(r @ Object::Error(_)) = &result {
            return r.clone();
        }
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
    fn test_error_handling() {
        let tests = [
            ("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
            ("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
            ("-true", "unknown operator: -BOOLEAN"),
            ("true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
            ("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN"),
            (
                "if (10 > 1) { true + false; }",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            (
                r#"if (10 > 1) {
                    if (10 > 1) {
                        return true + false;
                    }

                    return 1;
                  } "#,
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
        ];

        for (input, expected_error) in tests {
            let evaluated = test_eval(input);

            if let Object::Error(message) = evaluated {
                assert_eq!(message, expected_error);
            } else {
                panic!("expected error from {input}, got {evaluated:?}");
            }
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
    fn test_return_statements() {
        let tests = [
            ("return 10;", 10),
            ("return 10; 9;", 10),
            ("return 2 * 5; 9;", 10),
            ("9; return 2 * 5; 9;", 10),
            (
                r#"if (10 > 1) {
  if (10 > 1) {
    return 10;
  }

  return 1;
}"#,
                10,
            ),
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
