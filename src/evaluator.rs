use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    ast::{self, BlockStatement, HashLiteral, Identifier, IfExpression, Node, Program, Statement},
    builtins::BUILTINS,
    object::{Environment, Function, Hash, Object, SharedEnvironment},
    quote_unquote::quote,
};

macro_rules! eval_and_return_if_error {
    ($e:expr, $env:expr) => {{
        let result = crate::evaluator::eval($e, $env);

        if result.is_error() {
            return result;
        }

        result
    }};
}

pub(crate) fn eval(node: &Node, env: &SharedEnvironment) -> Object {
    use Node::*;

    match node {
        Program(p) => eval_program(p, env),
        Statement(s) => match s {
            ast::Statement::Let(e) => {
                let val = eval_and_return_if_error!(&Node::Expression(e.value.clone()), env);

                env.borrow_mut().set(&e.name.value, val.clone());

                val
            }
            ast::Statement::Return(e) => {
                let val = eval_and_return_if_error!(&Node::Expression(e.return_value.clone()), env);

                Object::ReturnValue(Box::new(val))
            }
            ast::Statement::Expression(e) => eval(&Expression(e.expression.clone()), env),
            ast::Statement::Block(e) => eval_block_statement(e, env),
        },
        Expression(e) => match e {
            ast::Expression::Identifier(e) => eval_identifier(e, env),
            ast::Expression::IntegerLiteral(i) => Object::Integer(i.value),
            ast::Expression::Boolean(i) => Object::Boolean(i.value),

            ast::Expression::Prefix(e) => {
                let right = eval_and_return_if_error!(&Node::Expression(*e.right.clone()), env);

                eval_prefix_expression(&e.operator, &right)
            }
            ast::Expression::Infix(e) => {
                let left = eval_and_return_if_error!(&Node::Expression(*e.left.clone()), env);
                let right = eval_and_return_if_error!(&Node::Expression(*e.right.clone()), env);

                eval_infix_expression(&e.operator, &left, &right)
            }
            ast::Expression::If(e) => eval_if_expression(e, env),
            ast::Expression::FunctionLiteral(f) => {
                let parameters = f.parameters.clone();
                let body = f.body.clone();
                let env = Rc::clone(env);

                Object::Function(Function {
                    parameters,
                    body,
                    env,
                })
            }
            ast::Expression::Call(e) => {
                // special case for the `quote` macro call that requires the ast prior to its
                // evaluation
                if &*e.function.token_literal() == "quote" {
                    return quote(Node::Expression(e.arguments[0].clone()), env);
                }

                let function =
                    eval_and_return_if_error!(&Node::Expression(*e.function.clone()), env);

                let args = eval_expressions(&e.arguments, env);
                if args.len() == 1 && args[0].is_error() {
                    return args[0].clone();
                }

                apply_function(function, args)
            }
            ast::Expression::StringLiteral(e) => Object::String(e.value.clone()),
            ast::Expression::ArrayLiteral(e) => {
                let elements = eval_expressions(&e.elements, env);

                if elements.len() == 1 && elements[0].is_error() {
                    return elements[0].clone();
                }

                Object::Array(elements)
            }
            ast::Expression::IndexOperator(e) => {
                let left = eval_and_return_if_error!(&Node::Expression(*e.left.clone()), env);
                let index = eval_and_return_if_error!(&Node::Expression(*e.index.clone()), env);

                eval_index_expression(left, index)
            }
            ast::Expression::HashLiteral(node) => eval_hash_literal(node, env),
        },
    }
}

fn eval_hash_literal(hash: &HashLiteral, env: &SharedEnvironment) -> Object {
    let mut pairs = HashMap::default();

    for (key_node, value_node) in hash.pairs.iter() {
        let key = eval_and_return_if_error!(&Node::Expression(key_node.clone()), env);

        let value = eval_and_return_if_error!(&Node::Expression(value_node.clone()), env);

        pairs.insert(key, value);
    }

    Object::Hash(Hash(pairs))
}

fn eval_index_expression(left: Object, index: Object) -> Object {
    let left_type = left.r#type();

    if let Object::Array(left) = left {
        if let Object::Integer(index) = index {
            return eval_array_index_expression(left, index);
        }
    } else if let Object::Hash(left) = left {
        return eval_hash_index_expression(left, index);
    }

    Object::Error(format!("index operator not supported: {}", left_type))
}

fn eval_array_index_expression(left: Vec<Object>, index: i64) -> Object {
    if index < 0 || index as usize >= left.len() {
        Object::Null
    } else {
        left[index as usize].clone()
    }
}

fn eval_hash_index_expression(left: Hash, index: Object) -> Object {
    let Hash(pairs) = left;

    // this can be omitted and use any value as key
    {
        use Object::*;
        match index {
            Integer(_) | String(_) | Boolean(_) => {}
            _ => return Error(format!("unusable as hash key: {}", index.r#type())),
        }
    }

    let value = pairs.get(&index);

    value.map(Clone::clone).unwrap_or_else(|| Object::Null)
}

fn apply_function(function: Object, args: Vec<Object>) -> Object {
    match function {
        Object::Function(Function {
            parameters,
            body,
            env,
        }) => {
            let extended_env = Rc::new(RefCell::new(extend_function_env(env, parameters, args)));
            let evaluated = eval(&Node::Statement(Statement::Block(body)), &extended_env);

            unwrap_return_value(evaluated)
        }
        Object::BuiltinFunction(f) => f(args),
        _ => panic!("not a function: {}", function.r#type()),
    }
}

fn unwrap_return_value(evaluated: Object) -> Object {
    match evaluated {
        Object::ReturnValue(v) => *v,
        _ => evaluated,
    }
}

fn extend_function_env(
    env: SharedEnvironment,
    parameters: Vec<Identifier>,
    args: Vec<Object>,
) -> Environment {
    let mut env = Environment::enclosed(env);

    for (i, param) in parameters.into_iter().enumerate() {
        env.set(&param.value, args[i].clone());
    }

    env
}

fn eval_expressions(expressions: &Vec<ast::Expression>, env: &SharedEnvironment) -> Vec<Object> {
    let mut result = Vec::default();

    for expression in expressions {
        let evaluated = eval(&Node::Expression(expression.clone()), env);
        if evaluated.is_error() {
            return vec![evaluated];
        }

        result.push(evaluated);
    }

    result
}

fn eval_identifier(identifier: &crate::ast::Identifier, env: &SharedEnvironment) -> Object {
    let identifier: &str = &identifier.value;
    let env = env.borrow();
    let val = env.get(identifier);
    val.or_else(|| {
        BUILTINS
            .get(identifier)
            .map(|f| Object::BuiltinFunction(*f))
    })
    .unwrap_or_else(|| Object::Error(format!("identifier not found: {identifier}")))
}

fn eval_if_expression(e: &IfExpression, env: &SharedEnvironment) -> Object {
    let condition = eval_and_return_if_error!(&Node::Expression(*e.condition.clone()), env);

    if is_truthy(&condition) {
        eval(
            &Node::Statement(Statement::Block(e.consequence.clone())),
            env,
        )
    } else if let Some(alternative) = &e.alternative {
        eval(&Node::Statement(Statement::Block(alternative.clone())), env)
    } else {
        Object::Null
    }
}

fn is_truthy(obj: &Object) -> bool {
    match obj {
        Object::Null => false,
        Object::Boolean(v) => *v,
        _ => true,
    }
}

fn eval_infix_expression(operator: &str, left: &Object, right: &Object) -> Object {
    if let Object::Integer(left) = left {
        if let Object::Integer(right) = right {
            return eval_integer_infix_expression(operator, left, right);
        }
    }

    if let Object::String(left) = left {
        if let Object::String(right) = right {
            return eval_string_infix_expression(operator, left, right);
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
        "==" => Object::Boolean(left == right),
        "!=" => Object::Boolean(left != right),
        _ => Object::Error(format!(
            "unknown operator: {} {operator} {}",
            left.r#type(),
            right.r#type()
        )),
    }
}

fn eval_string_infix_expression(operator: &str, left: &str, right: &str) -> Object {
    if operator != "+" {
        return Object::Error(format!("unknown operator: STRING {operator} STRING"));
    }

    Object::String(format!("{left}{right}"))
}

fn eval_integer_infix_expression(operator: &str, left: &i64, right: &i64) -> Object {
    match operator {
        "+" => Object::Integer(left + right),
        "-" => Object::Integer(left - right),
        "*" => Object::Integer(left * right),
        "/" => Object::Integer(left / right),
        "<" => Object::Boolean(left < right),
        ">" => Object::Boolean(left > right),
        "==" => Object::Boolean(left == right),
        "!=" => Object::Boolean(left != right),
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
    if let Object::Integer(value) = right {
        Object::Integer(-value)
    } else {
        Object::Error(format!("unknown operator: -{}", right.r#type()))
    }
}

fn eval_bang_operator_expression(right: &Object) -> Object {
    let val = is_truthy(right);

    Object::Boolean(!val)
}

fn eval_block_statement(block: &BlockStatement, env: &SharedEnvironment) -> Object {
    let statements = &block.statements;

    let mut result = None;

    for statement in statements {
        result = Some(eval(&Node::Statement(statement.clone()), env));

        if let Some(rv @ Object::ReturnValue(_)) = result {
            return rv;
        } else if let Some(e @ Object::Error(_)) = result {
            return e;
        }
    }

    result.unwrap()
}

fn eval_program(program: &Program, env: &SharedEnvironment) -> Object {
    let statements = &program.statements;

    let mut result = None;

    for statement in statements {
        let evaluated = eval_and_return_if_error!(&Node::Statement(statement.clone()), env);

        if let Object::ReturnValue(val) = evaluated {
            return *val;
        }

        result = Some(evaluated);
    }

    result.unwrap()
}

#[cfg(test)]
mod test {
    use std::{cell::RefCell, collections::HashMap};

    use crate::{lexer::Lexer, object::Quote, parser::Parser, test_utils::{match_or_fail, test_eval}};

    use super::*;

    #[test]
    fn test_string_literal() {
        let input = "\"Hello World!\"";

        let evaluated = test_eval(input);

        let result = match_or_fail!(evaluated, Object::String(e) => e);

        assert_eq!(result, "Hello World!");
    }

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
            ("foobar", "identifier not found: foobar"),
            ("\"Hello\" - \"World\"", "unknown operator: STRING - STRING"),
            (
                r#"{"name": "Monkey"}[fn(x) { x }];"#,
                "unusable as hash key: FUNCTION",
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
    fn test_array_literals() {
        let input = "[1, 2 * 2, 3 + 3]";

        let evaluated = test_eval(input);

        let result = match_or_fail!(evaluated, Object::Array(e) => e);

        assert_eq!(result.len(), 3);

        test_integer_object(&result[0], 1);
        test_integer_object(&result[1], 4);
        test_integer_object(&result[2], 6);
    }

    #[test]
    fn test_hash_literals() {
        let input = r#"
let two = "two";
{
    "one": 10 - 9,
    two: 1 + 1,
    "thr" + "ee": 6 / 2,
    4: 4,
    true: 5,
    false: 6
}
"#;

        let evaluated = test_eval(input);

        let pairs = match_or_fail!(evaluated, Object::Hash(Hash(e)) => e);

        let expected = HashMap::from([
            (Object::String("one".to_owned()), 1),
            (Object::String("two".to_owned()), 2),
            (Object::String("three".to_owned()), 3),
            (Object::Integer(4), 4),
            (Object::Boolean(true), 5),
            (Object::Boolean(false), 6),
        ]);

        assert_eq!(pairs.len(), expected.len());

        for (expected_key, expected_value) in expected {
            let value = pairs.get(&expected_key).expect("some value");
            test_integer_object(value, expected_value);
        }
    }

    #[test]
    fn test_array_index_expressions() {
        let tests = [
            ("[1, 2, 3][0]", Object::Integer(1)),
            ("[1, 2, 3][1]", Object::Integer(2)),
            ("[1, 2, 3][2]", Object::Integer(3)),
            ("let i = 0; [1][i];", Object::Integer(1)),
            ("[1, 2, 3][1 + 1];", Object::Integer(3)),
            ("let myArray = [1, 2, 3]; myArray[2];", Object::Integer(3)),
            (
                "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
                Object::Integer(6),
            ),
            (
                "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
                Object::Integer(2),
            ),
            ("[1, 2, 3][3]", Object::Null),
            ("[1, 2, 3][-1]", Object::Null),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);

            assert_eq!(evaluated, expected);
        }
    }

    #[test]
    fn test_hash_index_expressions() {
        let tests = [
            (r#"{"foo": 5}["foo"]"#, Object::Integer(5)),
            (r#"{"foo": 5}["bar"]"#, Object::Null),
            (r#"let key = "foo"; {"foo": 5}[key]"#, Object::Integer(5)),
            (r#"{}["foo"]"#, Object::Null),
            (r#"{5: 5}[5]"#, Object::Integer(5)),
            (r#"{true: 5}[true]"#, Object::Integer(5)),
            (r#"{false: 5}[false]"#, Object::Integer(5)),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);

            assert_eq!(evaluated, expected);
        }
    }

    #[test]
    fn test_let_statements() {
        let tests = [
            ("let a = 5; a;", 5),
            ("let a = 5 * 5; a;", 25),
            ("let a = 5; let b = a; b;", 5),
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ];

        for (input, expected) in tests {
            test_integer_object(&test_eval(input), expected);
        }
    }

    #[test]
    fn test_string_concatenation() {
        let input = "\"Hello\" + \" \" + \"World!\"";

        let evaluated = test_eval(input);

        let s = match_or_fail!(evaluated, Object::String(e) => e);

        assert_eq!(s, "Hello World!");
    }

    #[test]
    fn test_closures() {
        let input = r#"
let newAdder = fn(x) {
  fn(y) { x + y };
};

let addTwo = newAdder(2);
addTwo(2);
"#;

        test_integer_object(&test_eval(input), 4);
    }

    #[test]
    fn test_builtin_functions() {
        let tests = [
            // len
            ("len(\"\")", Object::Integer(0)),
            ("len(\"four\")", Object::Integer(4)),
            ("len(\"hello world\")", Object::Integer(11)),
            (
                "len(1)",
                Object::Error("argument to `len` not supported, got INTEGER".to_owned()),
            ),
            (
                "len(\"one\", \"two\")",
                Object::Error("wrong number of arguments. got=2, want=1".to_owned()),
            ),
            ("len([1, 2, 3])", Object::Integer(3)),
            // first
            ("first([1, 2, 3])", Object::Integer(1)),
            ("first([])", Object::Null),
            // last
            ("last([1, 2, 3])", Object::Integer(3)),
            ("last([])", Object::Null),
            // rest
            ("rest([])", Object::Null),
            ("rest([2,3])", Object::Array(vec![Object::Integer(3)])),
            (
                "rest([1,2,3])",
                Object::Array(vec![Object::Integer(2), Object::Integer(3)]),
            ),
            ("rest([3])", Object::Array(vec![])),
            // push
            (
                "push(1, 1)",
                Object::Error("argument to `push` not supported, got INTEGER".to_owned()),
            ),
            ("push([], 1)", Object::Array(vec![Object::Integer(1)])),
            (
                "push([1], 2)",
                Object::Array(vec![Object::Integer(1), Object::Integer(2)]),
            ),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);

            assert_eq!(evaluated, expected);
        }
    }

    #[test]
    fn test_custom_embedded_function() {
        let input = r#"
let map = fn(arr, f) {
  let iter = fn(arr, accumulated) {
    if (len(arr) == 0) {
      accumulated
    } else {
      iter(rest(arr), push(accumulated, f(first(arr))));
    }
  };

  iter(arr, []);
};

let a = [1, 2];
let double = fn(x) { x * 2 };

map(a, double);
"#;

        let evaluated = test_eval(input);
        assert_eq!(
            evaluated,
            Object::Array(vec![Object::Integer(2), Object::Integer(4)])
        );
    }

    #[test]
    fn test_if_else_expressions() {
        let tests = [
            ("if (true) { 10 }", Object::Integer(10)),
            ("if (false) { 10 }", Object::Null),
            ("if (1) { 10 }", Object::Integer(10)),
            ("if (1 < 2) { 10 }", Object::Integer(10)),
            ("if (1 > 2) { 10 }", Object::Null),
            ("if (1 > 2) { 10 } else { 20 }", Object::Integer(20)),
            ("if (1 < 2) { 10 } else { 20 }", Object::Integer(10)),
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

    #[test]
    fn test_function_object() {
        let input = "fn(x) { x + 2; };";

        let evaluated = test_eval(input);

        let f = match_or_fail!(evaluated, Object::Function(f) => f);

        assert_eq!(f.parameters.len(), 1);
        assert_eq!(f.parameters[0].string(), "x");
        assert_eq!(f.body.string(), "{(x + 2);}");
    }

    #[test]
    fn test_function_application() {
        let tests = [
            ("let identity = fn(x) { x; }; identity(5);", 5),
            ("let identity = fn(x) { return x; }; identity(5);", 5),
            ("let double = fn(x) { x * 2; }; double(5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
            ("fn(x) { x; }(5)", 5),
        ];

        for (input, expected) in tests {
            test_integer_object(&test_eval(input), expected);
        }
    }

    // *****************************************************************************************************
    // *************** Helpers ***************
    // *****************************************************************************************************

    fn test_boolean_object(evaluated: &Object, expected: bool) {
        let result = match_or_fail!(evaluated, Object::Boolean(e) => e);

        assert_eq!(*result, expected);
    }

    fn test_integer_object(evaluated: &Object, expected: i64) {
        let result = match_or_fail!(evaluated, Object::Integer(e) => e);

        assert_eq!(*result, expected);
    }
}
