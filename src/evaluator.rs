use crate::{
    ast::{Node, Statement},
    object::{Integer, Object},
};

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
            crate::ast::Expression::Boolean(_) => todo!(),
            crate::ast::Expression::PrefixExpression(_) => todo!(),
            crate::ast::Expression::InfixExpression(_) => todo!(),
            crate::ast::Expression::IfExpression(_) => todo!(),
            crate::ast::Expression::FunctionLiteral(_) => todo!(),
            crate::ast::Expression::CallExpression(_) => todo!(),
        },
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
        let tests = [("5", 5), ("10", 10)];

        for (input, expected) in tests {
            let evaluated = test_eval(input);

            test_integer_object(&evaluated, expected);
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

    fn test_integer_object(evaluated: &Object, expected: i64) {
        let result = match evaluated {
            Object::Integer(i) => i,
            _ => panic!("object is not Integer. got={:?}", evaluated),
        };

        assert_eq!(result.value, expected);
    }
}
