use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::ast::{Expression, Let, MacroLiteral, Program, Statement},
    object::{Macro, Object, SharedEnvironment},
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
