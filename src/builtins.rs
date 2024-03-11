use std::collections::HashMap;

use crate::object::{BuiltinFunction, Integer, Object};

lazy_static! {
    pub(crate) static ref BUILTINS: HashMap<&'static str, BuiltinFunction> = {
        let mut builtins = HashMap::<_, BuiltinFunction>::new();
        builtins.insert("len", len);
        builtins
    };
}

fn len(mut args: Vec<Object>) -> Object {
    if args.len() != 1 {
        Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ))
    } else {
        let arg = args.pop().unwrap();
        match arg {
            Object::String(s) => Object::Integer(Integer(s.len() as i64)),
            _ => Object::Error(format!(
                "argument to `len` not supported, got {}",
                arg.r#type()
            )),
        }
    }
}
