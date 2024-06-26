use std::collections::HashMap;

use crate::object::{BuiltinFunction, Object};

lazy_static! {
    pub(crate) static ref BUILTINS: HashMap<&'static str, BuiltinFunction> = {
        let mut builtins = HashMap::<_, BuiltinFunction>::new();
        // TODO - macro  for adding function identifier -> function - moliva - 2024/03/12
        builtins.insert("len", len);
        builtins.insert("first", first);
        builtins.insert("last", last);
        builtins.insert("rest", rest);
        builtins.insert("push", push);
        builtins.insert("puts", puts);
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
            Object::String(s) => Object::Integer(s.len() as i64),
            Object::Array(a) => Object::Integer(a.len() as i64),
            _ => Object::Error(format!(
                "argument to `len` not supported, got {}",
                arg.r#type()
            )),
        }
    }
}

fn first(mut args: Vec<Object>) -> Object {
    if args.len() != 1 {
        Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ))
    } else {
        let arg = args.pop().unwrap();

        match arg {
            Object::Array(mut a) => {
                if !a.is_empty() {
                    a.remove(0)
                } else {
                    Object::Null
                }
            }
            _ => Object::Error(format!(
                "argument to `first` not supported, got {}",
                arg.r#type()
            )),
        }
    }
}

fn last(mut args: Vec<Object>) -> Object {
    if args.len() != 1 {
        Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ))
    } else {
        let arg = args.pop().unwrap();

        match arg {
            Object::Array(mut a) => a.pop().unwrap_or_else(|| Object::Null),
            _ => Object::Error(format!(
                "argument to `last` not supported, got {}",
                arg.r#type()
            )),
        }
    }
}

fn rest(mut args: Vec<Object>) -> Object {
    if args.len() != 1 {
        Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ))
    } else {
        let arg = args.pop().unwrap();

        match arg {
            Object::Array(a) => {
                if a.is_empty() {
                    Object::Null
                } else {
                    Object::Array(a[1..a.len()].to_vec())
                }
            }
            _ => Object::Error(format!(
                "argument to `rest` not supported, got {}",
                arg.r#type()
            )),
        }
    }
}

fn push(mut args: Vec<Object>) -> Object {
    if args.len() != 2 {
        Object::Error(format!(
            "wrong number of arguments. got={}, want=2",
            args.len()
        ))
    } else {
        let elem = args.pop().unwrap();
        let array = args.pop().unwrap();

        match array {
            Object::Array(a) => {
                let mut a = a.clone();
                a.push(elem);

                Object::Array(a)
            }
            _ => Object::Error(format!(
                "argument to `push` not supported, got {}",
                array.r#type()
            )),
        }
    }
}

fn puts(args: Vec<Object>) -> Object {
    for arg in args {
        println!("{}", arg.inspect());
    }
    Object::Null
}
