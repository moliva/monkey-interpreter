use std::{cell::RefCell, collections::HashMap, rc::Rc};

use itertools::Itertools;

use crate::ast::{BlockStatement, Identifier};

pub(crate) type BuiltinFunction = fn(Vec<Object>) -> Object;

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Object {
    Null,
    Integer(Integer),
    String(String),
    Boolean(Boolean),
    Function(Function),
    BuiltinFunction(BuiltinFunction),
    Array(Vec<Object>),
    ReturnValue(Box<Object>),
    Error(String),
}

impl Object {
    pub fn is_error(&self) -> bool {
        matches!(self, Self::Error(_))
    }

    pub fn r#type(&self) -> String {
        match self {
            Object::Null => "NULL",
            Object::Integer(_) => "INTEGER",
            Object::Boolean(_) => "BOOLEAN",
            Object::ReturnValue(_) => "RETURN",
            Object::Error(_) => "ERROR",
            Object::Function(_) => "FUNCTION",
            Object::String(_) => "STRING",
            Object::BuiltinFunction(_) => "BUILTIN",
            Object::Array(_) => "ARRAY",
        }
        .to_owned()
    }

    pub fn inspect(&self) -> String {
        match self {
            Object::Integer(Integer(value)) => value.to_string(),
            Object::Boolean(Boolean(value)) => value.to_string(),
            Object::Null => "null".to_owned(),
            Object::ReturnValue(v) => v.inspect(),
            Object::Error(message) => format!("ERROR: {message}"),
            Object::Function(Function {
                parameters, body, ..
            }) => {
                let params = parameters.iter().map(Identifier::string).join(", ");
                let body = body.string();

                format!("fn({params}) {}\n{body}\n{}", "{", "}")
            }
            Object::String(s) => format!("\"{s}\""),
            Object::BuiltinFunction(_) => "builtin function".to_owned(),
            Object::Array(a) => {
                let elements = a.iter().map(Object::inspect).join(", ");
                format!("[{elements}]")
            }
        }
    }
}

#[derive(Debug, Clone, Default)]
pub(crate) struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Box<Rc<RefCell<Environment>>>>,
}

impl Environment {
    pub fn enclosed(outer: Rc<RefCell<Self>>) -> Self {
        Self {
            outer: Some(Box::new(outer)),
            ..Default::default()
        }
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        let obj = self.store.get(name);

        obj.map(Clone::clone)
            .or_else(|| self.outer.as_ref()?.borrow().get(name).clone())
    }

    pub fn set(&mut self, name: &str, val: Object) {
        self.store.insert(name.to_owned(), val);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Integer(pub i64);

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Boolean(pub bool);

#[derive(Debug, Clone)]
pub(crate) struct Function {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub env: Rc<RefCell<Environment>>,
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(&self.parameters, &other.parameters)
            && std::ptr::eq(&self.body, &other.body)
            && std::ptr::eq(&self.env, &other.env)
    }
}
