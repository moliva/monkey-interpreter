use std::{cell::RefCell, collections::HashMap};

use itertools::Itertools;

use crate::ast::{BlockStatement, Identifier};

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Object {
    Null,
    Integer(Integer),
    Boolean(Boolean),
    ReturnValue(Box<Object>),
    Error(String),
    Function(Function),
}

impl Object {
    pub fn is_error(&self) -> bool {
        matches!(self, Self::Error(_))
    }

    pub fn r#type(&self) -> String {
        match self {
            Self::Null => "NULL",
            Self::Integer(_) => "INTEGER",
            Self::Boolean(_) => "BOOLEAN",
            Self::ReturnValue(_) => "RETURN",
            Self::Error(_) => "ERROR",
            Self::Function(_) => "FUNCTION",
        }
        .to_owned()
    }

    pub fn inspect(&self) -> String {
        match self {
            Self::Integer(Integer(value)) => value.to_string(),
            Self::Boolean(Boolean(value)) => value.to_string(),
            Self::Null => "null".to_owned(),
            Self::ReturnValue(v) => v.inspect(),
            Self::Error(message) => format!("ERROR: {message}"),
            Self::Function(Function {
                parameters, body, ..
            }) => {
                let params = parameters.iter().map(Identifier::string).join(", ");
                let body = body.string();

                format!("fn({params}) {}\n{body}{}", "{", "}")
            }
        }
    }
}

#[derive(Debug, Clone, Default)]
pub(crate) struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Box<RefCell<Environment>>>,
}

impl Environment {
    pub fn enclosed(outer: RefCell<Self>) -> Self {
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
    pub env: RefCell<Environment>,
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(&self.parameters, &other.parameters)
            && std::ptr::eq(&self.body, &other.body)
            && std::ptr::eq(&self.env, &other.env)
    }
}
