use std::{cell::RefCell, collections::HashMap, rc::Rc};

use itertools::Itertools;

use crate::ast::ast::{BlockStatement, Identifier, Node};

pub(crate) type BuiltinFunction = fn(Vec<Object>) -> Object;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum Object {
    Null,
    Integer(i64),
    Boolean(bool),
    String(String),
    Array(Vec<Object>),
    Hash(Hash),
    Function(Function),
    Macro(Macro),
    BuiltinFunction(BuiltinFunction),
    ReturnValue(Box<Object>),
    Error(String),
    Quote(Quote),
}

impl Object {
    pub const fn is_error(&self) -> bool {
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
            Object::Hash(_) => "HASH",
            Object::Quote(_) => "QUOTE",
            Object::Macro(_) => "MACRO",
        }
        .to_owned()
    }

    pub fn inspect(&self) -> String {
        match self {
            Object::Integer(value) => value.to_string(),
            Object::Boolean(value) => value.to_string(),
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
            Object::Macro(Macro {
                parameters, body, ..
            }) => {
                let params = parameters.iter().map(Identifier::string).join(", ");
                let body = body.string();

                format!("macro({params}) {}\n{body}\n{}", "{", "}")
            }
            Object::String(s) => format!("\"{s}\""),
            Object::BuiltinFunction(_) => "builtin function".to_owned(),
            Object::Array(a) => {
                let elements = a.iter().map(Object::inspect).join(", ");
                format!("[{elements}]")
            }
            Object::Hash(Hash(pairs)) => {
                let pairs = pairs
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k.inspect(), v.inspect()))
                    .join(", ");

                format!("{{{pairs}}}")
            }
            Object::Quote(Quote(node)) => format!("QUOTE({})", node.string()),
        }
    }
}

pub(crate) type SharedEnvironment = Rc<RefCell<Environment>>;

#[derive(Debug, Clone, Default)]
pub(crate) struct Environment {
    store: HashMap<String, Object>,
    outer: Option<SharedEnvironment>,
}

impl Environment {
    pub fn enclosed(outer: SharedEnvironment) -> Self {
        Self {
            outer: Some(outer),
            ..Default::default()
        }
    }

    pub fn into_shared(self) -> SharedEnvironment {
        Rc::new(RefCell::new(self))
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        let obj = self.store.get(name);

        obj.cloned()
            .or_else(|| self.outer.as_ref()?.borrow().get(name).clone())
    }

    pub fn set(&mut self, name: &str, val: Object) {
        self.store.insert(name.to_owned(), val);
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Hash(pub HashMap<Object, Object>);

impl PartialEq for Hash {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(&self.0, &other.0)
    }
}

impl Eq for Hash {}

impl std::hash::Hash for Hash {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::ptr::hash(&self.0, state);
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Macro {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub env: SharedEnvironment,
}

impl PartialEq for Macro {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(&self.parameters, &other.parameters)
            && std::ptr::eq(&self.body, &other.body)
            && std::ptr::eq(&self.env, &other.env)
    }
}

impl std::hash::Hash for Macro {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::ptr::hash(&self.parameters, state);
        std::ptr::hash(&self.body, state);
        std::ptr::hash(&self.env, state);
    }
}

impl Eq for Macro {}

#[derive(Debug, Clone)]
pub(crate) struct Function {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub env: SharedEnvironment,
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(&self.parameters, &other.parameters)
            && std::ptr::eq(&self.body, &other.body)
            && std::ptr::eq(&self.env, &other.env)
    }
}

impl std::hash::Hash for Function {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::ptr::hash(&self.parameters, state);
        std::ptr::hash(&self.body, state);
        std::ptr::hash(&self.env, state);
    }
}

impl Eq for Function {}

#[derive(Debug, Clone)]
pub(crate) struct Quote(pub Node);

impl PartialEq for Quote {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(&self.0, &other.0)
    }
}

impl std::hash::Hash for Quote {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::ptr::hash(&self.0, state);
    }
}

impl Eq for Quote {}
