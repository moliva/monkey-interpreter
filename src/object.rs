#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Object {
    Null,
    Integer(Integer),
    Boolean(Boolean),
    ReturnValue(Box<Object>),
    Error(String),
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
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Integer(pub i64);

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Boolean(pub bool);
