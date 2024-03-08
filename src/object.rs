#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Object {
    Null,
    Integer(Integer),
    Boolean(Boolean),
    ReturnValue(Box<Object>),
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Self::Integer(Integer(value)) => value.to_string(),
            Self::Boolean(Boolean(value)) => value.to_string(),
            Self::Null => "null".to_owned(),
            Self::ReturnValue(v) => v.inspect(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Integer(pub i64);

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Boolean(pub bool);
