#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Object {
    Integer(Integer),
    Boolean(Boolean),
    Null,
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Self::Integer(Integer(value)) => value.to_string(),
            Self::Boolean(Boolean(value)) => value.to_string(),
            Self::Null => "null".to_owned(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Integer(pub i64);

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Boolean(pub bool);
