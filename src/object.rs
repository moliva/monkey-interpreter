#[derive(Debug, Clone)]
pub(crate) enum Object {
    Integer(Integer),
    Boolean(Boolean),
    Null,
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Self::Integer(Integer { value }) => value.to_string(),
            Self::Boolean(Boolean { value }) => value.to_string(),
            Self::Null => "null".to_owned(),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Integer {
    pub value: i64,
}

#[derive(Debug, Clone)]
pub(crate) struct Boolean {
    pub value: bool,
}
