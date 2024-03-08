enum Object {
    Integer(Integer),
    Boolean(Boolean),
    Null,
}

impl Object {
    fn inspect(&self) -> String {
        match self {
            Self::Integer(Integer { value }) => value.to_string(),
            Self::Boolean(Boolean { value }) => value.to_string(),
            Self::Null => "null".to_owned(),
        }
    }
}

struct Integer {
    value: i64,
}

struct Boolean {
    value: bool,
}
