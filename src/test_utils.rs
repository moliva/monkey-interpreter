/// Tries to match a given expression to a pattern and return the identifier given in the
/// pattern, failing otherwise.
macro_rules! match_or_fail {
    ($expression:expr, $pattern:pat => $identifier:ident) => {
        match $expression {
            $pattern => $identifier,
            _ => panic!("not expected type from expression {:?}", $expression),
        }
    };
}

pub(crate) use match_or_fail; // <-- the trick
