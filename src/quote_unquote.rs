use crate::{
    ast::Node,
    object::{Object, Quote},
};

pub(crate) fn quote(node: Node) -> Object {
    Object::Quote(Quote(node))
}

#[cfg(test)]
mod test {
    use crate::test_utils::{match_or_fail, test_eval};

    use super::*;

    #[test]
    fn test_quote() {
        let tests = [
            ("quote(5)", "5"),
            ("quote(5 + 8)", "(5 + 8)"),
            ("quote(foobar)", "foobar"),
            ("quote(foobar + barfoo)", "(foobar + barfoo)"),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);

            let Quote(node) = match_or_fail!(evaluated, Object::Quote(q) => q);

            assert_eq!(node.string(), expected);
        }
    }

    #[test]
    #[ignore]
    fn test_quote_unquote() {
        let tests = [
            ("quote(unquote(4))", "4"),
            ("quote(unquote(4 + 4))", "8"),
            ("quote(8 + unquote(4 + 4))", "(8 + 8)"),
            ("quote(unquote(4 + 4) + 8)", "(8 + 8)"),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);

            let Quote(node) = match_or_fail!(evaluated, Object::Quote(q) => q);

            assert_eq!(node.string(), expected);
        }
    }
}
