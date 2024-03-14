use crate::{
    ast::Node,
    object::{Object, Quote},
};

pub fn quote(node: Node) -> Object {
    Object::Quote(Quote(node))
}
