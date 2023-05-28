#[macro_use]
extern crate lazy_static;

pub mod token;
pub mod lexer;
pub mod repl;

pub fn main() {

}

#[cfg(test)]
mod test {

    #[ignore]
    #[test]
    fn test() {
        let monkey_source = r#"
let five = 5;
let ten = 10;
let add = fn(x, y) {
x + y;
};
let result = add(five, ten);
"#;
println!("source code: {}", monkey_source); }
}
