use anyhow::Result;

use monkey_interpreter::repl;

pub fn main() -> Result<()> {
    let user = whoami::realname();

    println!("Hello {}! This is the Monkey programming language!", user);
    println!("Feel free to type in commands");

    let stdin = std::io::stdin();
    let mut stdout = std::io::stdout();

    repl::start(stdin, &mut stdout)?;
    Ok(())
}
