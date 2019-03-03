mod lexer;
mod token;
mod repl;

fn main() {
    // how to get current user name in crossplatform style?
    let username = "user".to_string();
    println!("Dear user, welcome to Clojurium REPL!");
    println!("Some more advanced description of what's going on here");
    repl::start();
}
