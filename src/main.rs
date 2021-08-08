#![feature(type_alias_enum_variants)]

mod lexer;
mod token;
mod repl;
mod ast;
mod parser;
mod evaluation {
    pub mod object;
    pub mod evaluator;
    pub mod environment;
}
mod core {
    pub mod funcs;
}

fn main() {
    // how to get current user name in crossplatform style?
    let _username = "user".to_string();
    println!("Dear user, welcome to Renard REPL!");
    repl::start();
}
