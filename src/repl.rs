use crate::lexer;
use crate::token;
use std::io::{stdin, stdout, Write};

const PROMPT: &str = "clojurium $ ";

pub fn start() {
    loop {
        let mut user_input = String::new();
        print!("{}", PROMPT);

        let _ = stdout().flush();

        stdin()
            .read_line(&mut user_input)
            .expect("Fatal error: Cannot read user input");

        // Something like trim (perhaps just use `trim`?)
        if let Some('\n') = user_input.chars().next_back() {
            user_input.pop();
        }
        if let Some('\r') = user_input.chars().next_back() {
            user_input.pop();
        }

        match user_input.as_str() {
            ":q" => {
                println!("Bye! Have a nice day!");
                break;
            }
            _ => 1, // just ignore this for now
        };

        let mut lexer = lexer::Lexer::new(user_input);

        while let token = lexer.next_token() {
            if token.token_type == token::EOF {
                break;
            }

            println!("{:?}", token);
        }
    }
}
