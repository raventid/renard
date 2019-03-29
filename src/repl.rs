use crate::lexer;
use crate::parser;
use crate::evaluation::evaluator;
use crate::evaluation::environment;
use crate::evaluation::object::ObjectT;
use std::io::{stdin, stdout, Write};
use std::collections::HashMap;

const PROMPT: &str = "clojurium $ ";

pub fn start() {
    let mut env = environment::Environment::new();

    loop {
        let mut user_input = String::new();

        print!("{}", PROMPT);

        let _ = stdout().flush();

        stdin()
            .read_line(&mut user_input)
            .expect("Fatal error: Cannot read user input");

        // Something like trim (perhaps just use `trim`?)
        trim_input(&mut user_input);

        // HANDLE SPECIAL REPL CODES
        match user_input.as_str() {
            ":q" => {
                println!("Bye! Have a nice day!");
                break;
            }
            _ => 1, // just ignore this for now
        };

        let lexer = lexer::Lexer::new(user_input);
        let mut parser = parser::Parser::new(lexer);


        // TODO: Very ugly interface to parser.
        // LambdaParsers is a hack itself, so worth
        // to change it.
        let mut lambda_parsers = parser::LambdaParsers {
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };

        lambda_parsers.register_parsers();

        let program = parser.parse_program(&lambda_parsers);

        // We would like to accumulate every error in program
        // and later render them to user.

        // This function used extensively in parser tests
        if !parser.errors.is_empty() {
            println!("Parser encountered {} errors", parser.errors.len());

            for error in parser.errors {
                println!("parser error: {}", error);
            }
        } else {
            let evaluated = evaluator::eval(evaluator::WN::P(program), &mut env);
            println!("{}", evaluated.inspect())
        }
    }
}

fn trim_input(user_input: &mut String) {
    // Something like trim (perhaps just use `trim`?)
    if let Some('\n') = user_input.chars().next_back() {
        user_input.pop();
    }
    if let Some('\r') = user_input.chars().next_back() {
        user_input.pop();
    }
}
