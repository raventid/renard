use std::collections::HashMap;
use std::fmt;

use crate::ast;
use crate::lexer;
use crate::token;


// For debug visualization I migth potentially use this approach
// https://users.rust-lang.org/t/is-it-possible-to-implement-debug-for-fn-type/14824

// Greeting to the master of functinal Rust - mighty @raventid
type PrefixParseFnAlias = Fn () -> token::Expression;
struct PrefixParseFn(Box<PrefixParseFnAlias>);
impl fmt::Debug for PrefixParseFn {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", "prefix_parse_fn")
    }
}

type InfixParseFnAlias = Fn (token::Expression) -> token::Expression;
struct InfixParseFn(Box<InfixParseFnAlias>);
impl fmt::Debug for InfixParseFn {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", "infix_parse_fn")
    }
}

#[derive(Debug)]
pub struct Parser {
    lexer: lexer::Lexer,
    current_token: token::Token,
    peek_token: token::Token,
    pub errors: Vec<String>,

    prefix_parse_fns: HashMap<token::TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<token::TokenType, InfixParseFn>,
}

impl Parser {
    fn new(mut lexer: lexer::Lexer) -> Self {
        let current_token = lexer.next_token();
        let peek_token = lexer.next_token();
        let errors = Vec::new();
        let prefix_parse_fns = HashMap::new();
        let infix_parse_fns = HashMap::new();
        Self {
            lexer,
            current_token,
            peek_token,
            errors,
            prefix_parse_fns,
            infix_parse_fns,
        }
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    pub fn parse_program(&mut self) -> Option<ast::Program> {
        let mut program = ast::Program {
            statements: Vec::new(),
        };
        while self.current_token.token_type != token::EOF {
            let statement = self.parse_statement();
            match statement {
                Some(stmt) => program.statements.push(stmt),
                _ => (), // Just ignore that case
            };
            self.next_token();
        }
        Some(program)
    }

    fn parse_statement(&mut self) -> Option<token::Statements> {
        match self.current_token.token_type.as_ref() {
            token::LET => match self.parse_let_statement() {
                Some(stmt) => Some(token::Statements::LetStatement(stmt)),
                _ => None,
            },
            token::RETURN => match self.parse_return_statement() {
                Some(stmt) => Some(token::Statements::ReturnStatement(stmt)),
                _ => None,
            },
            _ => None,
        }
    }

    fn peek_error(&mut self, token: token::TokenType) {
        let message = format!(
            "expected next token to be {expected}, got {got} instead",
            expected = token,
            got = self.peek_token.token_type,
        );

        self.errors.push(message);
    }

    fn parse_let_statement(&mut self) -> Option<token::LetStatement> {
        let token = self.current_token.clone();

        if self.peek_token.token_type == token::IDENT {
            self.next_token();
        } else {
            self.peek_error(token::IDENT.to_string());
            return None;
        }

        let name = token::Identifier {
            token: self.current_token.clone(),
            value: self.current_token.literal.clone(),
        };

        if self.peek_token.token_type == token::ASSIGN {
            self.next_token();
        } else {
            self.peek_error(token::ASSIGN.to_string());
            return None;
        }

        // TODO: It's a fragile design for now, this code might hang if we don't
        // have a terminating semicolon and next token is token::EOF
        // in this case we'll enter an infinite loop.
        // Doesn't next_token() protect us from this? Apparently - not.
        while !(self.current_token.token_type == token::SEMICOLON) {
            self.next_token(); // skip to next statement in our program
        }

        Some(token::LetStatement {
            token,
            name,
            value: "dumb".to_string(),
        })
    }

    fn parse_return_statement(&mut self) -> Option<token::ReturnStatement> {
        let statement = token::ReturnStatement {
            token: self.current_token.clone(),
            return_value: "dumb".to_string(), // How to better describe expression?
        };

        self.next_token();

        while !(self.peek_token.token_type == token::SEMICOLON) {
            self.next_token(); // skip everything till `;` for now
        }

        Some(statement)
    }

    fn register_prefix(&mut self, token_type: token::TokenType, f: Box<PrefixParseFnAlias>) {
        self.prefix_parse_fns.insert(token_type, PrefixParseFn(f));
    }

    fn register_infix(&mut self, token_type: token::TokenType, f: Box<InfixParseFnAlias>) {
        self.infix_parse_fns.insert(token_type, InfixParseFn(f));
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::Node;
    use crate::lexer;
    use crate::parser::Parser;
    use crate::token::Statements;

    #[test]
    fn test_let_statements() {
        let input = r###"
          let x = 5;
          let y = 10;
          let bebe = 101010;
        "###
        .to_string();

        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = match parser.parse_program() {
            Some(program) => program,
            None => panic!("Could not parse program"),
        };

        // We would like to accumulate every error in program
        // and later render them to user.
        if !parser.errors.is_empty() {
            println!("Parser encountered {} errors", parser.errors.len());
            for error in parser.errors {
                println!("parser error: {}", error);
            }
            panic!("A few parsing error encountered, see them above.");
        }

        assert_eq!(program.statements.len(), 3);

        let expected = vec!["x".to_string(), "y".to_string(), "bebe".to_string()];

        program
            .statements
            .into_iter()
            .zip(expected.into_iter())
            .for_each(|(statement, expected_identifier)| {
                assert_eq!(statement.token_literal(), "let");

                let let_statement = match statement {
                    Statements::LetStatement(statement) => statement,
                    _ => panic!("I didn't expected anything besides `let` statement"),
                };

                assert_eq!(let_statement.name.value, expected_identifier);

                assert_eq!(let_statement.name.token_literal(), expected_identifier);
            });
    }

    #[test]
    fn test_broken_let_statements_and_check_if_errors_appear() {
        let input = r###"
          let x = 5;
          let y = 10;
          let bebe 101010;
        "###
        .to_string();

        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);

        match parser.parse_program() {
            Some(program) => program,
            None => panic!("Could not parse program"),
        };

        // We would like to accumulate every error in program
        // and later render them to user.
        if !parser.errors.is_empty() {
            for error in parser.errors {
                assert_eq!(
                    "parser error: expected next token to be =, got INT instead",
                    format!("parser error: {}", error)
                );
            }
        }
    }

    #[test]
    fn test_return_statement() {
        let input = r###"
          return 1;
          return 111;
        "###
        .to_string();

        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = match parser.parse_program() {
            Some(program) => program,
            None => panic!("Could not parse program"),
        };

        // We would like to accumulate every error in program
        // and later render them to user.
        if !parser.errors.is_empty() {
            println!("Parser encountered {} errors", parser.errors.len());
            for error in parser.errors {
                println!("parser error: {}", error);
            }
            panic!("A few parsing error encountered, see them above.");
        }

        assert_eq!(program.statements.len(), 2);

        let expected = vec!["1".to_string(), "111".to_string()];

        program
            .statements
            .into_iter()
            .zip(expected.into_iter())
            .for_each(|(statement, expected_identifier)| {
                assert_eq!(statement.token_literal(), "return");

                let return_statement = match statement {
                    Statements::ReturnStatement(statement) => statement,
                    _ => panic!("I didn't expected anything besides `return` statement"),
                };
            });
    }

    #[test]
    fn test_identifier_expression() {
        let input = r###"
          bebe;
        "###.to_string();

        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = match parser.parse_program() {
            Some(program) => program,
            None => panic!("Could not parse program"),
        };

        // We would like to accumulate every error in program
        // and later render them to user.
        if !parser.errors.is_empty() {
            println!("Parser encountered {} errors", parser.errors.len());
            for error in parser.errors {
                println!("parser error: {}", error);
            }
            panic!("A few parsing error encountered, see them above.");
        }

        assert_eq!(program.statements.len(), 1);

        program
            .statements
            .into_iter()
            .for_each(|statement| {
                let expression_statement = match statement {
                    Statements::ExpressionStatement(statement) => statement,
                    _ => panic!("I didn't expect something besides expression statement"),
                };

                let identifier = expression_statement.expression;
                // TODO: Maybe pattern matching on concrete branch is better here
                // then some general value() method.
                // Anyway in control code I will use it the other way.
                assert_eq!(identifier.value(), "bebe");
                assert_eq!(identifier.token_literal(), "bebe");
            })
    }
}
