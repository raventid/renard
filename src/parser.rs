use crate::ast;
use crate::lexer;
use crate::token;

pub struct Parser {
    lexer: lexer::Lexer,
    current_token: token::Token,
    peek_token: token::Token,
}

impl Parser {
    fn new(mut lexer: lexer::Lexer) -> Self {
        let current_token = lexer.next_token();
        let peek_token = lexer.next_token();
        Self {
            lexer,
            current_token,
            peek_token,
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
            _ => None,
        }
    }

    fn parse_let_statement(&mut self) -> Option<token::LetStatement> {
        let token = self.current_token.clone();

        if !(self.peek_token.token_type == token::IDENT) {
            self.next_token();
            return None;
        } else {
            ()
        }

        let name = token::Identifier {
            token: self.current_token.clone(),
            value: self.current_token.literal.clone(),
        };

        if !(self.peek_token.token_type == token::ASSIGN) {
            self.next_token();
            return None;
        } else {
            ()
        }

        while !(self.current_token.token_type == token::SEMICOLON) {
            self.next_token()
        }

        Some(token::LetStatement {
            token,
            name,
            value: "dumb".to_string(),
        })
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
          let bebe = 101010
        "###
        .to_string();

        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = match parser.parse_program() {
            Some(program) => program,
            None => panic!("Could not parse program"),
        };

        dbg!(&program);

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
}
