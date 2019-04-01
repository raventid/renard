use std::collections::HashMap;
use std::fmt;

use lazy_static::lazy_static;

use crate::ast;
use crate::lexer;
use crate::token;
use trace::trace;

trace::init_depth_var!();

// TODO: Consider using Pratt terminology like `nuds` and `leds`
// Not sure it's very comprehensible, though...

// Priority table for different fixity operations
lazy_static! {
    static ref PRECEDENCES: HashMap<token::TokenType, u8> = [
        (token::EQ.to_string(), token::EQUALS),
        (token::NOT_EQ.to_string(), token::EQUALS),
        (token::LT.to_string(), token::LESSGREATER),
        (token::GT.to_string(), token::LESSGREATER),
        (token::PLUS.to_string(), token::SUM),
        (token::MINUS.to_string(), token::SUM),
        (token::SLASH.to_string(), token::PRODUCT),
        (token::ASTERISK.to_string(), token::PRODUCT),
        (token::LPAREN.to_string(), token::CALL), // `(` in infix position should have highest priority
    ]
    .iter()
    .cloned()
    .collect();
}

// TODO: type alias for precedence instead of u8?
fn precedence_by_token_type(token_type: &token::TokenType) -> u8 {
    match PRECEDENCES.get(token_type) {
        Some(precedence) => *precedence,
        None => token::LOWEST,
    }
}

// For debug visualization I migth potentially use this approach
// https://users.rust-lang.org/t/is-it-possible-to-implement-debug-for-fn-type/14824

// Greeting to the master of functinal Rust - mighty @raventid
type PrefixParseFnAlias = Fn(&mut Parser) -> token::Expression + 'static;

pub struct PrefixParseFn(Box<PrefixParseFnAlias>);
impl fmt::Debug for PrefixParseFn {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", "prefix_parse_fn")
    }
}

type InfixParseFnAlias = Fn(&mut Parser, token::Expression) -> token::Expression + 'static;

pub struct InfixParseFn(Box<InfixParseFnAlias>);
impl fmt::Debug for InfixParseFn {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", "infix_parse_fn")
    }
}

#[derive(Debug)]
pub struct LambdaParsers {
    pub prefix_parse_fns: HashMap<token::TokenType, PrefixParseFn>,
    pub infix_parse_fns: HashMap<token::TokenType, InfixParseFn>,
}

impl LambdaParsers {
    pub fn register_parsers(&mut self) {
        // PREFIX PARSERS
        self.register_prefix(
            token::IDENT.to_string(),
            Box::new(|parser| {
                token::Expression::Identifier(token::Identifier {
                    token: parser.current_token.clone(),
                    value: parser.current_token.literal.clone(),
                })
            }),
        );

        self.register_prefix(token::INT.to_string(), Box::new(Self::parse_int_literal));

        self.register_prefix(token::STRING.to_string(), Box::new(Self::parse_string_literal));

        self.register_prefix(
            token::BANG.to_string(),
            Box::new(Self::parse_prefix_expression),
        );

        self.register_prefix(
            token::MINUS.to_string(),
            Box::new(Self::parse_prefix_expression),
        );

        self.register_prefix(token::TRUE.to_string(), Box::new(Self::parse_boolean));

        self.register_prefix(token::FALSE.to_string(), Box::new(Self::parse_boolean));

        self.register_prefix(
            token::LPAREN.to_string(),
            Box::new(Self::parse_grouped_expressions),
        );

        self.register_prefix(token::IF.to_string(), Box::new(Self::parse_if_expression));

        self.register_prefix(
            token::FUNCTION.to_string(),
            Box::new(Self::parse_function_literal),
        );

        // INFIX PARSERS
        self.register_infix(
            token::PLUS.to_string(),
            Box::new(Self::parse_infix_expression),
        );

        self.register_infix(
            token::MINUS.to_string(),
            Box::new(Self::parse_infix_expression),
        );

        self.register_infix(
            token::SLASH.to_string(),
            Box::new(Self::parse_infix_expression),
        );

        self.register_infix(
            token::ASTERISK.to_string(),
            Box::new(Self::parse_infix_expression),
        );

        self.register_infix(
            token::EQ.to_string(),
            Box::new(Self::parse_infix_expression),
        );

        self.register_infix(
            token::NOT_EQ.to_string(),
            Box::new(Self::parse_infix_expression),
        );

        self.register_infix(
            token::LT.to_string(),
            Box::new(Self::parse_infix_expression),
        );

        self.register_infix(
            token::GT.to_string(),
            Box::new(Self::parse_infix_expression),
        );

        // By registering `(` handler as an infix
        // parser we allow it to parse CALL syntax.
        self.register_infix(
            token::LPAREN.to_string(),
            Box::new(Self::parse_call_expression),
        )
    }

    fn register_prefix(&mut self, token_type: token::TokenType, f: Box<PrefixParseFnAlias>) {
        self.prefix_parse_fns.insert(token_type, PrefixParseFn(f));
    }

    fn register_infix(&mut self, token_type: token::TokenType, f: Box<InfixParseFnAlias>) {
        self.infix_parse_fns.insert(token_type, InfixParseFn(f));
    }

    fn parse_int_literal(parser: &mut Parser) -> token::Expression {
        let to_be_integer = parser.current_token.literal.clone();

        // TODO: This extremly bad
        // Lambda parsers should bubble errors to parser.
        // Parser should handle them gracefully.
        // For the future:
        //
        // enum ParserError {
        //     FailedToReconiseIntegerLiteral(parse_int_error),
        //     FailedToObtainSomeValue(some_error_message),
        // }
        let integer = to_be_integer.parse::<i32>().unwrap();

        token::Expression::IntegerLiteral(token::IntegerLiteral {
            token: parser.current_token.clone(),
            value: integer,
        })
    }

    fn parse_string_literal(parser: &mut Parser) -> token::Expression {
        token::Expression::StringLiteral(token::StringLiteral {
            token: parser.current_token.clone(),
            value: parser.current_token.literal.clone(),
        })
    }

    fn parse_boolean(parser: &mut Parser) -> token::Expression {
        // TODO: extract?
        // fn (parser: &Parser) cur_token_is(t: token::TokenType) -> bool { p.cur_token.type == t }
        let boolean_value = parser.current_token.token_type == token::TRUE;

        token::Expression::Boolean(token::Boolean {
            token: parser.current_token.clone(),
            value: boolean_value,
        })
    }

    fn parse_prefix_expression(parser: &mut Parser) -> token::Expression {
        let mut lambda_parsers = LambdaParsers {
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };

        lambda_parsers.register_parsers();

        // We have to extract current token and operator
        // Because we'll move to next_token now.
        // To call parse_expression and get `right` expression.
        let token = parser.current_token.clone();
        let operator = parser.current_token.literal.clone();

        parser.next_token();

        // If we enter `parse_expression()` here without `next_token()`
        // we enter the endless loop, followed by stack overflow.
        // parse_expression() -> parse_prefix_expression() -> parse_expression()
        let expression = match parser.parse_expression(&lambda_parsers, token::PREFIX) {
            Some(result) => result,
            None => panic!(
                "Can't parse parser.current_token = {}",
                parser.current_token.literal
            ),
        };

        token::Expression::PrefixExpression(Box::new(token::PrefixExpression {
            token,
            operator,
            right: expression,
        }))
    }

    fn parse_infix_expression(parser: &mut Parser, left: token::Expression) -> token::Expression {
        // TODO: Reinitialization of parser here and in the `parse_prefix_expression`
        // Should move this initialization somewhere and use link everywhere else.
        let mut lambda_parsers = LambdaParsers {
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };

        lambda_parsers.register_parsers();

        let token = parser.current_token.clone();
        let operator = parser.current_token.literal.clone();

        let precedence = precedence_by_token_type(&token.token_type);

        parser.next_token();

        let right = match parser.parse_expression(&lambda_parsers, precedence) {
            Some(parsed_expression) => parsed_expression,
            None => panic!("Cannot find infix parser for {:?}", token),
        };

        // TODO: improve syntax with box-patterns?
        token::Expression::InfixExpression(Box::new(token::InfixExpression {
            token,
            left,
            operator,
            right,
        }))
    }

    #[trace]
    fn parse_grouped_expressions(parser: &mut Parser) -> token::Expression {
        // TODO: Reinitialization of parser here and in the `parse_prefix_expression`
        // Should move this initialization somewhere and use link everywhere else.
        let mut lambda_parsers = LambdaParsers {
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };
        lambda_parsers.register_parsers();

        // If we see `(` we enter here and move cursor to the next token.
        parser.next_token();

        let expression = match parser.parse_expression(&lambda_parsers, token::LOWEST) {
            Some(expression) => expression,
            None => panic!("Cannot find parser for {:?}", parser.current_token),
        };

        // TODO: move to smth like expect_peek()? which returns result
        // instead of panic!? Result is logged in panic.
        if parser.peek_token.token_type != token::RPAREN {
            // TODO: Rework function to properly handle this case.
            // Transofrm this to parser error.
            panic!(
                "I've expected `)`, but got {}",
                parser.peek_token.token_type
            );
        } else {
            // it's `)` token, skip it, we already parced expression in `(...)`
            parser.next_token();
        }

        expression
    }

    #[trace]
    fn parse_if_expression(parser: &mut Parser) -> token::Expression {
        // TODO: Reinitialization of parser here and in the `parse_prefix_expression`
        // Should move this initialization somewhere and use link everywhere else.
        let mut lambda_parsers = LambdaParsers {
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };
        lambda_parsers.register_parsers();

        let token = parser.current_token.clone();

        if parser.peek_token.token_type != token::LPAREN {
            panic!(
                "I've expected `(`, but got {}",
                parser.peek_token.token_type
            );
        };
        parser.next_token(); // set cursor to `(`

        parser.next_token(); // skip `(`

        // I should find the best place to record errors.
        // Use Result instead of Option. Log result in Parser { errors }.
        let condition = match parser.parse_expression(&lambda_parsers, token::LOWEST) {
            Some(c) => c,
            _ => panic!("failed to parse some condition"),
        };

        if parser.peek_token.token_type != token::RPAREN {
            panic!(
                "I've expected closing `)`, but got {}",
                parser.peek_token.token_type
            );
        };

        // next token is `)`, everything is fine, skip it
        parser.next_token();

        if parser.peek_token.token_type != token::LBRACE {
            panic!(
                "I've expected opening `{{`, but got {}",
                parser.peek_token.token_type
            );
        };

        // next token is `{`, everything is fine, skip it
        parser.next_token();

        let consequence = Self::parse_block_statement(parser);

        let alternative = if parser.peek_token.token_type == token::ELSE {
            parser.next_token(); // we found else! next token, pls!

            // This block is the same as one above
            // We should find LBRACE or panic! (we should not panic actually)
            if parser.peek_token.token_type != token::LBRACE {
                panic!(
                    "I've expected opening `{{`, but got {}",
                    parser.peek_token.token_type
                );
            };

            // next token is `{`, everything is fine, set cursor on it
            parser.next_token();

            Some(Self::parse_block_statement(parser))
        } else {
            None
        };

        token::Expression::IfExpression(Box::new(token::IfExpression {
            token,
            condition,
            consequence,
            alternative,
        }))
    }

    fn parse_function_literal(parser: &mut Parser) -> token::Expression {
        // Parse function parameters helper function.
        fn parse_function_parameters(parser: &mut Parser) -> Option<Vec<token::Identifier>> {
            if parser.peek_token.token_type == token::RPAREN {
                parser.next_token();
                return None;
            }

            parser.next_token(); // we already parsed `fn` here, so current token is `(`

            let identifier = token::Identifier {
                token: parser.current_token.clone(),
                value: parser.current_token.literal.clone(),
            };

            let mut identifiers = vec![identifier];

            dbg!(&identifiers);
            while parser.peek_token.token_type == token::COMMA {
                // next identifier exists
                parser.next_token(); // set cursor to comma
                parser.next_token(); // skip comma

                let identifier = token::Identifier {
                    token: parser.current_token.clone(),
                    value: parser.current_token.literal.clone(),
                };
                identifiers.push(identifier);
            }

            // I expect closing `)` after function arguments.
            // (a,b,c,d,e) <- this one
            if parser.peek_token.token_type != token::RPAREN {
                panic!(
                    "Expected closing `)`, got `{}`",
                    parser.peek_token.token_type
                );
            }
            parser.next_token();

            Some(identifiers)
        }

        let token = parser.current_token.clone();

        if parser.peek_token.token_type != token::LPAREN {
            panic!(
                "I've expected `(`, but got {}",
                parser.peek_token.token_type
            );
        };
        parser.next_token(); // set cursor to `(`

        let parameters = parse_function_parameters(parser);

        // This block is the same as one above
        // We should find LBRACE or panic! (we should not panic actually)
        if parser.peek_token.token_type != token::LBRACE {
            panic!(
                "I've expected opening `{{`, but got `{}`",
                parser.peek_token.token_type
            );
        };

        // next token is `{`, everything is fine, set cursor on it
        parser.next_token();

        let body = Self::parse_block_statement(parser);

        token::Expression::FunctionLiteral(token::FunctionLiteral {
            token,
            parameters,
            body,
        })
    }

    #[trace]
    fn parse_block_statement(parser: &mut Parser) -> token::BlockStatement {
        // TODO: Reinitialization of parser here and in the `parse_prefix_expression`
        // Should move this initialization somewhere and use link everywhere else.
        let mut lambda_parsers = LambdaParsers {
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };
        lambda_parsers.register_parsers();

        let token = parser.current_token.clone();
        let mut statements = Vec::new();

        // current token is `{` skip it
        parser.next_token();

        while parser.current_token.token_type != token::RBRACE
            && parser.current_token.token_type != token::EOF
        {
            let statement = match parser.parse_statement(&lambda_parsers) {
                Some(s) => s,
                None => panic!("useless panic one more time, failed to parse if block"),
            };
            statements.push(statement);
            parser.next_token();
        }

        token::BlockStatement { token, statements }
    }

    fn parse_call_expression(
        parser: &mut Parser,
        function: token::Expression,
    ) -> token::Expression {
        fn parse_call_arguments(parser: &mut Parser) -> Option<Vec<token::Expression>> {
            // TODO: Reinitialization of parser here and in the `parse_prefix_expression`
            // Should move this initialization somewhere and use link everywhere else.
            let mut lambda_parsers = LambdaParsers {
                prefix_parse_fns: HashMap::new(),
                infix_parse_fns: HashMap::new(),
            };
            lambda_parsers.register_parsers();

            if parser.peek_token.token_type == token::RPAREN {
                parser.next_token();
                return None;
            }

            parser.next_token(); // skip `RPAREN`

            let first_param = match parser.parse_expression(&lambda_parsers, token::LOWEST) {
                Some(expr) => expr,
                None => panic!("Failed to parse param for function CALL"),
            };

            let mut params = vec![first_param];

            while parser.peek_token.token_type == token::COMMA {
                parser.next_token(); // set cursor to `,`
                parser.next_token(); // skip `,` and move cursor to next token

                let param = match parser.parse_expression(&lambda_parsers, token::LOWEST) {
                    Some(expr) => expr,
                    None => panic!("Failed to parse param for function CALL"),
                };

                params.push(param);
            }

            if parser.peek_token.token_type != token::RPAREN {
                panic!("Expected closing `)` after params, got {}", parser.peek_token.token_type);
            }
            parser.next_token(); // set cursor to RPAREN and leave parser

            Some(params)
        }

        let token = parser.current_token.clone();
        let arguments = parse_call_arguments(parser);

        token::Expression::CallExpression(Box::new(token::CallExpression {
            token,
            function,
            arguments,
        }))
    }
}

#[derive(Debug)]
pub struct Parser {
    lexer: lexer::Lexer,
    current_token: token::Token,
    peek_token: token::Token,
    pub errors: Vec<String>,
}

impl Parser {
    pub fn new(mut lexer: lexer::Lexer) -> Self {
        let current_token = lexer.next_token();
        let peek_token = lexer.next_token();
        let errors = Vec::new();

        Self {
            lexer,
            current_token,
            peek_token,
            errors,
        }
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    // TODO: Current attempt. Move link to LambdaParsers to every function.
    // To avoid double borrowing of self in case of mutual recursive
    // calls.
    pub fn parse_program(&mut self, lambda_parsers: &LambdaParsers) -> ast::Program {
        let mut program = ast::Program {
            statements: Vec::new(),
        };
        while self.current_token.token_type != token::EOF {
            let statement = self.parse_statement(lambda_parsers);
            match statement {
                Some(stmt) => program.statements.push(stmt),
                _ => (), // Just ignore that case
            };
            self.next_token();
        }
        program
    }

    fn parse_statement(&mut self, lambda_parsers: &LambdaParsers) -> Option<token::Statements> {
        match self.current_token.token_type.as_ref() {
            token::LET => match self.parse_let_statement() {
                Some(stmt) => Some(token::Statements::LetStatement(stmt)),
                _ => None,
            },
            token::RETURN => match self.parse_return_statement() {
                Some(stmt) => Some(token::Statements::ReturnStatement(stmt)),
                _ => None,
            },
            // If we did not encounter any `let` or `return` it might've happened that
            // we've encountered another type of statement.
            // The last one in our language - expresion statement.
            _ => match self.parse_expression_statement(lambda_parsers) {
                Some(stmt) => Some(token::Statements::ExpressionStatement(stmt)),
                _ => None,
            },
        }
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

        self.next_token(); // skip ASSIGN `=` in let statement

        // TODO: Very bad, we have to create lambda parsers here.
        // I will fix it soon.
        let mut lambda_parsers = LambdaParsers {
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };
        lambda_parsers.register_parsers();

        let value = match self.parse_expression(&lambda_parsers, token::LOWEST) {
            Some(expr) => expr,
            None => panic!("FAILED TO PARSE {}", self.current_token.token_type),
        };

        if self.peek_token.token_type == token::SEMICOLON {
            self.next_token(); // set cursor to SEMICOLON
        }
        // TODO: It's a fragile design for now, this code might hang if we don't
        // have a terminating semicolon and next token is token::EOF
        // in this case we'll enter an infinite loop.
        // Doesn't next_token() protect us from this? Apparently - not.
        //
        // UPD: Now as we parse expression here we do not care about
        // this corner case that much, but I think we should care about it
        // in the future. So I'll leave this note here.
        //
        // while !(self.current_token.token_type == token::SEMICOLON) {
        //     self.next_token(); // skip to next statement in our program
        // }

        // TODO: Same happens in return parser. I'm skipping
        // semicolon, so in `parse_statement` function I can
        // just start to parse next value.
        // Weird, it does not work here that way.
        // self.next_token();

        Some(token::LetStatement {
            token,
            name,
            value,
        })
    }

    // TODO: Why Option here?
    fn parse_return_statement(&mut self) -> Option<token::ReturnStatement> {
        let token = self.current_token.clone();

        self.next_token();

        // TODO: Very bad, we have to create lambda parsers here.
        // I will fix it soon.
        let mut lambda_parsers = LambdaParsers {
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };
        lambda_parsers.register_parsers();

        let return_value = match self.parse_expression(&lambda_parsers, token::LOWEST) {
            Some(expr) => expr,
            None => panic!("UNREACHABLE: return statement parser with {}", self.current_token.token_type),
        };

        if self.peek_token.token_type == token::SEMICOLON {
            self.next_token(); // set cursor to semicolon if any
        }

        let statement = token::ReturnStatement {
            token,
            return_value,
        };

        Some(statement)
    }

    fn parse_expression_statement(
        &mut self,
        lambda_parsers: &LambdaParsers,
    ) -> Option<token::ExpressionStatement> {
        let statement = token::ExpressionStatement {
            token: self.current_token.clone(),
            expression: match self.parse_expression(lambda_parsers, token::LOWEST) {
                Some(expression) => expression,
                None => panic!("I don't know how to parse `{}`", self.current_token.literal),
            },
        };

        if self.peek_token.token_type == token::SEMICOLON {
            self.next_token();
        }

        Some(statement)
    }

    #[trace]
    fn parse_expression(
        &mut self,
        lambda_parsers: &LambdaParsers,
        precedence: u8,
    ) -> Option<token::Expression> {
        let prefix_function = lambda_parsers
            .prefix_parse_fns
            .get(&self.current_token.token_type.clone());

        let mut left = match prefix_function {
            Some(PrefixParseFn(prefix_parse_fn)) => prefix_parse_fn(self),
            None => {
                // this step might be redundant, because we check the error above
                self.register_no_prefix_parser_found(self.current_token.token_type.clone());
                return None;
            }
        };

        while !(self.peek_token.token_type == token::SEMICOLON)
            && (precedence < precedence_by_token_type(&self.peek_token.token_type))
        {
            let infix_function = lambda_parsers
                .infix_parse_fns
                .get(&self.peek_token.token_type.clone());

            // change cursor position before calling infix_parse_fn
            // call it with new position
            self.next_token();

            // update left
            left = match infix_function {
                Some(InfixParseFn(infix_parse_fn)) => infix_parse_fn(self, left.clone()),
                None => panic!("Cannot find infix function for {:?}", self.peek_token),
            };
        }

        Some(left)
    }

    fn register_no_prefix_parser_found(&mut self, token_type: token::TokenType) {
        let message = format!("no prefix parser found for {} token", token_type);
        self.errors.push(message);
    }

    fn peek_error(&mut self, token: token::TokenType) {
        let message = format!(
            "expected next token to be {expected}, got {got} instead",
            expected = token,
            got = self.peek_token.token_type,
        );

        self.errors.push(message);
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::Node;
    use crate::lexer;
    use crate::parser::LambdaParsers;
    use crate::parser::Parser;
    use crate::token::Expression;
    use crate::token::Statements;
    use std::collections::HashMap;

    #[test]
    fn test_let_statements() {
        let input = r###"
          let a = 1;
          let b = 2;
          let bebe = 101010;
        "###
        .to_string();

        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let mut lambda_parsers = LambdaParsers {
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };

        lambda_parsers.register_parsers();

        let program = parser.parse_program(&lambda_parsers);

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

        let expected = vec![
            ("a".to_string(), 1),
            ("b".to_string(), 2),
            ("bebe".to_string(), 101010),
        ];

        program
            .statements
            .into_iter()
            .zip(expected.into_iter())
            .for_each(|(statement, (expected_identifier, integer))| {
                assert_eq!(statement.token_literal(), "let");

                let let_statement = match statement {
                    Statements::LetStatement(statement) => statement,
                    _ => panic!("I didn't expected anything besides `let` statement"),
                };

                assert_eq!(let_statement.name.value, expected_identifier);
                assert_eq!(let_statement.name.token_literal(), expected_identifier);
                assert_integer_literal(&let_statement.value, integer);
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

        let mut lambda_parsers = LambdaParsers {
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };

        lambda_parsers.register_parsers();

        parser.parse_program(&lambda_parsers);

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

        let mut lambda_parsers = LambdaParsers {
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };

        lambda_parsers.register_parsers();

        let program = parser.parse_program(&lambda_parsers);

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

        let expected = vec![1, 111];

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

                assert_integer_literal(&return_statement.return_value, expected_identifier);
            });
    }

    #[test]
    fn test_identifier_expression() {
        let input = r###"
          bebe;
        "###
        .to_string();

        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let mut lambda_parsers = LambdaParsers {
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };

        lambda_parsers.register_parsers();

        let program = parser.parse_program(&lambda_parsers);

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

        program.statements.into_iter().for_each(|statement| {
            let expression_statement = match statement {
                Statements::ExpressionStatement(statement) => statement,
                _ => panic!("I didn't expect something besides expression statement"),
            };

            let identifier = match expression_statement.expression {
                Expression::Identifier(i) => i,
                _ => panic!("expected to find identifier, but found smth else"),
            };
            // TODO: Maybe pattern matching on concrete branch is better here
            // then some general value() method.
            // Anyway in control code I will use it the other way.
            assert_eq!(identifier.value, "bebe");
            assert_eq!(identifier.token_literal(), "bebe");
        })
    }

    #[test]
    fn test_int_literal_expression() {
        let input = r###"
          42;
        "###
        .to_string();

        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let mut lambda_parsers = LambdaParsers {
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };

        lambda_parsers.register_parsers();

        let program = parser.parse_program(&lambda_parsers);

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

        program.statements.into_iter().for_each(|statement| {
            let expression_statement = match statement {
                Statements::ExpressionStatement(statement) => statement,
                _ => panic!("I didn't expect something besides expression statement"),
            };

            let integer_literal = match expression_statement.expression {
                Expression::IntegerLiteral(il) => il,
                _ => panic!("expected to find an integer_literal, but found smth else"),
            };
            // TODO: Maybe pattern matching on concrete branch is better here
            // then some general value() method.
            // Anyway in control code I will use it the other way.
            assert_eq!(integer_literal.value, 42);
            assert_eq!(integer_literal.token_literal(), "42");
        })
    }

    #[test]
    fn test_string_literal_expression() {
        let input = r###"
          "kobushka";
        "###
        .to_string();

        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let mut lambda_parsers = LambdaParsers {
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };

        lambda_parsers.register_parsers();

        let program = parser.parse_program(&lambda_parsers);

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

        program.statements.into_iter().for_each(|statement| {
            let expression_statement = match statement {
                Statements::ExpressionStatement(statement) => statement,
                _ => panic!("I didn't expect something besides expression statement"),
            };

            let string_literal = match expression_statement.expression {
                Expression::StringLiteral(sl) => sl,
                _ => panic!("expected to find an string_literal, but found smth else"),
            };
            // TODO: Maybe pattern matching on concrete branch is better here
            // then some general value() method.
            // Anyway in control code I will use it the other way.
            assert_eq!(string_literal.value, "kobushka".to_string());
            assert_eq!(string_literal.token_literal(), "kobushka");
        })
    }

    #[test]
    fn test_prefix_expressions() {
        let inputs = ["!10;".to_string(), "-101010;".to_string()];

        let expected = vec![("!".to_string(), 10), ("-".to_string(), 101010)];

        // Iterate over every prefix expression and test it individualy
        inputs
            .into_iter()
            .zip(expected.into_iter())
            .for_each(|(input, token_pair)| {
                let lexer = lexer::Lexer::new(input.to_string());
                let mut parser = Parser::new(lexer);

                let mut lambda_parsers = LambdaParsers {
                    prefix_parse_fns: HashMap::new(),
                    infix_parse_fns: HashMap::new(),
                };

                lambda_parsers.register_parsers();

                let program = parser.parse_program(&lambda_parsers);

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

                let expression_statement = match &program.statements[0] {
                    Statements::ExpressionStatement(statement) => statement,
                    _ => panic!("I didn't expected anything besides `expression` statement"),
                };

                let prefix_expression = match &expression_statement.expression {
                    Expression::PrefixExpression(pe) => pe,
                    _ => panic!("I've expected prefix expression here - sorry"),
                };

                let operator = token_pair.0;
                let integer = token_pair.1;

                assert_eq!(prefix_expression.operator, operator);
                assert_integer_literal(&prefix_expression.right, integer);
            });
    }

    #[test]
    fn test_infix_expressions() {
        let inputs = [
            "1 + 1;".to_string(),
            "1 - 1;".to_string(),
            "1 * 1;".to_string(),
            "1 / 1;".to_string(),
            "1 > 1;".to_string(),
            "1 < 1;".to_string(),
            "1 == 1;".to_string(),
            "1 != 1;".to_string(),
        ];

        let expected = vec![
            (1, "+".to_string(), 1),
            (1, "-".to_string(), 1),
            (1, "*".to_string(), 1),
            (1, "/".to_string(), 1),
            (1, ">".to_string(), 1),
            (1, "<".to_string(), 1),
            (1, "==".to_string(), 1),
            (1, "!=".to_string(), 1),
        ];

        // Iterate over every prefix expression and test it individualy
        inputs.into_iter().zip(expected.into_iter()).for_each(
            |(input, (left_integer, operator, right_integer))| {
                let lexer = lexer::Lexer::new(input.to_string());
                let mut parser = Parser::new(lexer);

                let mut lambda_parsers = LambdaParsers {
                    prefix_parse_fns: HashMap::new(),
                    infix_parse_fns: HashMap::new(),
                };

                lambda_parsers.register_parsers();

                let program = parser.parse_program(&lambda_parsers);

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

                let expression_statement = match &program.statements[0] {
                    Statements::ExpressionStatement(statement) => statement,
                    _ => panic!("I didn't expected anything besides `expression` statement"),
                };

                let infix_expression = match &expression_statement.expression {
                    Expression::InfixExpression(ie) => ie,
                    _ => panic!("I've expected infix expression here - sorry"),
                };

                assert_integer_literal(&infix_expression.left, left_integer);
                assert_eq!(infix_expression.operator, operator);
                assert_integer_literal(&infix_expression.right, right_integer);
            },
        );
    }

    #[test]
    fn test_boolean_expressions() {
        let inputs = ["true;".to_string(), "false;".to_string()];

        let expected = vec![true, false];

        // Iterate over every prefix expression and test it individualy
        inputs
            .into_iter()
            .zip(expected.into_iter())
            .for_each(|(input, boolean_value)| {
                let lexer = lexer::Lexer::new(input.to_string());
                let mut parser = Parser::new(lexer);

                let mut lambda_parsers = LambdaParsers {
                    prefix_parse_fns: HashMap::new(),
                    infix_parse_fns: HashMap::new(),
                };

                lambda_parsers.register_parsers();

                let program = parser.parse_program(&lambda_parsers);

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

                let expression_statement = match &program.statements[0] {
                    Statements::ExpressionStatement(statement) => statement,
                    _ => panic!("I didn't expected anything besides `expression` statement"),
                };

                let boolean_expression = match &expression_statement.expression {
                    Expression::Boolean(b) => b,
                    _ => panic!("I've expected boolean expression here - sorry"),
                };

                assert_eq!(boolean_expression.value, boolean_value);
            });
    }

    #[test]
    fn test_operator_precedence() {
        let inputs = [
            "(1 + 2) * 3 + 4;".to_string(),
            "!true == false;".to_string(),
        ];

        let expected = [
            "(((1 + 2) * 3) + 4)\n".to_string(),
            "((! true) == false)\n".to_string(),
        ];

        // Iterate over every prefix expression and test it individualy
        inputs
            .into_iter()
            .zip(expected.into_iter())
            .for_each(|(input, expected)| {
                let lexer = lexer::Lexer::new(input.to_string());
                let mut parser = Parser::new(lexer);

                let mut lambda_parsers = LambdaParsers {
                    prefix_parse_fns: HashMap::new(),
                    infix_parse_fns: HashMap::new(),
                };

                lambda_parsers.register_parsers();

                let program = parser.parse_program(&lambda_parsers);

                // We would like to accumulate every error in program
                // and later render them to user.
                if !parser.errors.is_empty() {
                    println!("Parser encountered {} errors", parser.errors.len());
                    for error in parser.errors {
                        println!("parser error: {}", error);
                    }
                    panic!("A few parsing error encountered, see them above.");
                }

                // This and a couple of next tests will be run with
                // stringification in mind. Like this assertion:
                assert_eq!(program.to_string(), *expected);
            });
    }

    #[test]
    fn test_if_then_expression() {
        let inputs = ["if (pirozhenka < bulochka) { bulochka }".to_string()];

        // Iterate over every prefix expression and test it individualy
        inputs.into_iter().for_each(|input| {
            let lexer = lexer::Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);

            let mut lambda_parsers = LambdaParsers {
                prefix_parse_fns: HashMap::new(),
                infix_parse_fns: HashMap::new(),
            };

            lambda_parsers.register_parsers();

            let program = parser.parse_program(&lambda_parsers);

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

            let if_statement = match &program.statements[0] {
                Statements::ExpressionStatement(statement) => statement,
                _ => panic!("I didn't expected anything besides `expression` statement"),
            };

            let if_expression = match &if_statement.expression {
                Expression::IfExpression(b) => b,
                _ => panic!("I've expected IF expression here - sorry"),
            };

            // Test that we've correctly parsed condition (it might be any expression)
            test_infix_expression(
                &if_expression.condition,
                ExpectedAssertLiteral::S("pirozhenka".to_string()),
                "<".to_string(),
                ExpectedAssertLiteral::S("bulochka".to_string()),
            );

            assert_eq!(if_expression.consequence.statements.len(), 1);

            let consequence = match &if_expression.consequence.statements[0] {
                Statements::ExpressionStatement(c) => c,
                _ => panic!("tried to extract expression statement from consequence and failed"),
            };

            assert_identifier(&consequence.expression, "bulochka".to_string());

            assert!(if_expression.alternative.is_none());
        });
    }

    #[test]
    fn test_if_then_else_expression() {
        let inputs = ["if (pirozhenka < bulochka) { bulochka } else { pirozhenka }".to_string()];

        // Iterate over every prefix expression and test it individualy
        inputs.into_iter().for_each(|input| {
            let lexer = lexer::Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);

            let mut lambda_parsers = LambdaParsers {
                prefix_parse_fns: HashMap::new(),
                infix_parse_fns: HashMap::new(),
            };

            lambda_parsers.register_parsers();

            let program = parser.parse_program(&lambda_parsers);

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

            let if_statement = match &program.statements[0] {
                Statements::ExpressionStatement(statement) => statement,
                _ => panic!("I didn't expected anything besides `expression` statement"),
            };

            let if_expression = match &if_statement.expression {
                Expression::IfExpression(b) => b,
                _ => panic!("I've expected IF expression here - sorry"),
            };

            // Test that we've correctly parsed condition (it might be any expression)
            test_infix_expression(
                &if_expression.condition,
                ExpectedAssertLiteral::S("pirozhenka".to_string()),
                "<".to_string(),
                ExpectedAssertLiteral::S("bulochka".to_string()),
            );

            assert_eq!(if_expression.consequence.statements.len(), 1);

            let consequence = match &if_expression.consequence.statements[0] {
                Statements::ExpressionStatement(c) => c,
                _ => panic!("tried to extract expression statement from consequence and failed"),
            };

            assert_identifier(&consequence.expression, "bulochka".to_string());

            let alternative = match &if_expression.alternative {
                Some(block) => match &block.statements[0] {
                    Statements::ExpressionStatement(c) => c,
                    _ => {
                        panic!("tried to extract expression statement from alternative and failed")
                    }
                },
                None => panic!("failed to parse block"),
            };

            assert_identifier(&alternative.expression, "pirozhenka".to_string());
        });
    }

    #[test]
    fn test_function_literal_with_no_params_expression() {
        let inputs = ["fn() { pirozhenka; }".to_string()];

        // Iterate over every prefix expression and test it individualy
        inputs.into_iter().for_each(|input| {
            let lexer = lexer::Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);

            let mut lambda_parsers = LambdaParsers {
                prefix_parse_fns: HashMap::new(),
                infix_parse_fns: HashMap::new(),
            };

            lambda_parsers.register_parsers();

            let program = parser.parse_program(&lambda_parsers);

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

            let expression_statement = match &program.statements[0] {
                Statements::ExpressionStatement(statement) => statement,
                _ => panic!("I didn't expected anything besides `expression` statement"),
            };

            let fn_literal = match &expression_statement.expression {
                Expression::FunctionLiteral(f) => f,
                _ => panic!("I've expected FN literal here - sorry"),
            };

            assert!(fn_literal.parameters.is_none());

            assert_eq!(fn_literal.body.statements.len(), 1);

            let body_statements = match fn_literal.body.statements[0].clone() {
                Statements::ExpressionStatement(stmt) => stmt,
                _ => panic!(
                    "Expected expression statement, got {}",
                    fn_literal.body.statements[0]
                ),
            };

            assert_eq!(body_statements.expression.token_literal(), "pirozhenka");
        });
    }

    #[test]
    fn test_function_literal_with_one_params_expression() {
        let inputs = ["fn(pirozhenka) { pirozhenka; }".to_string()];

        // Iterate over every prefix expression and test it individualy
        inputs.into_iter().for_each(|input| {
            let lexer = lexer::Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);

            let mut lambda_parsers = LambdaParsers {
                prefix_parse_fns: HashMap::new(),
                infix_parse_fns: HashMap::new(),
            };

            lambda_parsers.register_parsers();

            let program = parser.parse_program(&lambda_parsers);

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

            let expression_statement = match &program.statements[0] {
                Statements::ExpressionStatement(statement) => statement,
                _ => panic!("I didn't expected anything besides `expression` statement"),
            };

            let fn_literal = match &expression_statement.expression {
                Expression::FunctionLiteral(f) => f,
                _ => panic!("I've expected FN literal here - sorry"),
            };

            assert_eq!(fn_literal.parameters.clone().unwrap().len(), 1);

            assert_eq!(
                &fn_literal.parameters.clone().unwrap()[0].value,
                "pirozhenka"
            );

            assert_eq!(fn_literal.body.statements.len(), 1);

            let body_statements = match fn_literal.body.statements[0].clone() {
                Statements::ExpressionStatement(stmt) => stmt,
                _ => panic!(
                    "Expected expression statement, got {}",
                    fn_literal.body.statements[0]
                ),
            };

            assert_eq!(body_statements.expression.token_literal(), "pirozhenka");
        });
    }

    #[test]
    fn test_function_literal_with_two_params_expression() {
        let inputs = ["fn(pirozhenka, bulochka) { pirozhenka + bulochka; }".to_string()];

        // Iterate over every prefix expression and test it individualy
        inputs.into_iter().for_each(|input| {
            let lexer = lexer::Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);

            let mut lambda_parsers = LambdaParsers {
                prefix_parse_fns: HashMap::new(),
                infix_parse_fns: HashMap::new(),
            };

            lambda_parsers.register_parsers();

            let program = parser.parse_program(&lambda_parsers);

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

            let expression_statement = match &program.statements[0] {
                Statements::ExpressionStatement(statement) => statement,
                _ => panic!("I didn't expected anything besides `expression` statement"),
            };

            let fn_literal = match &expression_statement.expression {
                Expression::FunctionLiteral(f) => f,
                _ => panic!("I've expected FN literal here - sorry"),
            };

            assert_eq!(fn_literal.parameters.clone().unwrap().len(), 2);

            assert_eq!(
                &fn_literal.parameters.clone().unwrap()[0].value,
                "pirozhenka"
            );
            assert_eq!(&fn_literal.parameters.clone().unwrap()[1].value, "bulochka");

            assert_eq!(fn_literal.body.statements.len(), 1);

            let body_statements = match fn_literal.body.statements[0].clone() {
                Statements::ExpressionStatement(stmt) => stmt,
                _ => panic!(
                    "Expected expression statement, got {}",
                    fn_literal.body.statements[0]
                ),
            };

            test_infix_expression(
                &body_statements.expression,
                ExpectedAssertLiteral::S("pirozhenka".to_string()),
                "+".to_string(),
                ExpectedAssertLiteral::S("bulochka".to_string()),
            );
        });
    }

    #[test]
    fn test_call_expression() {
        let inputs = ["sdelay_pirozhenku(muka, sahar, slivki + ricotta)".to_string()];

        // Iterate over every prefix expression and test it individualy
        inputs.into_iter().for_each(|input| {
            let lexer = lexer::Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);

            let mut lambda_parsers = LambdaParsers {
                prefix_parse_fns: HashMap::new(),
                infix_parse_fns: HashMap::new(),
            };

            lambda_parsers.register_parsers();

            let program = parser.parse_program(&lambda_parsers);

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

            let expression_statement = match &program.statements[0] {
                Statements::ExpressionStatement(statement) => statement,
                _ => panic!("I didn't expected anything besides `expression` statement"),
            };

            let call_expression = match &expression_statement.expression {
                Expression::CallExpression(ce) => ce,
                _ => panic!("I've expected CALL expression here - sorry"),
            };

            assert_identifier(&call_expression.function, "sdelay_pirozhenku".to_string());
            assert_eq!(call_expression.arguments.clone().unwrap().len(), 3);
            assert_literal_expression(
                &call_expression.arguments.clone().unwrap()[0],
                ExpectedAssertLiteral::S("muka".to_string()),
            );
            assert_literal_expression(
                &call_expression.arguments.clone().unwrap()[1],
                ExpectedAssertLiteral::S("sahar".to_string()),
            );
            test_infix_expression(
                &call_expression.arguments.clone().unwrap()[2],
                ExpectedAssertLiteral::S("slivki".to_string()),
                "+".to_string(),
                ExpectedAssertLiteral::S("ricotta".to_string()),
            );
        });
    }

    //**********************************************
    //**********************************************
    //**********************************************
    //*********<<-- HELPER ASSERTIONS -->>**********
    //**********************************************
    //**********************************************
    //**********************************************
    fn assert_integer_literal(expression: &Expression, expected: i32) {
        let integer = match expression {
            Expression::IntegerLiteral(il) => il,
            _ => panic!("fail in assert_integer_literal"),
        };

        assert_eq!(integer.value, expected);
    }

    fn assert_identifier(expression: &Expression, expected: String) {
        let identifier = match expression {
            Expression::Identifier(identifier) => identifier,
            _ => panic!("Expected Identifier, got {:?}", expression),
        };

        assert_eq!(identifier.value, expected)
    }

    fn assert_boolean_literal(expression: &Expression, expected: bool) {
        let boolean_expression = match expression {
            Expression::Boolean(boolean) => boolean,
            _ => panic!("Expected boolean, got {:?}", expression),
        };

        assert_eq!(boolean_expression.value, expected);
        assert_eq!(boolean_expression.token_literal(), expected.to_string())
    }

    enum ExpectedAssertLiteral {
        S(String),
        I(i32),
        B(bool),
    }

    fn assert_literal_expression(expression: &Expression, expected: ExpectedAssertLiteral) {
        match expected {
            ExpectedAssertLiteral::S(v) => assert_identifier(expression, v),
            ExpectedAssertLiteral::I(v) => assert_integer_literal(expression, v),
            ExpectedAssertLiteral::B(v) => assert_boolean_literal(expression, v),
        };
    }

    fn test_infix_expression(
        expression: &Expression,
        left: ExpectedAssertLiteral,
        operator: String,
        right: ExpectedAssertLiteral,
    ) {
        let infix_expression = match expression {
            Expression::InfixExpression(e) => e,
            _ => panic!("Expected infix expression, got {:?}", expression),
        };

        assert_literal_expression(&infix_expression.left, left);

        assert_eq!(infix_expression.operator, operator);

        assert_literal_expression(&infix_expression.right, right);
    }
}
