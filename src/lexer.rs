use crate::token;
use std::str;

#[derive(Default, Debug)]
struct Lexer {
    input: String,
    position: u32,
    read_position: u32,
    ch: u8,
}

impl Lexer {
    fn new(input: String) -> Self {
        let mut lexer = Lexer {
            input,
            ..Default::default()
        };

        // Setup lexer:
        // 1) load first character
        // 2) move reader position
        // ...see read_char for details.
        lexer.read_char();

        lexer
    }

    // At the beginning position and read_position are zeroes.
    // After the first iteration situation changes to position: 0, read_position: 1.
    fn read_char(&mut self) {
        if self.read_position >= self.input.len() as u32 {
            self.ch = 0
        } else {
            self.ch = self.input.as_bytes()[self.read_position as usize]
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    // TODO: rethink types. Current one are not very nice.
    fn next_token(&mut self) -> token::Token {
        let tok = match self.ch {
            b'=' => token::Token {
                token_type: token::ASSIGN.to_string(),
                literal: [self.ch].iter().map(|&c| c as char).collect::<String>(),
            },
            b';' => token::Token {
                token_type: token::SEMICOLON.to_string(),
                literal: [self.ch].iter().map(|&c| c as char).collect::<String>(),
            },
            b'(' => token::Token {
                token_type: token::LPAREN.to_string(),
                literal: [self.ch].iter().map(|&c| c as char).collect::<String>(),
            },
            b')' => token::Token {
                token_type: token::RPAREN.to_string(),
                literal: [self.ch].iter().map(|&c| c as char).collect::<String>(),
            },
            b',' => token::Token {
                token_type: token::COMMA.to_string(),
                literal: [self.ch].iter().map(|&c| c as char).collect::<String>(),
            },
            b'+' => token::Token {
                token_type: token::PLUS.to_string(),
                literal: [self.ch].iter().map(|&c| c as char).collect::<String>(),
            },
            b'{' => token::Token {
                token_type: token::LBRACE.to_string(),
                literal: [self.ch].iter().map(|&c| c as char).collect::<String>(),
            },
            b'}' => token::Token {
                token_type: token::RBRACE.to_string(),
                literal: [self.ch].iter().map(|&c| c as char).collect::<String>(),
            },
            0 => token::Token {
                token_type: token::EOF.to_string(),
                literal: "".to_string(),
            },
            _ => panic!("Unexpected character in lexer"),
        };

        self.read_char();

        tok
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::token;

    #[test]
    fn test_next_token() {
        let input = String::from("=+(){},;");

        let expected = vec![
            (token::ASSIGN.to_string(), String::from("=")),
            (token::PLUS.to_string(), String::from("+")),
            (token::LPAREN.to_string(), String::from("(")),
            (token::RPAREN.to_string(), String::from(")")),
            (token::LBRACE.to_string(), String::from("{")),
            (token::RBRACE.to_string(), String::from("}")),
            (token::COMMA.to_string(), String::from(",")),
            (token::SEMICOLON.to_string(), String::from(";")),
            (token::EOF.to_string(), String::from("")),
        ];

        let mut lexer = Lexer::new(input);

        expected.into_iter().for_each(|(token_type, literal)| {
            let token = lexer.next_token();

            assert_eq!(token.token_type, token_type);
            assert_eq!(token.literal, literal);
        });
    }
}
