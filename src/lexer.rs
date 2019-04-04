use crate::token;

// TODO: Why byte? Maybe just use char and support unicode language out of box? wtp?
#[derive(Default, Debug, Clone)]
pub struct Lexer {
    input: String,
    position: u32,
    read_position: u32,
    ch: u8,
}

impl Lexer {
    pub fn new(input: String) -> Self {
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
    pub fn next_token(&mut self) -> token::Token {
        self.skip_whitespace();

        let tok = match self.ch {
            b'=' => {
                if self.peek_char() == b'=' {
                    let ch = self.ch;
                    self.read_char();
                    let literal = [ch, self.ch].iter().map(|&c| c as char).collect::<String>();
                    token::Token {
                        token_type: token::EQ.to_string(),
                        literal,
                    }
                } else {
                    token::Token {
                        token_type: token::ASSIGN.to_string(),
                        literal: [self.ch].iter().map(|&c| c as char).collect::<String>(),
                    }
                }
            }
            b'+' => token::Token {
                token_type: token::PLUS.to_string(),
                literal: [self.ch].iter().map(|&c| c as char).collect::<String>(),
            },
            b'-' => token::Token {
                token_type: token::MINUS.to_string(),
                literal: [self.ch].iter().map(|&c| c as char).collect::<String>(),
            },
            b'!' => {
                if self.peek_char() == b'=' {
                    let ch = self.ch;
                    self.read_char();
                    let literal = [ch, self.ch].iter().map(|&c| c as char).collect::<String>();
                    token::Token {
                        token_type: token::NOT_EQ.to_string(),
                        literal,
                    }
                } else {
                    token::Token {
                        token_type: token::BANG.to_string(),
                        literal: [self.ch].iter().map(|&c| c as char).collect::<String>(),
                    }
                }
            }
            b'/' => token::Token {
                token_type: token::SLASH.to_string(),
                literal: [self.ch].iter().map(|&c| c as char).collect::<String>(),
            },
            b'*' => token::Token {
                token_type: token::ASTERISK.to_string(),
                literal: [self.ch].iter().map(|&c| c as char).collect::<String>(),
            },
            b'<' => token::Token {
                token_type: token::LT.to_string(),
                literal: [self.ch].iter().map(|&c| c as char).collect::<String>(),
            },
            b'>' => token::Token {
                token_type: token::GT.to_string(),
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
            b'{' => token::Token {
                token_type: token::LBRACE.to_string(),
                literal: [self.ch].iter().map(|&c| c as char).collect::<String>(),
            },
            b'}' => token::Token {
                token_type: token::RBRACE.to_string(),
                literal: [self.ch].iter().map(|&c| c as char).collect::<String>(),
            },
            b'"' => token::Token {
                token_type: token::STRING.to_string(),
                literal: self.read_string(),
            },
            b'[' => token::Token {
                token_type: token::LBRACKET.to_string(),
                literal: [self.ch].iter().map(|&c| c as char).collect::<String>(),
            },
            b']' => token::Token {
                token_type: token::RBRACKET.to_string(),
                literal: [self.ch].iter().map(|&c| c as char).collect::<String>(),
            },
            0 => token::Token {
                token_type: token::EOF.to_string(),
                literal: "".to_string(),
            },
            _ => {
                let character = self.ch as char;

                if character.is_ascii_alphabetic() || character == '_' {
                    let literal = self.read_identifier();
                    let token_type = token::lookup_ident(literal.clone());
                    // We need early return here because
                    // in the end of function we run read_char(),
                    // but read_identifier() already at last position
                    return token::Token {
                        token_type,
                        literal,
                    };
                } else if character.is_digit(10) {
                    // TODO: Add linenumber and position to track the error
                    // and show output to user.
                    let literal = self.read_number();
                    let token_type = token::INT.to_string();
                    // We need early return here because
                    // in the end of function we run read_char(),
                    // but read_number() already at last position
                    return token::Token {
                        token_type,
                        literal,
                    };
                } else {
                    token::Token {
                        token_type: token::ILLEGAL.to_string(),
                        literal: [self.ch].iter().map(|&c| c as char).collect::<String>(),
                    }
                }
            }
        };

        self.read_char();

        tok
    }

    fn read_identifier(&mut self) -> String {
        let position = self.position;
        while self.is_letter(self.ch) {
            self.read_char()
        }
        self.input.as_bytes()[position as usize..self.position as usize]
            .iter()
            .map(|&c| c as char)
            .collect::<String>()
    }

    fn read_string(&mut self) -> String {
        let position = self.position + 1; // ignore opening `"`

        loop {
            self.read_char();

            if self.ch == b'"' || self.ch == 0 {
                break
            }
        }

        self.input.as_bytes()[position as usize..self.position as usize]
            .iter()
            .map(|&c| c as char)
            .collect::<String>()
    }

    fn read_number(&mut self) -> String {
        let position = self.position;
        while (self.ch as char).is_digit(10) {
            self.read_char()
        }

        self.input.as_bytes()[position as usize..self.position as usize]
            .iter()
            .map(|&c| c as char)
            .collect::<String>()
    }

    fn peek_char(&self) -> u8 {
        if self.read_position >= self.input.len() as u32 {
            0
        } else {
            self.input.as_bytes()[self.read_position as usize]
        }
    }

    fn is_letter(&self, c: u8) -> bool {
        let character = c as char;
        character.is_ascii_alphabetic() || character == '_'
    }

    fn skip_whitespace(&mut self) {
        while self.ch == b' ' || self.ch == b'\t' || self.ch == b'\n' || self.ch == b'\r' {
            self.read_char()
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::token;

    #[test]
    fn test_next_token_basic() {
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

    #[test]
    fn test_next_token_clojurium_snippet() {
        let input = r###"
          let five = 5;
          let ten = 10;

          let add = fn(x, y) {
            x + y;
          };

          let result = add(five, ten);

          "kobushka";

          [1, 2];
        "###
        .to_string();

        let expected = vec![
            (token::LET.to_string(), String::from("let")),
            (token::IDENT.to_string(), String::from("five")),
            (token::ASSIGN.to_string(), String::from("=")),
            (token::INT.to_string(), String::from("5")),
            (token::SEMICOLON.to_string(), String::from(";")),
            (token::LET.to_string(), String::from("let")),
            (token::IDENT.to_string(), String::from("ten")),
            (token::ASSIGN.to_string(), String::from("=")),
            (token::INT.to_string(), String::from("10")),
            (token::SEMICOLON.to_string(), String::from(";")),
            (token::LET.to_string(), String::from("let")),
            (token::IDENT.to_string(), String::from("add")),
            (token::ASSIGN.to_string(), String::from("=")),
            (token::FUNCTION.to_string(), String::from("fn")),
            (token::LPAREN.to_string(), String::from("(")),
            (token::IDENT.to_string(), String::from("x")),
            (token::COMMA.to_string(), String::from(",")),
            (token::IDENT.to_string(), String::from("y")),
            (token::RPAREN.to_string(), String::from(")")),
            (token::LBRACE.to_string(), String::from("{")),
            (token::IDENT.to_string(), String::from("x")),
            (token::PLUS.to_string(), String::from("+")),
            (token::IDENT.to_string(), String::from("y")),
            (token::SEMICOLON.to_string(), String::from(";")),
            (token::RBRACE.to_string(), String::from("}")),
            (token::SEMICOLON.to_string(), String::from(";")),
            (token::LET.to_string(), String::from("let")),
            (token::IDENT.to_string(), String::from("result")),
            (token::ASSIGN.to_string(), String::from("=")),
            (token::IDENT.to_string(), String::from("add")),
            (token::LPAREN.to_string(), String::from("(")),
            (token::IDENT.to_string(), String::from("five")),
            (token::COMMA.to_string(), String::from(",")),
            (token::IDENT.to_string(), String::from("ten")),
            (token::RPAREN.to_string(), String::from(")")),
            (token::SEMICOLON.to_string(), String::from(";")),
            (token::STRING.to_string(), String::from("kobushka")),
            (token::SEMICOLON.to_string(), String::from(";")),
            (token::LBRACKET.to_string(), String::from("[")),
            (token::INT.to_string(), String::from("1")),
            (token::COMMA.to_string(), String::from(",")),
            (token::INT.to_string(), String::from("2")),
            (token::RBRACKET.to_string(), String::from("]")),
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

    #[test]
    fn test_arithmetic_operators() {
        let input = r###"
         !-/*5;
         5 < 10 > 5;
       "###
        .to_string();

        let mut lexer = Lexer::new(input);

        let expected = vec![
            (token::BANG.to_string(), String::from("!")),
            (token::MINUS.to_string(), String::from("-")),
            (token::SLASH.to_string(), String::from("/")),
            (token::ASTERISK.to_string(), String::from("*")),
            (token::INT.to_string(), String::from("5")),
            (token::SEMICOLON.to_string(), String::from(";")),
            (token::INT.to_string(), String::from("5")),
            (token::LT.to_string(), String::from("<")),
            (token::INT.to_string(), String::from("10")),
            (token::GT.to_string(), String::from(">")),
            (token::INT.to_string(), String::from("5")),
            (token::SEMICOLON.to_string(), String::from(";")),
        ];

        expected.into_iter().for_each(|(token_type, literal)| {
            let token = lexer.next_token();

            assert_eq!(token.token_type, token_type);
            assert_eq!(token.literal, literal);
        });
    }

    #[test]
    fn test_flow_control_keywords() {
        let input = r###"
          if (2 < 3) {
              return true;
          } else {
              return false;
          }
        "###
        .to_string();

        let expected = vec![
            (token::IF.to_string(), String::from("if")),
            (token::LPAREN.to_string(), String::from("(")),
            (token::INT.to_string(), String::from("2")),
            (token::LT.to_string(), String::from("<")),
            (token::INT.to_string(), String::from("3")),
            (token::RPAREN.to_string(), String::from(")")),
            (token::LBRACE.to_string(), String::from("{")),
            (token::RETURN.to_string(), String::from("return")),
            (token::TRUE.to_string(), String::from("true")),
            (token::SEMICOLON.to_string(), String::from(";")),
            (token::RBRACE.to_string(), String::from("}")),
            (token::ELSE.to_string(), String::from("else")),
            (token::LBRACE.to_string(), String::from("{")),
            (token::RETURN.to_string(), String::from("return")),
            (token::FALSE.to_string(), String::from("false")),
            (token::SEMICOLON.to_string(), String::from(";")),
            (token::RBRACE.to_string(), String::from("}")),
        ];

        let mut lexer = Lexer::new(input);

        expected.into_iter().for_each(|(token_type, literal)| {
            let token = lexer.next_token();

            assert_eq!(token.token_type, token_type);
            assert_eq!(token.literal, literal);
        });
    }

    #[test]
    fn test_multichar_operators() {
        let input = r###"1 == 1 1 != 2"###.to_string();

        let expected = vec![
            (token::INT.to_string(), String::from("1")),
            (token::EQ.to_string(), String::from("==")),
            (token::INT.to_string(), String::from("1")),
            (token::INT.to_string(), String::from("1")),
            (token::NOT_EQ.to_string(), String::from("!=")),
            (token::INT.to_string(), String::from("2")),
        ];

        let mut lexer = Lexer::new(input);

        expected.into_iter().for_each(|(token_type, literal)| {
            let token = lexer.next_token();

            assert_eq!(token.token_type, token_type);
            assert_eq!(token.literal, literal);
        });
    }
}
