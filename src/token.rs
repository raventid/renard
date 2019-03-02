type TokenType = String;

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

pub const ILLEGAL: &str = "ILLEGAL";
pub const EOF: &str = "EOF";

// Identifiers and literals
pub const IDENT: &str = "IDENT";
pub const INT: &str = "INT";

// Operators
pub const ASSIGN: &str = "=";
pub const PLUS: &str = "+";

// Delimiters
pub const COMMA: &str = ",";
pub const SEMICOLON: &str = ";";

pub const LPAREN: &str = "(";
pub const RPAREN: &str = ")";
pub const LBRACE: &str = "{";
pub const RBRACE: &str = "}";

// Keywords
pub const FUNCTION: &str = "FUNCTION";
pub const LET: &str = "LET";
