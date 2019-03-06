use crate::ast;
use std::collections::HashMap;

type TokenType = String;

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

pub fn lookup_ident(ident: String) -> TokenType {
    let keywords: HashMap<String, TokenType> = [
        ("fn".to_string(), FUNCTION.to_string()),
        ("let".to_string(), LET.to_string()),
        ("true".to_string(), TRUE.to_string()),
        ("false".to_string(), FALSE.to_string()),
        ("if".to_string(), IF.to_string()),
        ("else".to_string(), ELSE.to_string()),
        ("return".to_string(), RETURN.to_string()),
    ]
    .iter()
    .cloned()
    .collect();

    match keywords.get(&ident) {
        Some(keyword) => keyword.clone(),
        None => IDENT.to_string(),
    }
}

pub const ILLEGAL: &str = "ILLEGAL";
pub const EOF: &str = "EOF";

// Identifiers and literals
pub const IDENT: &str = "IDENT";
pub const INT: &str = "INT";

// Operators
pub const ASSIGN: &str = "=";
pub const PLUS: &str = "+";
pub const MINUS: &str = "-";
pub const BANG: &str = "!";
pub const ASTERISK: &str = "*";
pub const SLASH: &str = "/";

pub const LT: &str = "<";
pub const GT: &str = ">";

pub const EQ: &str = "==";
pub const NOT_EQ: &str = "!=";

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
pub const TRUE: &str = "TRUE";
pub const FALSE: &str = "FALSE";
pub const IF: &str = "IF";
pub const ELSE: &str = "ELSE";
pub const RETURN: &str = "RETURN";

// <<--**********************-->>
// Statements used by AST
// <<--**********************-->>

// TODO: Move this block to ast module. It's a bad place for it
// to be here.
pub enum Statements {
    LetStatement(LetStatement),
}

impl ast::Node for Statements {
    fn token_literal(&self) -> String {
        match self {
            Statements::LetStatement(ls) => ls.token_literal(),
            _ => panic!("Node for some expression is not implemented yet"),
        }
    }
}

pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: String, // for now it's good enough :)
    // pub value: Box<ast::Expression>, // interface
}

impl ast::Statement for LetStatement {
    fn statement_node(&self) -> String {
        "LetStatement".to_string()
    }
}

impl ast::Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }
}

pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl ast::Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }
}

// Use expression as a marker trait? And so I should use Statement as a marker trait?
impl ast::Expression for Identifier {
    fn expression_node(&self) -> String {
        "Identifier".to_string()
    }
}
