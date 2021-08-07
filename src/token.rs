use crate::ast;
use std::collections::HashMap;
use std::fmt;

pub type TokenType = String;

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
pub const STRING: &str = "STRING";

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
pub const LBRACKET: &str = "[";
pub const RBRACKET: &str = "]";

// Keywords
pub const FUNCTION: &str = "FUNCTION";
pub const LET: &str = "LET";
pub const TRUE: &str = "TRUE";
pub const FALSE: &str = "FALSE";
pub const IF: &str = "IF";
pub const ELSE: &str = "ELSE";
pub const RETURN: &str = "RETURN";

// Precedences
pub const LOWEST: u8 = 1;
pub const EQUALS: u8 = 2; // ==
pub const LESSGREATER: u8 = 3; // > or <
pub const SUM: u8 = 4; // +
pub const PRODUCT: u8 = 5; // *
pub const PREFIX: u8 = 6; // -B or !B
pub const CALL: u8 = 7; // do_something()
pub const INDEX: u8 = 8; // array index `[]`

// <<--**********************-->>
// Statements used by AST
// <<--**********************-->>

// TODO: Move this block to ast module. It's a bad place for it
// to be here.

// <<--**********************-->>
// STATEMENTS
// <<--**********************-->>

#[derive(Debug, Clone)]
pub enum Statements {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
}

impl ast::Node for Statements {
    fn token_literal(&self) -> String {
        match self {
            Statements::LetStatement(ls) => ls.token_literal(),
            Statements::ReturnStatement(rs) => rs.token_literal(),
            Statements::ExpressionStatement(es) => es.token_literal(),
        }
    }
}

impl fmt::Display for Statements {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statements::LetStatement(ls) => fmt::Display::fmt(ls, f),
            Statements::ReturnStatement(rs) => fmt::Display::fmt(rs, f),
            Statements::ExpressionStatement(es) => fmt::Display::fmt(es, f),
        }
    }
}

// <<--**********************-->>
// EXPRESSIONS
// <<--**********************-->>

#[derive(Debug, Clone)]
pub enum Expression {
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    StringLiteral(StringLiteral),
    ArrayLiteral(ArrayLiteral),
    PrefixExpression(Box<PrefixExpression>), // This expression contains recursion
    InfixExpression(Box<InfixExpression>),   // Same as previous
    Boolean(Boolean),
    IfExpression(Box<IfExpression>), // recur
    FunctionLiteral(FunctionLiteral),
    CallExpression(Box<CallExpression>), // recur
    IndexExpression(Box<IndexExpression>),
}

impl ast::Node for Expression {
    fn token_literal(&self) -> String {
        match self {
            Expression::Identifier(i) => i.token_literal(),
            Expression::IntegerLiteral(il) => il.token_literal(),
            Expression::PrefixExpression(pe) => pe.token_literal(),
            Expression::InfixExpression(ie) => ie.token_literal(),
            Expression::StringLiteral(sl) => sl.token_literal(),
            Expression::ArrayLiteral(al) => al.token_literal(),
            Expression::Boolean(b) => b.token_literal(),
            Expression::IfExpression(ie) => ie.token_literal(),
            Expression::FunctionLiteral(f) => f.token_literal(),
            Expression::CallExpression(ce) => ce.token_literal(),
            Expression::IndexExpression(ie) => ie.token_literal(),
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Identifier(i) => fmt::Display::fmt(i, f),
            Expression::IntegerLiteral(il) => fmt::Display::fmt(il, f),
            Expression::StringLiteral(sl) => fmt::Display::fmt(sl, f),
            Expression::ArrayLiteral(al) => fmt::Display::fmt(al, f),
            Expression::PrefixExpression(pe) => fmt::Display::fmt(pe, f),
            Expression::InfixExpression(ie) => fmt::Display::fmt(ie, f),
            Expression::Boolean(b) => fmt::Display::fmt(b, f),
            Expression::IfExpression(ie) => fmt::Display::fmt(ie, f),
            Expression::FunctionLiteral(func) => fmt::Display::fmt(func, f),
            Expression::CallExpression(ce) => fmt::Display::fmt(ce, f),
            Expression::IndexExpression(ie) => fmt::Display::fmt(ie, f),
        }
    }
}

// Let statement.
// The way to introduce binding in Renard.
#[derive(Debug, Clone)]
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Expression,
}

impl ast::Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }
}

impl fmt::Display for LetStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use crate::ast::Node;
        write!(
            f,
            "{} {} = {};",
            self.token_literal(),
            self.name.value,
            self.value
        )
    }
}

// Include Identifier in Expression group?
#[derive(Debug, Clone)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl ast::Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

// String literal.
#[derive(Debug, Clone)]
pub struct StringLiteral {
    pub token: Token,
    pub value: String,
}

impl ast::Node for StringLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }
}

impl fmt::Display for StringLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

// Return statement grammar.
//
// Structure: `return <expression>`;
#[derive(Debug, Clone)]
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Expression,
}

impl ast::Node for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }
}

impl fmt::Display for ReturnStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use crate::ast::Node;
        write!(f, "{} {};", self.token_literal(), self.return_value)
    }
}

// Expression statement.
//
// The reason this statement exists is that in our
// language we might have a statement with next structure:
// `10 + 5;`
// as you can see this just expression without any `let` binding.
// To unify our grammar we'll use expression statement entity,
// which represent this situation.
//
// Structure: `10 + 5`
#[derive(Debug, Clone)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Expression, // move to expression later
}

impl ast::Node for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }
}

impl fmt::Display for ExpressionStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.expression, f)
    }
}

// Integer literal.
//
// Represent parsed integer number.
//
// Structure: `5`
#[derive(Debug, Clone)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i32,
}

impl ast::Node for IntegerLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }
}

impl fmt::Display for IntegerLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.token.literal)
    }
}

// Prefix expression.
//
// Represent any prefix expression like `!` or `-`.
//
// Structure: `!5`
#[derive(Debug, Clone)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Expression,
}

impl ast::Node for PrefixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }
}

impl fmt::Display for PrefixExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({} {})", self.operator, self.right)
    }
}

// Infix expression
//
// Represents any infix expression like `"Julian" + "Pokrovsky"`
//
// Structure: `<expression> <infix operator> <expression>`
#[derive(Debug, Clone)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Expression,
    pub operator: String,
    pub right: Expression,
}

impl ast::Node for InfixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }
}

impl fmt::Display for InfixExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
    }
}

// Boolean literals
//
// Example: `true;`
//
// Structure: `<true|false>`
#[derive(Debug, Clone)]
pub struct Boolean {
    pub token: Token,
    pub value: bool,
}

impl ast::Node for Boolean {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }
}

impl fmt::Display for Boolean {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.token.literal)
    }
}

// If expression
//
// Example: `if (user_is_your_friend) { say_czesc() } else { say_dzien_dobry() };`
//
// Structure: if (<condition>) <consequence> else <alternative>
#[derive(Debug, Clone)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Expression,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl ast::Node for IfExpression {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }
}

impl fmt::Display for IfExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.alternative.clone() {
            Some(alternative) => write!(
                f,
                "if {} {} else {}",
                self.condition, self.consequence, alternative
            ),
            None => write!(f, "if {} {}", self.condition, self.consequence),
        }
    }
}

// Internal statement representation designed to work with IfExpression
// It is now also used for FunctionLiteral besides IfExpression.
#[derive(Debug, Clone)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statements>,
}

impl ast::Node for BlockStatement {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }
}

impl fmt::Display for BlockStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for statement in self.statements.clone() {
            write!(f, "{}", statement)?
        }

        Ok(())
    }
}

// Function literals.
//
// Example: fn (x, y) { return x + y; }
//
// Structure: fn <parameters> <block statement>
#[derive(Debug, Clone)]
pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Option<Vec<Identifier>>,
    pub body: BlockStatement,
}

impl ast::Node for FunctionLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }
}

impl fmt::Display for FunctionLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ast::Node;
        let params = match self.parameters.clone() {
            Some(params) => params
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(","),
            None => "".to_string(),
        };

        write!(f, "{}({}){{{}}}", self.token_literal(), params, self.body)
    }
}

// Call expression.
//
// Human readable description:
//
// To elaborate a bit on `Example1` I'd like to note that
// we can call function inplace, this is exactly what you
// can see in the this example.
//
// Second example illustrates a simple "named function" call.
//
// Example1: fn(a,b) {a+b;}(1,2)
// Example2: hello("World")
//
// Structure: <expression>(<comma separated expressions>)
#[derive(Debug, Clone)]
pub struct CallExpression {
    pub token: Token,
    pub function: Expression,
    pub arguments: Option<Vec<Expression>>,
}

impl ast::Node for CallExpression {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }
}

impl fmt::Display for CallExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let args = match self.arguments.clone() {
            Some(args) => args
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(", "),
            None => "".to_string(),
        };

        write!(f, "{}({})", self.function, args)
    }
}

// Array literal.
//
// Example: [1, 2]
//
// Structure: [<expression>, <expression>, ...]
#[derive(Debug, Clone)]
pub struct ArrayLiteral {
    pub token: Token,
    pub elements: Vec<Expression>,
}

impl ast::Node for ArrayLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }
}

impl fmt::Display for ArrayLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let elems = self
            .elements
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join(", ");

        write!(f, "[{}]", elems)
    }
}

// Index expression.
//
// Example: [1,2,3,4][1];
//
// Structure: <expression>[<expression>]
#[derive(Debug, Clone)]
pub struct IndexExpression {
    pub token: Token,
    pub left: Expression,
    pub index: Expression,
}

impl ast::Node for IndexExpression {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }
}

impl fmt::Display for IndexExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}[{}])", self.left, self.index)
    }
}
