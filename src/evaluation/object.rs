use crate::evaluation::environment;
use crate::token;

type ObjectType = String;

pub trait ObjectT {
    fn object_type(&self) -> ObjectType;
    fn inspect(&self) -> String;
}

// **********************************************
// * Interpreted object represanted as sum type *
// **********************************************
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Object {
    Integer(Integer),
    Boolean(Boolean),
    Nil(Nil),
    ReturnValue(Box<ReturnValue>),
    Error(Error),
}

impl ObjectT for Object {
    fn object_type(&self) -> ObjectType {
        match self {
            Object::Integer(i) => i.object_type(),
            Object::Boolean(b) => b.object_type(),
            Object::Nil(n) => n.object_type(),
            Object::ReturnValue(rv) => rv.object_type(),
            Object::Error(err) => err.object_type(),
        }
    }

    fn inspect(&self) -> String {
        match self {
            Object::Integer(i) => i.inspect(),
            Object::Boolean(b) => b.inspect(),
            Object::Nil(n) => n.inspect(),
            Object::ReturnValue(rv) => rv.inspect(),
            Object::Error(err) => err.inspect(),
        }
    }
}

// ************************************************
// * Internal represantion of interpreted objects.*
// ************************************************

// Integer value
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Integer {
    pub value: i32,
}

impl ObjectT for Integer {
    fn object_type(&self) -> ObjectType {
        "INTEGER".to_string()
    }

    fn inspect(&self) -> String {
        self.value.to_string()
    }
}

// Boolean value
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Boolean {
    pub value: bool,
}

impl ObjectT for Boolean {
    fn object_type(&self) -> ObjectType {
        "BOOLEAN".to_string()
    }

    fn inspect(&self) -> String {
        self.value.to_string()
    }
}

// Nil value (billion dollar mistake should be in every language, lol)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Nil {}

impl ObjectT for Nil {
    fn object_type(&self) -> ObjectType {
        "NULL".to_string()
    }

    fn inspect(&self) -> String {
        "null".to_string()
    }
}

// ReturnValue
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReturnValue {
    pub value: Object,
}

impl ObjectT for ReturnValue {
    fn object_type(&self) -> ObjectType {
        "RETURN_VALUE".to_string()
    }

    fn inspect(&self) -> String {
        self.value.inspect()
    }
}

// Error value
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Error {
    pub message: String,
}

impl ObjectT for Error {
    fn object_type(&self) -> ObjectType {
        "ERROR".to_string()
    }

    fn inspect(&self) -> String {
        self.message.to_string()
    }
}

// Function object
#[derive(Debug, Clone)]
pub struct Function {
    pub parameters: Option<Vec<token::Identifier>>,
    pub body: token::BlockStatement,
    pub env: environment::Environment,
}

impl ObjectT for Function {
    fn object_type(&self) -> ObjectType {
        "FUNCTION".to_string()
    }

    fn inspect(&self) -> String {
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
