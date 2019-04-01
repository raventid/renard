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
    Stringl(Stringl),
    Boolean(Boolean),
    Nil(Nil),
    ReturnValue(Box<ReturnValue>),
    Error(Error),
    Function(Function),
}

impl Object {
    pub fn same_tag(&self, other: &Object) -> bool {
        match (self, other) {
            (Object::Integer(_), Object::Integer(_)) => true,
            (Object::Stringl(_), Object::Stringl(_)) => true,
            (Object::Boolean(_), Object::Boolean(_)) => true,
            (Object::Nil(_), Object::Nil(_)) => true,
            (Object::ReturnValue(_), Object::ReturnValue(_)) => true,
            (Object::Error(_), Object::Error(_)) => true,
            (Object::Function(_), Object::Function(_)) => true,
            (_, _) => false,
        }
    }
}

impl ObjectT for Object {
    fn object_type(&self) -> ObjectType {
        match self {
            Object::Integer(i) => i.object_type(),
            Object::Stringl(s) => s.object_type(),
            Object::Boolean(b) => b.object_type(),
            Object::Nil(n) => n.object_type(),
            Object::ReturnValue(rv) => rv.object_type(),
            Object::Error(err) => err.object_type(),
            Object::Function(fun) => fun.object_type(),
        }
    }

    fn inspect(&self) -> String {
        match self {
            Object::Integer(i) => i.inspect(),
            Object::Stringl(s) => s.inspect(),
            Object::Boolean(b) => b.inspect(),
            Object::Nil(n) => n.inspect(),
            Object::ReturnValue(rv) => rv.inspect(),
            Object::Error(err) => err.inspect(),
            Object::Function(fun) => fun.inspect(),
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

// String literal
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Stringl {
    pub value: String,
}

impl ObjectT for Stringl {
    fn object_type(&self) -> ObjectType {
        "STRING".to_string()
    }

    fn inspect(&self) -> String {
        self.value.clone()
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

// Functions are never equal.
// It's like NaN.
impl PartialEq for Function {
    fn eq(&self, other: &Function) -> bool {
        false
    }
}
impl Eq for Function {}

impl ObjectT for Function {
    fn object_type(&self) -> ObjectType {
        "FUNCTION".to_string()
    }

    fn inspect(&self) -> String {
        let params = match self.parameters.clone() {
            Some(params) => params
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(","),
            None => "".to_string(),
        };

        format!("fn({}) {{{}}}", params, self.body)
    }
}
