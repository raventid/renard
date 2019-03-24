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
}

impl ObjectT for Object {
    fn object_type(&self) -> ObjectType {
        match self {
            Object::Integer(i) => i.object_type(),
            Object::Boolean(b) => b.object_type(),
            Object::Nil(n) => n.object_type(),
        }
    }

    fn inspect(&self) -> String {
        match self {
            Object::Integer(i) => i.inspect(),
            Object::Boolean(b) => b.inspect(),
            Object::Nil(n) => n.inspect(),
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
