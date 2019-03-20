type ObjectType = String;

trait ObjectT {
    fn object_type() -> ObjectType;
    fn inspect(&self) -> String;
}

// **********************************************
// * Interpreted object represanted as sum type *
// **********************************************
#[derive(Debug, Clone)]
pub enum Object {
    Integer(Integer),
    Boolean(Boolean),
    Nil(Nil),
}

// ************************************************
// * Internal represantion of interpreted objects.*
// ************************************************

// Integer value
#[derive(Debug, Clone)]
pub struct Integer {
    pub value: i32,
}

impl ObjectT for Integer {
    fn object_type() -> ObjectType {
        "INTEGER".to_string()
    }

    fn inspect(&self) -> String {
        self.value.to_string()
    }
}

// Boolean value
#[derive(Debug, Clone)]
pub struct Boolean {
    pub value: bool,
}

impl ObjectT for Boolean {
    fn object_type() -> ObjectType {
        "BOOLEAN".to_string()
    }

    fn inspect(&self) -> String {
        self.value.to_string()
    }
}

// Nil value (billion dollar mistake should be in every language, lol)
#[derive(Debug, Clone)]
pub struct Nil {}

impl ObjectT for Nil {
    fn object_type() -> ObjectType {
        "NULL".to_string()
    }

    fn inspect(&self) -> String {
        "null".to_string()
    }
}
