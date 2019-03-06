use crate::token::Statements;

pub trait Node {
    fn token_literal(&self) -> String {
        "To implement".to_string()
    }
}

pub trait Statement : Node {
    fn statement_node(&self) -> String {
        "To implement".to_string()
    }
}

pub trait Expression : Node {
    fn expression_node(&self) -> String {
        "To implement".to_string()
    }
}

pub struct Program {
    pub statements: Vec<Statements>,
}

impl Node for Program {
    fn token_literal(&self) -> String {
        if !self.statements.is_empty() {
            self.statements[0].token_literal()
        } else {
            "".to_string()
        }
    }
}
