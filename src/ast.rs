use std::fmt;
use crate::token::Statements;

pub trait Node {
    fn token_literal(&self) -> String {
        "To implement".to_string()
    }
}

#[derive(Debug)]
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

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for statement in self.statements.iter() {
            fmt::Display::fmt(&statement, f)?;
            writeln!(f, "{}", "");
        }

        Ok(())
    }
}
