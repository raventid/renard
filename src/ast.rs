use std::fmt;
use crate::token::Statements;

pub trait Node {
    fn token_literal(&self) -> String;
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statements>,
}

impl Node for Program {
    fn token_literal(&self) -> String {
        match self.statements.first() {
            Some(statement) => statement.token_literal(),
            None => "".to_string()
        }
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for statement in self.statements.iter() {
            fmt::Display::fmt(&statement, f)?;
            writeln!(f, "{}", "")?
        }

        Ok(())
    }
}
