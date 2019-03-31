use std::collections::HashMap;

use crate::evaluation::object;

#[derive(Debug, Clone)]
pub struct Environment {
    store: HashMap<String, object::Object>,
    outer: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment { store: HashMap::new(), outer: None }
    }

    // New enclosed environment accepts a copy of outer environment.
    // It should be reflected in specification, that after function
    // is described you cannot modify outer scope inside it.
    // Because you access your own copy of the outer scope.
    pub fn new_enclosed_environment(outer: Self) -> Self {
        Environment { store: HashMap::new(), outer: Some(Box::new(outer)) }
    }

    pub fn get(&self, name: String) -> Option<&object::Object> {
        match self.store.get(&name) {
            Some(identifier) => Some(identifier),
            None => match &self.outer {
                Some(outer_scope) => outer_scope.get(name),
                None => None,
            }
        }
    }

    pub fn set(&mut self, name: String, object: object::Object) -> &object::Object {
        self.store.insert(name.clone(), object);
        self.get(name).unwrap() // we can unwrap here,
                                // because we inserted element on previous line
                                // TODO: might be a good idea to check this one more time
    }
}
