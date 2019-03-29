use std::collections::HashMap;

use crate::evaluation::object;

#[derive(Debug, Clone)]
pub struct Environment {
    store: HashMap<String, object::Object>,
}

impl Environment {
    pub fn new() -> Self {
        Environment { store: HashMap::new() }
    }

    pub fn get(&self, name: String) -> Option<&object::Object> {
        self.store.get(&name)
    }

    pub fn set(&mut self, name: String, object: object::Object) -> &object::Object {
        self.store.insert(name.clone(), object);
        self.get(name).unwrap() // we can unwrap here,
                                // because we inserted element on previous line
                                // TODO: might be a good idea to check this one more time
    }
}
