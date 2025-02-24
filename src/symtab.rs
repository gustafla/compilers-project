use std::collections::hash_map::{Entry, HashMap, OccupiedEntry};
use thiserror::Error;

#[derive(Debug, Error)]
#[error("Unresolved identifier {0:?}")]
pub struct Error(String);

pub struct SymbolTable<'a, T: Clone>(Vec<HashMap<&'a str, T>>);

impl<'a, T: Clone> SymbolTable<'a, T> {
    pub fn new(root: &[(&'a str, T)]) -> Self {
        Self(vec![root.iter().cloned().collect()])
    }

    pub fn resolve(&mut self, key: &'a str) -> Result<OccupiedEntry<'_, &'a str, T>, Error> {
        for table in self.0.iter_mut().rev() {
            if let Entry::Occupied(entry) = table.entry(key) {
                return Ok(entry);
            }
        }
        Err(Error(String::from(key)))
    }

    pub fn push(&mut self) {
        self.0.push(HashMap::new());
    }

    pub fn pop(&mut self) {
        self.0.pop();
    }

    pub fn insert(&mut self, key: &'a str, value: T) -> Option<T> {
        self.0.last_mut().unwrap().insert(key, value)
    }
}
