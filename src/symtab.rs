use std::collections::hash_map::{Entry, HashMap, OccupiedEntry};
use thiserror::Error;

#[derive(Debug, Error)]
#[error("Unresolved identifier {0:?}")]
pub struct Error(pub String);

#[doc(alias = "SymTab")]
#[derive(Clone)]
pub struct SymbolTable<'a, T>(Vec<HashMap<&'a str, T>>);

impl<'a, T, I> From<I> for SymbolTable<'a, T>
where
    I: IntoIterator<Item = (&'a str, T)>,
{
    fn from(value: I) -> Self {
        Self(vec![value.into_iter().collect()])
    }
}

impl<'a, T> SymbolTable<'a, T> {
    pub fn new() -> Self {
        Self(vec![HashMap::new()])
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
        if self.0.is_empty() {
            panic!("Symbol table root was popped");
        }
    }

    pub fn depth(&self) -> usize {
        self.0.len()
    }

    pub fn insert(&mut self, key: &'a str, value: T) -> Option<T> {
        self.0.last_mut().unwrap().insert(key, value)
    }
}

impl<T> Default for SymbolTable<'_, T> {
    fn default() -> Self {
        Self::new()
    }
}
