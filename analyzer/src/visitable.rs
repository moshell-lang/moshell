use crate::name::Name;
use indexmap::IndexSet;
use std::collections::HashSet;

#[derive(Eq, PartialEq, Debug)]
pub struct ModulesVisitable {
    to_visit: IndexSet<Name>,
    visited: HashSet<Name>,
}

impl ModulesVisitable {
    pub fn with_entry(entry_point: Name) -> Self {
        Self {
            to_visit: IndexSet::from([entry_point]),
            visited: HashSet::new(),
        }
    }

    #[cfg(test)]
    pub(crate) fn empty() -> Self {
        Self {
            to_visit: IndexSet::new(),
            visited: HashSet::new(),
        }
    }

    #[cfg(test)]
    pub(crate) fn new<const A: usize, const B: usize>(
        to_visit: [Name; A],
        visited: [Name; B],
    ) -> Self {
        Self {
            to_visit: IndexSet::from(to_visit),
            visited: HashSet::from(visited),
        }
    }

    /// Inserts a name in this visitable,
    /// Returns true if the name was effectively inserted, false if it was already visited.
    pub fn push(&mut self, name: Name) -> bool {
        if self.is_already_visited(&name) {
            return false;
        }
        self.to_visit.insert(name);
        true
    }

    pub fn pop(&mut self) -> Option<Name> {
        if let Some(name) = self.to_visit.pop() {
            self.visited.insert(name.clone());
            return Some(name);
        }
        None
    }

    pub fn is_empty(&self) -> bool {
        self.to_visit.is_empty()
    }

    pub fn is_already_visited(&self, name: &Name) -> bool {
        self.visited.contains(name)
    }
}
