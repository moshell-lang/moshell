use crate::relations::{GlobalObjectId, ObjectId, Relations, SourceObjectId, Symbol};
use indexmap::IndexMap;
use std::ops::Neg;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TypeInfo {
    Variable,
    Function,
}

/// A collection of variables
#[derive(Debug, Clone, Default)]
pub struct Variables {
    locals: Locals,

    globals: IndexMap<String, GlobalObjectId>,
}

impl Variables {
    /// Creates a new local variable.
    pub fn declare_local(&mut self, name: String, ty: TypeInfo) -> Symbol {
        self.locals.declare(name, ty)
    }

    /// Identifies a named variable to a binding.
    ///
    /// This creates a new global variable if the variable is not already known or is not reachable,
    /// or returns the existing variable identifier. To only lookup a variable, use [`Variables::get_reachable`].
    pub fn identify(
        &mut self,
        state: SourceObjectId,
        relations: &mut Relations,
        name: &str,
    ) -> Symbol {
        match self
            .locals
            .position_reachable_local(name)
            .map(|idx| self.locals.vars.len() - 1 - idx)
        {
            Some(var) => Symbol::Local(var),
            None => {
                let id = *self
                    .globals
                    .entry(name.to_string())
                    .or_insert_with(|| relations.track_new_object(state));
                id.into()
            }
        }
    }

    /// Gets the local symbol associated with an already known name.
    ///
    /// The lookup uses the current scope, which is frequently updated during the collection phase.
    /// That's the main reason why this method should be used in pair the variable capture
    /// resolution, immediately after the closure is observed and inertly populated.
    pub fn get_reachable(&self, name: &str) -> Option<Symbol> {
        self.locals
            .position_reachable_local(name)
            .map(|idx| Symbol::Local(self.locals.vars.len() - 1 - idx))
    }

    /// Gets the local exported symbol associated with an already known name.
    ///
    /// Exported symbols are always declared in the outermost scope, and should be checked only
    /// after the whole environment is collected.
    pub fn get_exported(&self, name: &str) -> Option<Symbol> {
        self.locals
            .vars
            .iter()
            .rev()
            .position(|var| var.name == name && var.depth == -1)
            .map(|idx| Symbol::Local(self.locals.vars.len() - 1 - idx))
    }

    /// Lists all local variables, in the order they are declared.
    ///
    /// This exposes their current state, which is only interesting for debugging.
    /// Use [`Variables::get_reachable`] to lookup any variable during the collection phase,
    /// or [`Variables::get_exported`] to lookup an exported variable after the collection phase.
    pub fn all_vars(&self) -> &[Variable] {
        &self.locals.vars
    }

    /// Iterates over all the exported variables, local to the environment.
    pub fn exported_vars(&self) -> impl Iterator<Item = &Variable> {
        //consider for now that all local vars of the outermost scope are exported
        self.locals.vars.iter().filter(|var| var.depth == -1)
    }

    /// Iterates over all the global variable ids, with their corresponding name.
    pub fn external_vars(&self) -> impl Iterator<Item = (&String, GlobalObjectId)> {
        self.globals.iter().map(|(name, id)| (name, *id))
    }

    /// Gets the name of a global variable.
    ///
    /// This returns the name only if the global object comes from this environment.
    pub fn get_symbol_name(&self, object_id: GlobalObjectId) -> Option<&str> {
        self.globals.iter().find_map(|(name, &id)| {
            if id == object_id {
                Some(name.as_ref())
            } else {
                None
            }
        })
    }

    pub fn begin_scope(&mut self) {
        self.locals.begin_scope();
    }

    pub fn end_scope(&mut self) {
        self.locals.end_scope();
    }
}

#[derive(Debug, Clone, Default)]
struct Locals {
    /// The actual list of seen and unique variables.
    vars: Vec<Variable>,

    /// The current depth of the scope.
    ///
    /// The first scope is 0.
    current_depth: usize,
}

impl Locals {
    /// Adds a new variable and binds it to the current scope.
    fn declare(&mut self, name: String, ty: TypeInfo) -> Symbol {
        let id = self.vars.len();
        self.vars.push(Variable {
            name,
            depth: self.current_depth as isize,
            ty,
        });
        Symbol::Local(id)
    }

    /// Declares a new variable of type `TypeInfo::Variable`.
    fn declare_variable(&mut self, name: String) {
        self.declare(name, TypeInfo::Variable);
    }

    /// Moves into a new scope.
    ///
    /// # Panics
    /// This method panics if the maximum number of scopes has been reached.
    fn begin_scope(&mut self) {
        self.current_depth = (self.current_depth as isize)
            .checked_add(1)
            .expect("Too many scopes") as usize;
    }

    /// Moves out of the current scope.
    ///
    /// This method marks all the variables that are not reachable anymore.
    ///
    /// # Panics
    /// This method panics if the current scope is already the root scope.
    fn end_scope(&mut self) {
        self.vars
            .iter_mut()
            .rev()
            .take_while(|var| var.depth == self.current_depth as isize)
            .for_each(|var| {
                var.depth = var.depth.neg();
            });

        self.current_depth = self
            .current_depth
            .checked_sub(1)
            .expect("Cannot end the root scope");
    }

    /// Looks up a variable by name that is reachable from the current scope.
    fn lookup_reachable_local(&self, name: &str) -> Option<&Variable> {
        self.vars
            .iter()
            .rev()
            .find(|var| var.name == name && var.depth >= 0)
    }

    /// Gets the offset of a variable from the current scope.
    ///
    /// This relative index is from the end of the Vec of variables, so it
    /// becomes invalid when a new variable is declared. Prefers the other
    /// methods that exposes an index from the beginning of the Vec.
    fn position_reachable_local(&self, name: &str) -> Option<ObjectId> {
        self.vars
            .iter()
            .rev()
            .position(|var| var.name == name && var.depth >= 0)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    /// The name identifier of the variable.
    pub name: String,

    pub ty: TypeInfo,

    /// The depth of the variable.
    ///
    /// This is used to keep track if the variable is still reachable during the first
    /// pass of the analyzer. The value is positive if the variable scope has not ended
    /// yet. If it is out of scope, the value is negative, with the absolute value being
    /// the depth of the scope where the variable was declared.
    depth: isize,
}

impl Variable {
    /// Creates a new variable.
    ///
    /// This convenience method accepts negative values as depths, which are the internal
    /// representations of unreachable variables.
    pub fn scoped(name: String, depth: isize) -> Self {
        Self {
            name,
            depth,
            ty: TypeInfo::Variable,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn access_by_name() {
        let mut locals = Locals::default();
        locals.declare_variable("foo".to_owned());
        locals.begin_scope();
        locals.declare_variable("bar".to_owned());
        assert_eq!(
            locals.lookup_reachable_local("foo"),
            Some(&Variable::scoped("foo".to_owned(), 0))
        );

        assert_eq!(
            locals.lookup_reachable_local("bar"),
            Some(&Variable::scoped("bar".to_owned(), 1))
        );
    }

    #[test]
    fn access_out_of_scope() {
        let mut locals = Locals::default();
        locals.begin_scope();
        locals.declare_variable("bar".to_owned());
        locals.end_scope();
        assert_eq!(locals.lookup_reachable_local("bar"), None);
        locals.begin_scope();
        assert_eq!(locals.lookup_reachable_local("bar"), None);
    }

    #[test]
    fn shadow_nested() {
        let mut locals = Locals::default();
        locals.declare_variable("foo".to_owned());
        locals.begin_scope();
        locals.begin_scope();
        locals.declare_variable("foo".to_owned());
        assert_eq!(
            locals.lookup_reachable_local("foo"),
            Some(&Variable::scoped("foo".to_owned(), 2))
        );
        locals.end_scope();
        assert_eq!(
            locals.lookup_reachable_local("foo"),
            Some(&Variable::scoped("foo".to_owned(), 0))
        );
        locals.end_scope();
        assert_eq!(
            locals.lookup_reachable_local("foo"),
            Some(&Variable::scoped("foo".to_owned(), 0))
        );
    }
}
