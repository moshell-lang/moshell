use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};

use crate::name::Name;
use crate::relations::{GlobalObjectId, ObjectId, Symbol};

/// Information over the declared type of a variable
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TypeInfo {
    /// The variable is a regular variable
    Variable,
    /// The variable is a function declaration
    Function,
}

impl Display for TypeInfo {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeInfo::Variable => write!(f, "variable"),
            TypeInfo::Function => write!(f, "function"),
        }
    }
}

/// A collection of variables
#[derive(Debug, Clone, Default)]
pub struct Variables {
    /// Locals declarations
    locals: Locals,

    /// Relations with external variables.
    /// The key is the variable Names, where value is the relation with another external environment's [Locals]
    externals: HashMap<Name, GlobalObjectId>,
}

impl Variables {
    /// Creates a new local variable.
    pub fn declare_local(&mut self, name: String, ty: TypeInfo) -> Symbol {
        self.locals.declare(name, ty)
    }

    /// Returns the local variable associated with the id
    pub fn get_var(&self, id: ObjectId) -> Option<&Variable> {
        self.locals.vars.get(id)
    }

    /// Returns an entry for the given external symbol name relation
    pub fn external(&mut self, name: Name) -> Entry<Name, GlobalObjectId> {
        self.externals.entry(name)
    }

    /// Gets the local identifier associated with an already known name.
    ///
    /// The lookup uses the current scope, which is frequently updated during the collection phase.
    /// That's the main reason why this method should be used in pair the variable capture
    /// resolution, immediately after the closure is observed and inertly populated.
    pub fn get_reachable(&self, name: &str) -> Option<ObjectId> {
        self.locals.position_reachable_local(name)
    }

    /// Gets the local exported symbol associated with an already known name.
    ///
    /// Exported symbols are always declared in the outermost scope, and should be checked only
    /// after the whole environment is collected.
    pub fn get_exported(&self, name: &str) -> Option<ObjectId> {
        self.locals
            .vars
            .iter()
            .rev()
            .position(|var| var.name == name && var.is_exported())
            .map(|idx| self.locals.vars.len() - 1 - idx)
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
    pub fn external_vars(&self) -> impl Iterator<Item = (&Name, GlobalObjectId)> {
        self.externals.iter().map(|(name, id)| (name, *id))
    }

    /// Gets the name of an external variable.
    ///
    /// This returns the name only if the global object comes from this environment.
    pub fn get_external_symbol_name(&self, object_id: GlobalObjectId) -> Option<&Name> {
        self.externals
            .iter()
            .find_map(|(name, id)| (id == &object_id).then_some(name))
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
                var.depth = -var.depth;
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

    /// Gets the variable id from the current scope.
    fn position_reachable_local(&self, name: &str) -> Option<ObjectId> {
        self.vars
            .iter()
            .rev()
            .position(|var| var.name == name && var.depth >= 0)
            .map(|idx| self.vars.len() - 1 - idx)
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

    /// Returns `true` if the variable can be accessed externally, without being
    /// captured.
    pub const fn is_exported(&self) -> bool {
        self.depth == -1
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
