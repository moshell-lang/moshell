use crate::name::Name;
use crate::relations::{GlobalObjectId, ObjectId, Relations, SourceObjectId, Symbol};
use indexmap::IndexMap;
use std::num::NonZeroUsize;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TypeInfo {
    Variable,
    Function,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeUsage {
    Variable(Name),
    Function(Name),
}

impl TypeUsage {
    pub fn name(&self) -> &Name {
        match self {
            TypeUsage::Variable(name) => name,
            TypeUsage::Function(name) => name,
        }
    }
}

/// A collection of variables
#[derive(Debug, Clone, Default)]
pub struct Variables {
    locals: Locals,

    external_usages: IndexMap<TypeUsage, GlobalObjectId>,
}

impl Variables {
    /// Creates a new local variable.
    pub fn declare_local(&mut self, name: String, ty: TypeInfo) -> Symbol {
        self.locals.declare(name, ty)
    }

    /// Identifies a named variable to a binding.
    ///
    /// This creates a new external variable if the variable is not already known or is not reachable,
    /// or returns the existing variable identifier. To only lookup a variable, use [`Variables::get_symbol`].
    pub fn identify(
        &mut self,
        state: SourceObjectId,
        relations: &mut Relations,
        usage: TypeUsage,
    ) -> Symbol {
        let mut local = None;
        let var_name = usage.name();
        if var_name.parts().len() == 1 {
            local = self.locals.position_reachable_local(var_name.simple_name());
        }
        match local {
            Some(var) => Symbol::Local(var),
            None => {
                let id = *self
                    .external_usages
                    .entry(usage)
                    .or_insert_with(|| relations.track_new_object(state));
                id.into()
            }
        }
    }

    pub fn get_var(&self, id: ObjectId) -> Option<&Variable> {
        self.locals.vars.get(id)
    }

    /// Gets the symbol associated with an already known name.
    pub fn get_symbol(&self, usage: &TypeUsage) -> Option<Symbol> {
        let name = usage.name();
        self.locals
            .vars
            .iter()
            .position(|var| name.parts().len() == 1 && var.name == name.simple_name() /*&& var.depth.is_some()*/)
            .map(Symbol::Local)
            .or_else(|| {
                self.external_usages
                    .get(usage)
                    .map(|id| Symbol::Global(id.0))
            })
    }

    /// Iterates over all the exported variables, local to the environment.
    pub fn exported_vars(&self) -> impl Iterator<Item = &Variable> {
        //consider for now that all local vars are exported.
        self.locals.vars.iter()
    }

    /// Iterates over all the global variable ids, with their corresponding name.
    pub fn external_usages(&self) -> impl Iterator<Item = (&TypeUsage, GlobalObjectId)> {
        self.external_usages.iter().map(|(name, id)| (name, *id))
    }

    /// Gets the name of a global variable.
    ///
    /// This returns the name only if the global object comes from this environment.
    pub fn get_external_symbol_usage(&self, object_id: GlobalObjectId) -> Option<&TypeUsage> {
        self.external_usages
            .iter()
            .find_map(|(usage, &id)| (id == object_id).then_some(usage))
    }

    pub fn begin_scope(&mut self) {
        self.locals.begin_scope();
    }

    pub fn end_scope(&mut self) {
        self.locals.end_scope();
    }
}

#[derive(Debug, Clone)]
struct Locals {
    /// The actual list of seen and unique variables.
    vars: Vec<Variable>,

    /// The current depth of the scope.
    ///
    /// Scopes indices are 1-based, so the first scope is 1.
    /// This allows to reserve the 0 index for non reachable variables.
    current_depth: NonZeroUsize,
}

impl Locals {
    /// Adds a new variable and binds it to the current scope.
    fn declare(&mut self, name: String, ty: TypeInfo) -> Symbol {
        let id = self.vars.len();
        self.vars.push(Variable {
            name,
            depth: Some(self.current_depth),
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
        self.current_depth = self.current_depth.checked_add(1).expect("Too many scopes");
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
            .take_while(|var| var.depth == Some(self.current_depth))
            .for_each(|var| {
                var.depth.take();
            });

        self.current_depth =
            NonZeroUsize::new(self.current_depth.get() - 1).expect("Cannot end the root scope");
    }

    /// Looks up a variable by name that is reachable from the current scope.
    fn lookup_reachable_local(&self, name: &str) -> Option<&Variable> {
        self.vars
            .iter()
            .rev()
            .find(|var| var.name == name && var.depth.is_some())
    }

    /// Gets the position of a variable from the current scope.
    fn position_reachable_local(&self, name: &str) -> Option<ObjectId> {
        self.vars
            .iter()
            .rev()
            .position(|var| var.name == name && var.depth.is_some())
            .map(|idx| self.vars.len() - 1 - idx)
    }
}

impl Default for Locals {
    fn default() -> Self {
        Self {
            vars: Vec::new(),
            current_depth: NonZeroUsize::new(1).unwrap(),
        }
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
    /// pass of the analyzer. The value is guaranteed to be relevant only if the scope
    /// has not ended yet. If not, the value is undefined.
    ///
    /// Using an [`Option<NonZeroUsize>`] allows to bake the scope depth and the
    /// variable reachability in the same 8 bytes on a 64-bit architecture.
    depth: Option<NonZeroUsize>,
}

impl Variable {
    /// Creates a new variable.
    ///
    /// This convenience method accepts zero as a depth, which is the internal
    /// representation of a non reachable variable.
    pub fn scoped(name: String, depth: usize) -> Self {
        Self {
            name,
            depth: NonZeroUsize::try_from(depth).ok(),
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
            Some(&Variable::scoped("foo".to_owned(), 1))
        );

        assert_eq!(
            locals.lookup_reachable_local("bar"),
            Some(&Variable::scoped("bar".to_owned(), 2))
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
            Some(&Variable::scoped("foo".to_owned(), 3))
        );
        locals.end_scope();
        assert_eq!(
            locals.lookup_reachable_local("foo"),
            Some(&Variable::scoped("foo".to_owned(), 1))
        );
        locals.end_scope();
        assert_eq!(
            locals.lookup_reachable_local("foo"),
            Some(&Variable::scoped("foo".to_owned(), 1))
        );
    }
}
