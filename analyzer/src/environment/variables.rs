use crate::relations::{GlobalObjectId, ObjectId, Relations, SourceObjectId, Symbol};
use std::num::NonZeroUsize;
use ast::variable::{VarReference as VarReferenceExpr, VarReference};
use indexmap::IndexMap;

#[derive(Debug, Copy, Clone, Default, PartialEq, Eq)]
pub enum TypeInfo {
    #[default]
    Unknown,
    Ref(Symbol),
}

/// A collection of variables
#[derive(Debug, Clone, Default)]
pub struct Variables<'a> {
    locals: Locals,

    external_references: ExternalVariableReferences<'a>,
}

#[derive(Debug, Clone, Default)]
pub struct ExternalVariableReferences<'a> {
    references: IndexMap<String, ExternalVariableReference<'a>>,
}

#[derive(Debug, Clone)]
pub struct ExternalVariableReference<'a> {
    pub relation_tracking_pos: GlobalObjectId,
    pub references_expr: Vec<&'a VarReferenceExpr<'a>>,
}

impl<'a> ExternalVariableReference<'a> {
    pub fn new(relation: GlobalObjectId) -> Self {
        Self {
            relation_tracking_pos: relation,
            references_expr: Vec::new(),
        }
    }
}

impl<'a> Variables<'a> {
    pub fn declare_local(&mut self, name: String) {
        self.locals.declare(name);
    }

    /// Identifies a named variable to a binding.
    ///
    /// If the variable is not reachable from the current scope, it is considered a global variable.
    pub fn identify(
        &mut self,
        state: SourceObjectId,
        relations: &mut Relations<'a>,
        var: &'a VarReference<'a>,
    ) -> Symbol {
        match self.locals.position_reachable_local(var.name) {
            Some(var) => Symbol::Local(var),
            None => {
                let ext = self
                    .external_references
                    .references
                    .entry(var.name.to_string())
                    .or_insert_with(|| ExternalVariableReference::new(relations.track_new_object(state)));
                ext.references_expr.push(var);
                ext.relation_tracking_pos
                    .into()
            }
        }
    }

    pub fn exported_vars(&self) -> impl Iterator<Item=&LocalVariable> {
        //consider for now that all local vars are exported.
        self.locals.vars.iter()
    }

    pub fn external_vars(&self) -> impl Iterator<Item=(&String, &ExternalVariableReference<'a>)> {
        self.external_references.references.iter().map(|(name, id)| (name, id))
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
    vars: Vec<LocalVariable>,

    /// The current depth of the scope.
    ///
    /// Scopes indices are 1-based, so the first scope is 1.
    /// This allows to reserve the 0 index for non reachable variables.
    current_depth: NonZeroUsize,
}

impl Locals {
    /// Adds a new variable and binds it to the current scope.
    fn declare(&mut self, name: String) {
        self.vars.push(LocalVariable {
            name,
            depth: Some(self.current_depth),
            ty: TypeInfo::Unknown,
        });
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
    fn lookup_reachable_local(&self, name: &str) -> Option<&LocalVariable> {
        self.vars
            .iter()
            .rev()
            .find(|var| var.name == name && var.depth.is_some())
    }

    fn position_reachable_local(&self, name: &str) -> Option<ObjectId> {
        self.vars
            .iter()
            .rev()
            .position(|var| var.name == name && var.depth.is_some())
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
pub struct LocalVariable {
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

impl LocalVariable {
    /// Creates a new variable.
    ///
    /// This convenience method accepts zero as a depth, which is the internal
    /// representation of a non reachable variable.
    fn scoped(name: String, depth: usize) -> Self {
        Self {
            name,
            depth: NonZeroUsize::try_from(depth).ok(),
            ty: TypeInfo::Unknown,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn access_by_name() {
        let mut locals = Locals::default();
        locals.declare("foo".to_owned());
        locals.begin_scope();
        locals.declare("bar".to_owned());
        assert_eq!(
            locals.lookup_reachable_local("foo"),
            Some(&LocalVariable::scoped("foo".to_owned(), 1))
        );

        assert_eq!(
            locals.lookup_reachable_local("bar"),
            Some(&LocalVariable::scoped("bar".to_owned(), 2))
        );
    }

    #[test]
    fn access_out_of_scope() {
        let mut locals = Locals::default();
        locals.begin_scope();
        locals.declare("bar".to_owned());
        locals.end_scope();
        assert_eq!(locals.lookup_reachable_local("bar"), None);
        locals.begin_scope();
        assert_eq!(locals.lookup_reachable_local("bar"), None);
    }

    #[test]
    fn shadow_nested() {
        let mut locals = Locals::default();
        locals.declare("foo".to_owned());
        locals.begin_scope();
        locals.begin_scope();
        locals.declare("foo".to_owned());
        assert_eq!(
            locals.lookup_reachable_local("foo"),
            Some(&LocalVariable::scoped("foo".to_owned(), 3))
        );
        locals.end_scope();
        assert_eq!(
            locals.lookup_reachable_local("foo"),
            Some(&LocalVariable::scoped("foo".to_owned(), 1))
        );
        locals.end_scope();
        assert_eq!(
            locals.lookup_reachable_local("foo"),
            Some(&LocalVariable::scoped("foo".to_owned(), 1))
        );
    }
}
