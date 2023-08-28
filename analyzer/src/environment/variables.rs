use crate::diagnostic::{Diagnostic, DiagnosticID, Observation};
use crate::name::Name;
use crate::reef::{Reef, ReefContext, ReefId};
use ast::r#use::InclusionPathItem;
use context::source::{SourceSegment, SourceSegmentHolder};
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};

use crate::relations::{LocalId, RelationId, SourceId, Symbol};

/// Information over the declared type of a variable
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TypeInfo {
    /// The variable is a regular variable
    Variable,
    /// The variable is a function declaration
    Function,
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub enum SymbolPathItem {
    Reef(SourceSegment),
    Symbol(String, SourceSegment),
}

impl SourceSegmentHolder for SymbolPathItem {
    fn segment(&self) -> SourceSegment {
        match self {
            SymbolPathItem::Reef(s) => s.clone(),
            SymbolPathItem::Symbol(_, s) => s.clone(),
        }
    }
}

impl Display for TypeInfo {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeInfo::Variable => write!(f, "variable"),
            TypeInfo::Function => write!(f, "function"),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct SymbolLocation {
    pub name: Name,
    pub is_current_reef_explicit: bool,
}

impl SymbolLocation {
    pub fn in_current_reef(name: Name) -> Self {
        Self {
            name,
            is_current_reef_explicit: true,
        }
    }

    pub fn unspecified(name: Name) -> Self {
        Self {
            name,
            is_current_reef_explicit: false,
        }
    }

    pub fn compute<'a>(
        path: &'a [InclusionPathItem<'a>],
        source_id: SourceId,
        must_be_relative: bool,
    ) -> Result<Self, Diagnostic> {
        let current_reef = path
            .first()
            .is_some_and(|f| !must_be_relative && matches!(f, InclusionPathItem::Reef(_)));

        let mut path_it = path.iter();

        if current_reef {
            path_it.next();
        }

        let mut parts = Vec::new();
        let mut observations = Vec::new();
        for it in path_it {
            match it {
                InclusionPathItem::Reef(seg) => observations.push(Observation::context(
                    source_id,
                    seg.clone(),
                    "`reef` keyword is invalid here.",
                )),
                InclusionPathItem::Symbol(item, _) => parts.push(item.to_string()),
            }
        }

        if !observations.is_empty() {
            return Err(Diagnostic::new(
                DiagnosticID::InvalidSymbolPath,
                "Symbol path contains invalid items",
            )
            .with_observations(observations));
        }

        Ok(Self {
            name: Name::from(parts),
            is_current_reef_explicit: current_reef,
        })
    }
}

pub fn resolve_loc<'a, 'e>(
    loc: &SymbolLocation,
    ctx: &'a ReefContext<'a, 'e>,
) -> Option<(&'a Reef<'e>, ReefId)> {
    if loc.is_current_reef_explicit {
        return Some((ctx.current_reef(), ctx.reef_id));
    }

    let reef_name = loc.name.root();
    ctx.reefs().get_reef_by_name(reef_name)
}

/// A collection of variables
#[derive(Debug, Clone, Default)]
pub struct Variables {
    /// Locals declarations
    locals: Locals,

    /// Relations with external variables.
    /// The key is the variable Names, where value is the relation with another external environment's [Locals]
    externals: HashMap<SymbolLocation, RelationId>,
}

impl Variables {
    /// Creates a new local variable.
    pub fn declare_local(&mut self, name: String, ty: TypeInfo) -> Symbol {
        self.locals.declare(name, ty)
    }

    /// Returns the local variable associated with the id
    pub fn get_var(&self, id: LocalId) -> Option<&Variable> {
        self.locals.vars.get(id.0)
    }

    /// Returns an entry for the given external symbol name relation
    pub fn external(&mut self, name: SymbolLocation) -> Entry<SymbolLocation, RelationId> {
        self.externals.entry(name)
    }

    /// Finds the local identifier associated with an already known name.
    ///
    /// The lookup uses the current scope, which is frequently updated during the collection phase.
    /// That's the main reason why this method should be used in pair the variable capture
    /// resolution, immediately after the closure is observed and inertly populated.
    pub fn find_reachable(&self, name: &str) -> Option<LocalId> {
        self.locals.position_reachable_local(name)
    }

    /// Finds the local exported symbol associated with an already known name.
    ///
    /// Exported symbols are always declared in the outermost scope, and should be checked only
    /// after the whole environment is collected.
    pub fn find_exported(&self, name: &str) -> Option<LocalId> {
        self.locals
            .vars
            .iter()
            .rev()
            .position(|var| var.name == name && var.is_exported())
            .map(|idx| LocalId(self.locals.vars.len() - 1 - idx))
    }

    /// Lists all local variables, in the order they are declared.
    ///
    /// This exposes their current state, which is frequently updated.
    /// Use [`Variables::find_reachable`] to lookup any variable during the collection phase,
    /// or [`Variables::find_exported`] to lookup an exported variable after the collection phase.
    pub fn all_vars(&self) -> &[Variable] {
        &self.locals.vars
    }

    /// returns an iterator over all variables, with their local identifier
    pub fn iter(&self) -> impl Iterator<Item = (LocalId, &Variable)> {
        self.locals
            .vars
            .iter()
            .enumerate()
            .map(|(i, v)| (LocalId(i), v))
    }

    /// Iterates over all the exported variables, local to the environment.
    pub fn exported_vars(&self) -> impl Iterator<Item = &Variable> {
        //consider for now that all local vars of the outermost scope are exported
        self.locals.vars.iter().filter(|var| var.depth == -1)
    }

    /// Iterates over all the global variable ids, with their corresponding name.
    pub fn external_vars(&self) -> impl Iterator<Item = (&SymbolLocation, RelationId)> {
        self.externals.iter().map(|(loc, id)| (loc, *id))
    }

    /// Finds the name of an external variable.
    ///
    /// This returns the name only if the global object comes from this environment.
    pub fn find_external_symbol_name(&self, object_id: RelationId) -> Option<&SymbolLocation> {
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
        Symbol::Local(LocalId(id))
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
    fn position_reachable_local(&self, name: &str) -> Option<LocalId> {
        self.vars
            .iter()
            .rev()
            .position(|var| var.name == name && var.depth >= 0)
            .map(|idx| LocalId(self.vars.len() - 1 - idx))
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
