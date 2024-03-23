use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};

use ast::r#use::InclusionPathItem;
use context::source::{SourceSegment, SourceSegmentHolder};

use crate::engine::Engine;
use crate::name::Name;
use crate::reef::{Externals, ReefId};
use crate::relations::{LocalId, RelationId};

/// Information over the declared type of a variable
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum SymbolInfo {
    /// The symbol is a regular variable
    Variable,
    /// The symbol is a function declaration
    Function,
    /// The symbol is a type
    Type,

    /// A magic symbol
    Magic(MagicSymbolKind),
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum MagicSymbolKind {
    /// This magic symbol refers to the program's arguments
    ProgramArguments,
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

impl Display for SymbolInfo {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SymbolInfo::Variable => write!(f, "variable"),
            SymbolInfo::Function => write!(f, "function"),
            SymbolInfo::Type => write!(f, "type"),
            SymbolInfo::Magic(MagicSymbolKind::ProgramArguments) => write!(f, "program arguments"),
        }
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum SymbolRegistry {
    /// type symbols
    Types,
    /// Variable and functions symbols
    Objects,
    /// Specific magic variable
    Magic(MagicSymbolKind),
}

impl SymbolRegistry {
    /// returns true if the given symbol info is part of this registry
    pub(crate) fn accepts(self, kind: SymbolInfo) -> bool {
        match self {
            SymbolRegistry::Types => matches!(kind, SymbolInfo::Type),
            SymbolRegistry::Objects => matches!(kind, SymbolInfo::Variable | SymbolInfo::Function),
            SymbolRegistry::Magic(pr) => matches!(kind, SymbolInfo::Magic(pl) if pr == pl),
        }
    }
}

/// A symbol location is the result of the resolution of a sequence of [SymbolPathItem] (see [SymbolLocation::compute]).
#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct SymbolLocation {
    /// The resolved name, can be relative only if [`is_current_reef_explicit`] is set to false.
    pub name: Name,
    /// If set to true, this location is an absolute location pointing to the current reef.
    pub is_current_reef_explicit: bool,
}

impl SymbolLocation {
    /// constructs an absolute symbol location, pointing to the current reef
    pub fn in_current_reef(fqn: Name) -> Self {
        Self {
            name: fqn,
            is_current_reef_explicit: true,
        }
    }

    /// constructs a a symbol location, without specifying that the given name is absolute or relative, and
    /// if this location targets the current reef.
    pub fn unspecified(name: Name) -> Self {
        Self {
            name,
            is_current_reef_explicit: false,
        }
    }

    /// Computes a symbol location from a given slice of [InclusionPathItem],
    /// returning `Err(Vec<SourceSegment>)` if the path input contains invalid items, where the vector's segments are
    /// the invalid item segments.
    ///
    /// A path is invalid if it contains any non-heading [InclusionPathItem::Reef] item.
    /// If the [`must_be_relative`] flag is set, the path must not contain any [InclusionPathItem::Reef] to be valid.
    ///
    /// The function can also fail if the `must_be_relative`
    pub fn compute(path: &[InclusionPathItem]) -> Result<Self, Vec<SourceSegment>> {
        let current_reef = path
            .first()
            .is_some_and(|f| matches!(f, InclusionPathItem::Reef(_)));

        let mut path_it = path.iter();

        if current_reef && path.len() > 1 {
            path_it.next();
        }

        let mut parts = Vec::new();
        let mut bad_segments = Vec::new();
        for it in path_it {
            match it {
                InclusionPathItem::Reef(seg) => bad_segments.push(seg.clone()),
                InclusionPathItem::Symbol(ident) => parts.push(ident.to_string()),
            }
        }

        if !bad_segments.is_empty() {
            return Err(bad_segments);
        }

        Ok(Self {
            name: Name::from(parts),
            is_current_reef_explicit: current_reef,
        })
    }
}

pub fn resolve_loc<'a, 'e>(
    loc: &SymbolLocation,
    current: &'a Engine<'e>,
    externals: &'a Externals<'e>,
) -> Option<(&'a Engine<'e>, ReefId)> {
    if loc.is_current_reef_explicit {
        Some((current, externals.current))
    } else {
        let reef_name = loc.name.root();
        externals
            .get_reef_by_name(reef_name)
            .map(|(reef, id)| (&reef.engine, id))
    }
}

/// A collection of variables
#[derive(Debug, Clone, Default)]
pub struct Symbols {
    /// Locals declarations
    locals: Locals,

    /// Relations with external variables.
    /// The key is the variable Names, where value is the concerned symbol registry,
    /// with relation to another external environment symbols.
    externals: HashMap<SymbolLocation, RelationId>,
}

impl Symbols {
    /// Creates a new named local variable.
    pub fn declare_local(&mut self, name: String, ty: SymbolInfo) -> LocalId {
        self.locals.declare(name, ty)
    }

    /// Creates a new magic variable
    pub fn declare_magic(&mut self, ty: MagicSymbolKind) -> LocalId {
        self.locals.declare_magic(ty)
    }

    pub fn find_magic(&self, ty: MagicSymbolKind) -> Option<LocalId> {
        self.locals.find_magic(ty)
    }

    /// Returns the local variable associated with the id
    pub fn get(&self, id: LocalId) -> Option<&Symbol> {
        self.locals.vars.get(id.0)
    }

    /// Returns an entry for the given external symbol name relation
    pub fn external(&mut self, loc: SymbolLocation) -> Entry<SymbolLocation, RelationId> {
        self.externals.entry(loc)
    }

    /// Finds the local identifier associated with an already known name.
    ///
    /// The lookup uses the current scope, which is frequently updated during the collection phase.
    /// That's the main reason why this method should be used in pair the variable capture
    /// resolution, immediately after the closure is observed and inertly populated.
    pub fn find_reachable(&self, name: &str, registry: SymbolRegistry) -> Option<LocalId> {
        self.locals.position_reachable_local(name, registry)
    }

    /// Finds the local exported symbol associated with an already known name.
    ///
    /// Exported symbols are always declared in the outermost scope, and should be checked only
    /// after the whole environment is collected.
    pub fn find_exported(&self, name: &str, registry: SymbolRegistry) -> Option<LocalId> {
        self.locals
            .vars
            .iter()
            .rev()
            .position(|sym| sym.name == name && sym.is_exported() && registry.accepts(sym.ty))
            .map(|idx| LocalId(self.locals.vars.len() - 1 - idx))
    }

    /// Lists all local variables, in the order they are declared.
    ///
    /// This exposes their current state, which is frequently updated.
    /// Use [`Symbols::find_reachable`] to lookup any variable during the collection phase,
    /// or [`Symbols::find_exported`] to lookup an exported variable after the collection phase.
    pub fn all(&self) -> &[Symbol] {
        &self.locals.vars
    }

    /// returns an iterator over all variables, with their local identifier
    pub fn iter(&self) -> impl Iterator<Item = (LocalId, &Symbol)> {
        self.locals
            .vars
            .iter()
            .enumerate()
            .map(|(i, v)| (LocalId(i), v))
    }

    /// Return the amount of locals presents
    pub fn len(&self) -> usize {
        self.locals.vars.len()
    }

    pub fn is_empty(&self) -> bool {
        self.locals.vars.is_empty()
    }

    /// Iterates over all the exported symbols, local to the environment.
    pub fn exported_symbols(&self) -> impl Iterator<Item = (LocalId, &Symbol)> {
        //consider for now that all local vars of the outermost scope are exported
        self.locals
            .vars
            .iter()
            .enumerate()
            .filter(|(_, var)| var.is_exported())
            .map(|(id, var)| (LocalId(id), var))
    }

    /// Iterates over all the global symbol ids, with their corresponding name.
    pub fn external_symbols(&self) -> impl Iterator<Item = (&SymbolLocation, RelationId)> {
        self.externals.iter().map(|(loc, sym)| (loc, *sym))
    }

    /// Finds the name of an external symbol.
    ///
    /// This returns the name only if the global object comes from this environment.
    pub fn find_external_symbol_name(&self, object_id: RelationId) -> Option<&SymbolLocation> {
        self.externals
            .iter()
            .find_map(|(name, id)| (*id == object_id).then_some(name))
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
    vars: Vec<Symbol>,

    /// The current depth of the scope.
    ///
    /// The first scope is 0.
    current_depth: usize,
}

impl Locals {
    /// Adds a new symbol and binds it to the current scope.
    fn declare(&mut self, name: String, ty: SymbolInfo) -> LocalId {
        let id = self.vars.len();
        self.vars.push(Symbol {
            name,
            depth: self.current_depth as isize,
            ty,
        });
        LocalId(id)
    }

    fn declare_magic(&mut self, ty: MagicSymbolKind) -> LocalId {
        let id = self.vars.len();
        self.vars.push(Symbol {
            name: String::default(),
            depth: -1, //exported
            ty: SymbolInfo::Magic(ty),
        });
        LocalId(id)
    }

    pub(crate) fn find_magic(&self, ty: MagicSymbolKind) -> Option<LocalId> {
        self.vars
            .iter()
            .position(|var| var.ty == SymbolInfo::Magic(ty))
            .map(LocalId)
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
    fn lookup_reachable_local(&self, name: &str, registry: SymbolRegistry) -> Option<&Symbol> {
        self.vars
            .iter()
            .rev()
            .find(|var| var.depth >= 0 && var.name == name && registry.accepts(var.ty))
    }

    /// Gets the variable id from the current scope.
    fn position_reachable_local(&self, name: &str, registry: SymbolRegistry) -> Option<LocalId> {
        self.vars
            .iter()
            .rev()
            .position(|var| var.depth >= 0 && var.name == name && registry.accepts(var.ty))
            .map(|idx| LocalId(self.vars.len() - 1 - idx))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Symbol {
    /// The name identifier of the symbol.
    pub name: String,

    /// Additional information about the symbol
    pub ty: SymbolInfo,

    /// The depth of the symbol.
    ///
    /// This is used to keep track if the variable is still reachable during the first
    /// pass of the analyzer. The value is positive if the symbol's scope has not ended
    /// yet. If it is out of scope, the value is negative, with the absolute value being
    /// the depth of the scope where the variable was declared.
    depth: isize,
}

impl Symbol {
    /// Creates a new symbol.
    ///
    /// This convenience method accepts negative values as depths, which are the internal
    /// representations of unreachable variables.
    pub fn scoped(name: String, depth: isize) -> Self {
        Self {
            name,
            depth,
            ty: SymbolInfo::Variable,
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
        locals.declare("foo".to_owned(), SymbolInfo::Variable);
        locals.begin_scope();
        locals.declare("bar".to_owned(), SymbolInfo::Variable);
        assert_eq!(
            locals.lookup_reachable_local("foo", SymbolRegistry::Objects),
            Some(&Symbol::scoped("foo".to_owned(), 0))
        );

        assert_eq!(
            locals.lookup_reachable_local("bar", SymbolRegistry::Objects),
            Some(&Symbol::scoped("bar".to_owned(), 1))
        );

        assert_eq!(
            locals.lookup_reachable_local("bar", SymbolRegistry::Types),
            None
        );
    }

    #[test]
    fn access_out_of_scope() {
        let mut locals = Locals::default();
        locals.begin_scope();
        locals.declare("bar".to_owned(), SymbolInfo::Variable);
        locals.end_scope();
        assert_eq!(
            locals.lookup_reachable_local("bar", SymbolRegistry::Objects),
            None
        );
        locals.begin_scope();
        assert_eq!(
            locals.lookup_reachable_local("bar", SymbolRegistry::Objects),
            None
        );
    }

    #[test]
    fn shadow_nested() {
        let mut locals = Locals::default();
        locals.declare("foo".to_owned(), SymbolInfo::Variable);
        locals.begin_scope();
        locals.begin_scope();
        locals.declare("foo".to_owned(), SymbolInfo::Variable);
        assert_eq!(
            locals.lookup_reachable_local("foo", SymbolRegistry::Objects),
            Some(&Symbol::scoped("foo".to_owned(), 2))
        );
        locals.end_scope();
        assert_eq!(
            locals.lookup_reachable_local("foo", SymbolRegistry::Objects),
            Some(&Symbol::scoped("foo".to_owned(), 0))
        );
        locals.end_scope();
        assert_eq!(
            locals.lookup_reachable_local("foo", SymbolRegistry::Objects),
            Some(&Symbol::scoped("foo".to_owned(), 0))
        );
    }
}
