//! Resolves paths to types.
//!
//! Some expressions contain arbitrary paths such as function paths or type paths. Most of the time,
//! those paths contain a single symbol, but they can also contain multiple symbols separated by
//! two colons. For instance, `std::io::File` is a valid path that first contains where the `File`
//! symbol is located and then the type itself.

use crate::module::{ModuleTree, ModuleView};
use crate::symbol::{SymbolRegistry, UndefinedSymbol};
use crate::typing::user::{TypeArena, TypeId, UserType};
use crate::typing::variable::{SymbolEntry, VariableTable};
use crate::typing::ErroneousSymbolDesc;
use ast::r#use::InclusionPathItem;
use std::fmt;

/// A keyword or identifier with an identified type.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum PathEntry {
    /// A named variable.
    Variable,

    /// A named function.
    Function,

    /// A struct type.
    ///
    /// This excludes modules, in contrast to the [`SymbolRegistry::Type`] symbol variant.
    Type,

    /// A module that contains other modules and/or symbols.
    Module,

    /// A `reef` keyword.
    Reef,
}

impl fmt::Display for PathEntry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PathEntry::Variable => write!(f, "variable"),
            PathEntry::Function => write!(f, "function"),
            PathEntry::Type => write!(f, "type"),
            PathEntry::Module => write!(f, "module"),
            PathEntry::Reef => write!(f, "reef"),
        }
    }
}

impl From<SymbolEntry> for PathEntry {
    fn from(entry: SymbolEntry) -> Self {
        Self::from(SymbolRegistry::from(entry))
    }
}

impl From<SymbolRegistry> for PathEntry {
    fn from(registry: SymbolRegistry) -> Self {
        match registry {
            SymbolRegistry::Variable => Self::Variable,
            SymbolRegistry::Function => Self::Function,
            SymbolRegistry::Type => Self::Type,
        }
    }
}

impl From<&ErroneousSymbolDesc> for PathEntry {
    fn from(desc: &ErroneousSymbolDesc) -> Self {
        match desc {
            ErroneousSymbolDesc::Partial(entry) => *entry,
            ErroneousSymbolDesc::Complete(desc) => Self::from(desc.registry),
        }
    }
}

/// A search for the last symbol in a path.
///
/// Most of the time, the expected result may belong to only one [`SymbolRegistry`]. In those cases,
/// the search of the last symbol in the path can be done directly after constructing the search.
/// The [`crate::typing::lookup_type`] function may be used to do so.
pub(crate) struct SymbolSearch<'a> {
    /// The last symbol in the path.
    item: &'a InclusionPathItem,

    /// The search state after traversing the first part of the path.
    state: SearchState<'a>,
}

impl<'a> SymbolSearch<'a> {
    /// Creates a new search for the given path.
    ///
    /// The search will be initialized with all symbols in the path excepted the last one.
    /// If those first symbols are not found, an error will be returned. If not, the returned
    /// search can be used to query different registries where the last symbol might be found.
    pub(crate) fn new(
        path: &'a [InclusionPathItem],
        types: &'a TypeArena,
        modules: ModuleView<'a>,
        table: &'a VariableTable<'a>,
    ) -> Result<Self, PathItemError<'a>> {
        let (item, path) = path.split_last().expect("path should not be empty");
        let search = PathSearch::new(path, types, modules, table)?;
        Ok(Self {
            item,
            state: search.state,
        })
    }

    /// Looks up the last symbol in the path in the given registry.
    pub(crate) fn lookup(&self, registry: SymbolRegistry) -> Result<TypeId, PathItemError<'a>> {
        let InclusionPathItem::Symbol(ident) = self.item else {
            return Err(PathItemError {
                item: self.item,
                err: PathError::Invalid(PathEntry::Reef),
            });
        };
        match self.state {
            SearchState::Start { modules, table } => {
                match table.lookup(ident.value.as_str(), registry) {
                    Ok(symbol) => Ok(symbol.ty),
                    Err(err @ UndefinedSymbol::WrongRegistry(_)) => Err(PathItemError {
                        item: self.item,
                        err: PathError::from(err),
                    }),
                    Err(UndefinedSymbol::NotFound) => Err(PathItemError {
                        item: self.item,
                        err: if modules.get(self.item).is_some() {
                            PathError::Invalid(PathEntry::Module)
                        } else {
                            PathError::NotFound
                        },
                    }),
                }
            }
            SearchState::Module(tree) => match tree.find_export(ident.value.as_str(), registry) {
                Ok(export) => Ok(export.ty),
                Err(err) => Err(PathItemError {
                    item: self.item,
                    err: PathError::from(err),
                }),
            },
        }
    }
}

/// The first part of a symbol search.
struct PathSearch<'a> {
    types: &'a TypeArena,
    state: SearchState<'a>,
}

/// The query point for the next symbol in the path.
enum SearchState<'a> {
    /// The initial state at the beginning of the path.
    ///
    /// Before the first symbol in the path, it is not yet known whether the path is absolute and
    /// starts with the name of another reef or it relative to a symbol already in scope.
    Start {
        modules: ModuleView<'a>,
        table: &'a VariableTable<'a>,
    },

    /// The search is currently in a module.
    Module(&'a ModuleTree),
}

/// A [`PathError`] with the path item that caused the error.
pub struct PathItemError<'a> {
    /// The path item that caused the error.
    pub item: &'a InclusionPathItem,

    /// The error that occurred.
    pub err: PathError,
}

/// An error that occurred while searching for a path item.
pub enum PathError {
    /// The path item was not found.
    NotFound,

    /// The path item matches a symbol of an unexpected type.
    Invalid(PathEntry),

    /// The path does refer to an already invalid path.
    ErrorType,
}

impl From<UndefinedSymbol> for PathError {
    fn from(err: UndefinedSymbol) -> Self {
        match err {
            UndefinedSymbol::NotFound => Self::NotFound,
            UndefinedSymbol::WrongRegistry(desc) => Self::Invalid(PathEntry::from(desc.registry)),
        }
    }
}

impl From<PathError> for Option<ErroneousSymbolDesc> {
    fn from(err: PathError) -> Self {
        match err {
            PathError::NotFound => None,
            PathError::Invalid(entry) => Some(ErroneousSymbolDesc::Partial(entry)),
            PathError::ErrorType => None,
        }
    }
}

impl<'a> PathSearch<'a> {
    fn new(
        path: &'a [InclusionPathItem],
        types: &'a TypeArena,
        modules: ModuleView<'a>,
        table: &'a VariableTable<'a>,
    ) -> Result<Self, PathItemError<'a>> {
        let mut search = Self {
            types,
            state: SearchState::Start { modules, table },
        };
        for item in path {
            search.state = search
                .next_state(item)
                .map_err(|err| PathItemError { item, err })?;
        }
        Ok(search)
    }

    fn next_state(&self, item: &InclusionPathItem) -> Result<SearchState<'a>, PathError> {
        match self.state {
            SearchState::Start { modules, table } => {
                if let InclusionPathItem::Symbol(ident) = item {
                    match table.lookup(ident.value.as_str(), SymbolRegistry::Type) {
                        Ok(symbol) => {
                            return match self.types[symbol.ty] {
                                UserType::Module(ref path) => modules
                                    .get_direct(path)
                                    .map_or(Err(PathError::ErrorType), |tree| {
                                        Ok(SearchState::Module(tree))
                                    }),
                                UserType::Error => Err(PathError::ErrorType),
                                _ => Err(PathError::Invalid(PathEntry::Type)),
                            }
                        }
                        Err(UndefinedSymbol::WrongRegistry(symbol)) => {
                            return Err(PathError::Invalid(PathEntry::from(symbol.registry)));
                        }
                        Err(UndefinedSymbol::NotFound) => { /* continue */ }
                    }
                }
                if let Some(tree) = modules.get(item) {
                    Ok(SearchState::Module(tree))
                } else {
                    Err(PathError::NotFound)
                }
            }
            SearchState::Module(tree) => {
                let InclusionPathItem::Symbol(ident) = item else {
                    return Err(PathError::Invalid(PathEntry::Reef));
                };
                if let Some(tree) = tree.get(ident.value.as_ref()) {
                    Ok(SearchState::Module(tree))
                } else {
                    Err(PathError::NotFound)
                }
            }
        }
    }
}
