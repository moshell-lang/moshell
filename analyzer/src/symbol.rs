use crate::module::Export;
use crate::typing::user::TypeId;
use crate::typing::ErroneousSymbolDesc;
use context::source::Span;
use std::fmt;
use std::path::PathBuf;

/// A binding that can be accessed by a name.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Symbol {
    /// The name from which this symbol can be accessed.
    pub name: String,

    /// The known type of the symbol.
    ///
    /// The type should not be changed after it is set.
    pub ty: TypeId,

    /// The depth at which this symbol was declared.
    scope: usize,

    /// The byte span where this symbol was declared.
    pub declared_at: Span,

    /// The kind of symbol this is.
    pub registry: SymbolRegistry,
}

/// A short representation of a [`Symbol`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SymbolDesc {
    /// The kind of symbol this is.
    pub registry: SymbolRegistry,

    /// The byte span where this symbol was declared.
    pub span: Span,
}

impl From<&Symbol> for SymbolDesc {
    fn from(symbol: &Symbol) -> Self {
        Self {
            registry: symbol.registry,
            span: symbol.declared_at.clone(),
        }
    }
}

/// A hint of where the symbol could be found.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum SymbolRegistry {
    /// A named variable.
    Variable,

    /// A named function.
    ///
    /// When there is both a function and a variable with the same name and when we are looking
    /// for a function call, the function should be preferred.
    Function,

    /// A named type, either a struct or a module.
    Type,
}

impl fmt::Display for SymbolRegistry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SymbolRegistry::Variable => write!(f, "variable"),
            SymbolRegistry::Function => write!(f, "function"),
            SymbolRegistry::Type => write!(f, "type"),
        }
    }
}

/// Tracks the currently reachable symbols.
pub(crate) struct SymbolTable {
    /// The path to the file that this symbol table is for.
    pub(crate) path: PathBuf,

    /// The symbols in scope.
    ///
    /// The last symbol is the most recently defined one and the first symbol to be looked up
    /// when resolving a symbol.
    symbols: Vec<Symbol>,

    /// Tracks the current depth in order to detect when symbols become out of scope.
    pub(crate) current_depth: usize,
}

impl SymbolTable {
    /// Creates a new table for the given file path.
    pub(super) fn new(path: PathBuf) -> Self {
        Self {
            path,
            symbols: Vec::new(),
            current_depth: 0,
        }
    }

    /// Registers a new symbol that is defined in the current scope.
    ///
    /// It may be retrieved by its name and registry while it is reachable. Symbols are dropped when
    /// going out of scope and masked when a new symbol with the same name is defined.
    pub(crate) fn insert_local(
        &mut self,
        name: String,
        ty: TypeId,
        declared_at: Span,
        registry: SymbolRegistry,
    ) {
        self.symbols.push(Symbol {
            name,
            ty,
            scope: self.current_depth,
            declared_at,
            registry,
        });
    }

    /// Inserts a symbol that comes from an external source.
    pub(crate) fn insert_remote(
        &mut self,
        name: String,
        imported_at: Span,
        Export { ty, registry, .. }: &Export,
    ) {
        self.symbols.push(Symbol {
            name,
            ty: *ty,
            scope: self.current_depth,
            declared_at: imported_at,
            registry: *registry,
        });
    }

    /// Gets a named binding that refers to a specific registry.
    ///
    /// The same identifier may be used for different kinds of bindings, such as a variable and a
    /// function. This method will use the most recently defined binding that matches the given
    /// registry.
    pub(crate) fn get(&self, name: &str, registry: SymbolRegistry) -> Option<&Symbol> {
        self.symbols
            .iter()
            .rev()
            .find(|symbol| symbol.name == name && symbol.registry == registry)
    }

    /// Tries to find a symbol by its name and registry.
    ///
    /// It tries to find a similar symbol in the [`Err`] variant that may help the user to find the
    /// correct symbol.
    pub(crate) fn lookup(
        &self,
        name: &str,
        registry: SymbolRegistry,
    ) -> Result<&Symbol, UndefinedSymbol> {
        self.lookup_position(name, registry)
            .map(|(_, symbol)| symbol)
    }

    pub(crate) fn lookup_position(
        &self,
        name: &str,
        registry: SymbolRegistry,
    ) -> Result<(usize, &Symbol), UndefinedSymbol> {
        let mut other_symbol: Option<&Symbol> = None;
        for (idx, symbol) in self.symbols.iter().enumerate().rev() {
            if symbol.name == name {
                if symbol.registry == registry {
                    return Ok((idx, symbol));
                } else {
                    other_symbol = Some(symbol);
                }
            }
        }
        Err(match other_symbol {
            Some(symbol) => UndefinedSymbol::WrongRegistry(SymbolDesc::from(symbol)),
            None => UndefinedSymbol::NotFound,
        })
    }

    pub(crate) fn len(&self) -> usize {
        self.symbols.len()
    }

    pub(crate) fn enter_scope(&mut self) {
        self.current_depth += 1;
    }

    pub(crate) fn exit_scope(&mut self) {
        self.current_depth -= 1;
        self.symbols
            .retain(|symbol| symbol.scope <= self.current_depth);
    }
}

/// Some [`Symbol`] could not be found.
pub(super) enum UndefinedSymbol {
    /// No symbol with the given name was found.
    NotFound,

    /// A symbol with the same name was found but with a different [`SymbolRegistry`].
    WrongRegistry(SymbolDesc),
}

impl From<UndefinedSymbol> for Option<SymbolDesc> {
    fn from(undefined: UndefinedSymbol) -> Self {
        match undefined {
            UndefinedSymbol::NotFound => None,
            UndefinedSymbol::WrongRegistry(symbol) => Some(symbol),
        }
    }
}

impl From<UndefinedSymbol> for Option<ErroneousSymbolDesc> {
    fn from(undefined: UndefinedSymbol) -> Self {
        Option::<SymbolDesc>::from(undefined).map(ErroneousSymbolDesc::Complete)
    }
}

impl From<&Export> for SymbolDesc {
    fn from(export: &Export) -> Self {
        Self {
            registry: export.registry,
            span: export.span.clone(),
        }
    }
}
