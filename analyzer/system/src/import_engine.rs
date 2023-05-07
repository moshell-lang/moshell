use crate::environment::Environment;
use crate::name::Name;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

///The import engine is a structure that hosts all the imported symbols of an environment.
/// Its implementation allows to insert new imports in the engine
#[derive(Debug, Clone, PartialEq, Default)]
pub struct ImportEngine {
    content: ImportEngineContent,
}

/// The content of the ImportEngines
#[derive(Debug, Clone, Default)]
pub struct ImportEngineContent {
    /// This map contains the imported symbols.
    /// Where the key is the symbol name (or an alias) and the value is the bound symbol.
    imported_symbols: HashMap<String, ImportedSymbol>,
}

impl PartialEq for ImportEngineContent {
    fn eq(&self, other: &Self) -> bool {
        other.imported_symbols == self.imported_symbols
    }
}

/// An imported symbol struct hosts the information
/// of a symbol such as if it was used or not or explicitly imported
#[derive(Debug, Clone, PartialEq)]
struct ImportedSymbol {
    ///Symbol environment's full name
    env_fqn: Name,
    ///Symbol name relative to its environment
    symbol_name: Name,
    ///A flag to track if the import was already used.
    used: bool,
    ///True if this symbol was explicitly imported.
    /// A symbol is explicitly imported if it was imported by [ImportEngine::import] or [ImportEngine::import_aliased].
    /// Implicitly imported symbols are symbols imported by [ImportEngine::import_all_in]
    explicitly_imported: bool,
}

///An unused symbol. (returned by list_unused methods)
#[derive(Debug, PartialEq, Eq)]
pub struct UnusedSymbol {
    ///The full name of the symbol, environment's name included.
    pub symbol_fqn: Name,
    ///True if this symbol was explicitly imported.
    /// See [ImportedSymbol.explicitly_imported]
    pub explicitly_imported: bool,
}

///A trait to access exported symbols of a context.
pub trait ContextExports<S> {
    ///Retrieve the context from given environment.
    fn from_env(env: Rc<RefCell<Environment>>) -> Rc<RefCell<Self>>;

    ///Find an exported symbol of type S in this context with given relative name
    fn find_exported(&self, name: &Name) -> Option<S>;

    ///List direct childrens of given symbol's name that are exported.
    fn list_exported_names(&self, symbol: Option<Name>) -> Vec<Name>;
}

impl ImportEngine {
    ///Creates a new engine with lang pre-imported
    ///This function panics if the given imports are invalid.
    pub(crate) fn with_imports_unchecked<const N: usize>(imports: [&str; N]) -> Self {
        let mut s = Self::default();
        for fqn in imports {
            let symbol_fqn = Name::new(fqn);
            s.content.import(&symbol_fqn).expect("unchecked");
        }
        s
    }
}

impl ImportEngineContent {
    ///Lists all unused symbols imported in the engine.
    /// All symbols from the lang environment are filtered out.
    fn list_unused(&self) -> Vec<UnusedSymbol> {
        self.imported_symbols
            .iter()
            .filter(|(_, import)| !import.used && import.env_fqn.root() != "lang")
            .map(|(alias, import)| {
                //merge env and symbol names
                let parts = import.env_fqn.appended(import.symbol_name.clone());
                let symbol_fqn = parts.with_name(alias);
                UnusedSymbol {
                    symbol_fqn,
                    explicitly_imported: import.explicitly_imported,
                }
            })
            .collect()
    }

    ///Imports the provided symbol.
    /// Can fail if the symbols or its environment is not found.
    /// This import will shadow any alias of the same symbol if it was previously imported
    fn import(&mut self, fqn: &Name) -> Result<(), String> {
        self.import_aliased(fqn, fqn.simple_name())
    }

    ///Imports the provided symbol with an alias.
    /// Can fail if the symbols or its environment is not found.
    /// This import will shadow any alias of the same symbol if it was previously imported
    /// Accessing the symbol with its original name will not work if not explicitly or implicitly re-imported
    fn import_aliased(&mut self, fqn: &Name, alias: &str) -> Result<(), String> {
        /*let layers = self.layers.upgrade().expect("used layers got cleaned up");
        let env = layers
            .borrow()
            .get_env_of(fqn)
            .ok_or_else(|| format!("unknown module {fqn}"))?;
        let env_fqn = env.borrow().fqn.clone();
        let inner_name = fqn.relative_to(&env_fqn);

        if let Some(inner_name) = &inner_name {
            if env.borrow().find_exported(inner_name).is_none() {
                return Err(format!(
                    "symbol {inner_name} in module {env_fqn} not found."
                ));
            }
        }*/

        self.imported_symbols.insert(
            alias.to_string(),
            ImportedSymbol {
                symbol_name: fqn.clone(),
                env_fqn: fqn.clone(),
                used: false,
                explicitly_imported: true,
            },
        );
        Ok(())
    }

    //remove all the symbols childrens of given symbol fqn
    fn remove_all_in(&mut self, fqn: &Name) {
        self.imported_symbols = self
            .imported_symbols
            .drain()
            .filter(|(_, v)| {
                let import_fqn = v.env_fqn.appended(v.symbol_name.clone());
                fqn == &import_fqn
            })
            .collect()
    }

    ///Imports all direct childs of the symbol fqn.
    /// The imported symbols will then be considered as implicitely imported.
    fn import_all_in(&mut self, _fqn: &Name) -> Result<(), String> {
        todo!("import all in")
    }
}
