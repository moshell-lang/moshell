use crate::environment::Environment;
use crate::layers::ModuleLayers;
use crate::name::Name;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::{Rc, Weak};

///The import engine is a structure that hosts all the imported symbols of an environement.
/// Its implementation allows to insert new imports in the engine
#[derive(Debug, Clone, PartialEq)]
pub struct ImportEngine {
    content: Rc<RefCell<ImportEngineContent>>,
}

///The read only engine is a view of the ImportEngine that cannot import new symbols
#[derive(Debug, Clone, PartialEq)]
pub struct FixedImportEngine {
    content: Rc<RefCell<ImportEngineContent>>,
}

/// The content of the ImportEngines
#[derive(Debug, Clone)]
pub struct ImportEngineContent {
    /// This map contains the imported symbols.
    /// Where the key is the symbol name (or an alias) and the value is the bound symbol.
    imported_symbols: HashMap<String, ImportedSymbol>,
    /// The layers of the engine that hosts all the dependents environments.
    layers: Weak<RefCell<ModuleLayers>>,
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
    /// A symbol is explicitly imported if it was imported by [[ImportEngine::import]] or [[ImportEngine::import_aliased]].
    /// Implicitly imported symbols are symbols imported by [[ImportEngine::import_all_in]]
    explicitly_imported: bool,
}

///An unused symbol.
#[derive(Debug, PartialEq, Eq)]
pub struct UnusedSymbol {
    ///The full name of the symbol, environment's name included.
    pub symbol_fqn: Name,
    ///True if this symbol was explicitly imported.
    /// See [[ImportedSymbol.explicitly_imported]]
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

impl FixedImportEngine {
    ///See [[ImportEngineContent::use_symbol]]
    pub fn use_element<V, E: ContextExports<V>>(&mut self, name: &Name) -> Option<V> {
        self.content.borrow_mut().use_symbol::<V, E>(name)
    }

    ///See [[ImportEngineContent::list_unused]]
    pub fn list_unused(&self) -> Vec<UnusedSymbol> {
        self.content.borrow().list_unused()
    }
}

impl ImportEngine {
    ///Creates a read only view of this engine
    pub fn fixed(&self) -> FixedImportEngine {
        FixedImportEngine {
            content: self.content.clone(),
        }
    }

    ///Creates a new engine with all symbols in lang pre-imported.
    pub fn new(layers: Rc<RefCell<ModuleLayers>>) -> Self {
        let mut s = Self {
            content: Rc::new(RefCell::new(ImportEngineContent {
                imported_symbols: HashMap::default(),
                layers: Rc::downgrade(&layers),
            })),
        };
        s.import_all_in(&Name::new("lang"))
            .expect("required lang module not found in provided layers");
        s
    }

    ///Creates a new engine with no import (not even lang)
    pub(crate) fn empty(layers: Rc<RefCell<ModuleLayers>>) -> Self {
        Self {
            content: Rc::new(RefCell::new(ImportEngineContent {
                imported_symbols: HashMap::new(),
                layers: Rc::downgrade(&layers),
            })),
        }
    }

    ///Creates a new engine with lang pre-imported
    ///This function panics if the given imports are invalid.
    pub(crate) fn with_imports_unchecked<const N: usize>(
        imports: [&str; N],
        layers: Rc<RefCell<ModuleLayers>>,
    ) -> Self {
        let mut s = Self::new(layers);
        for fqn in imports {
            let symbol_fqn = Name::new(fqn);
            s.import(&symbol_fqn).expect("unchecked");
        }
        s
    }

    ///See [[ImportEngineContent::import]]
    pub fn import(&mut self, fqn: &Name) -> Result<&mut Self, String> {
        self.content.borrow_mut().import(fqn)?;
        Ok(self)
    }

    ///See [[ImportEngineContent::import_aliased]]
    pub fn import_aliased(&mut self, fqn: &Name, alias: &str) -> Result<&mut Self, String> {
        self.content.borrow_mut().import_aliased(fqn, alias)?;
        Ok(self)
    }

    ///See [[ImportEngineContent::import_all_in]]
    pub fn import_all_in(&mut self, fqn: &Name) -> Result<&mut Self, String> {
        self.content.borrow_mut().import_all_in(fqn)?;
        Ok(self)
    }

    ///See [[ImportEngineContent::use_symbol]]
    pub fn use_element<V, E: ContextExports<V>>(&mut self, name: &Name) -> Option<V> {
        self.content.borrow_mut().use_symbol::<V, E>(name)
    }

    ///See [[ImportEngineContent::list_unused]]
    pub fn list_unused(&self) -> Vec<UnusedSymbol> {
        self.content.borrow().list_unused()
    }
}

impl ImportEngineContent {
    ///Lists all unused symbols imported in the engine.
    /// All symbols from the lang environment are filtered out.
    fn list_unused(&self) -> Vec<UnusedSymbol> {
        self.imported_symbols
            .iter()
            .filter(|(_, import)| !import.used && import.env_fqn != Name::new("lang"))
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
        self.import_aliased(fqn, fqn.name())
    }

    ///Imports the provided symbol with an alias.
    /// Can fail if the symbols or its environment is not found.
    /// This import will shadow any alias of the same symbol if it was previously imported
    /// Accessing the symbol with its original name will not work if not explicitly or implicitly re-imported
    fn import_aliased(&mut self, fqn: &Name, alias: &str) -> Result<(), String> {
        let layers = self.layers.upgrade().expect("used layers got cleaned up");
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
        }

        self.imported_symbols.insert(
            alias.to_string(),
            ImportedSymbol {
                symbol_name: inner_name.unwrap_or(env_fqn.clone()),
                env_fqn,
                used: false,
                explicitly_imported: true,
            },
        );
        Ok(())
    }

    //remove all the symbols childrens of given symbol fqn
    fn remove_all_in(&mut self, fqn: &Name) {
        self.imported_symbols = self.imported_symbols
            .drain()
            .filter(|(_, v)| {
                let import_fqn = v.env_fqn.appended(v.symbol_name.clone());
                fqn == &import_fqn
            })
            .collect()
    }

    ///Imports all direct childs of the symbol fqn.
    /// The imported symbols will then be considered as implicitely imported.
    fn import_all_in(&mut self, fqn: &Name) -> Result<(), String> {
        let layers = self.layers.upgrade().expect("used layers got cleaned up");
        let env = layers
            .borrow()
            .get_env_of(fqn)
            .ok_or_else(|| format!("unknown module {fqn}"))?;
        let env_fqn = env.borrow().fqn.clone();
        let inner_name = fqn.relative_to(&env_fqn);

        self.remove_all_in(fqn);

        for symbol_name in env.borrow().list_exported_names(inner_name) {
            self.imported_symbols.insert(
                symbol_name.name().to_string(),
                ImportedSymbol {
                    symbol_name,
                    env_fqn: env_fqn.clone(),
                    used: false,
                    explicitly_imported: false,
                },
            );
        }
        Ok(())
    }

    ///Lookups a symbol of type `V` from given context `E` and marks the imported symbol as used if it was found.
    fn use_symbol<V, E: ContextExports<V>>(&mut self, name: &Name) -> Option<V> {
        let layers = match self.layers.upgrade() {
            None => return None,
            Some(layers) => layers,
        };
        let layers = layers.borrow();

        match self.imported_symbols.get_mut(name.root()) {
            None => None,
            Some(import) => {
                let env = layers.get_env_of(&import.env_fqn).unwrap_or_else(|| {
                    panic!(
                        "symbol {} maps to an unknown module {} but is imported",
                        name, import.env_fqn
                    )
                });

                let ctx = E::from_env(env);
                let ctx = ctx.borrow();

                let mut name = name.clone();

                if name.path().is_empty() {
                    name = name.with_name(import.symbol_name.name());
                }
                let result = ctx.find_exported(&name.tail().unwrap_or(name));
                if result.is_some() {
                    import.used = true;
                }
                result
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::import_engine::UnusedSymbol;
    use crate::layers::ModuleLayers;
    use crate::name::Name;
    use crate::types::class::ClassTypeDefinition;
    use crate::types::context::TypeContext;

    #[test]
    fn specific_imports() {
        let layers = ModuleLayers::new();

        let foo_env =
            ModuleLayers::declare_env(layers.clone(), &Name::new("bar::foo")).expect("error");

        let foo_tcx = foo_env.borrow().type_context.clone();

        let test_env =
            ModuleLayers::declare_env(layers.clone(), &Name::new("my_module")).expect("error");

        let test_tcx = test_env.borrow().type_context.clone();

        let a =
            TypeContext::define_class(foo_tcx.clone(), ClassTypeDefinition::new(Name::new("A")))
                .expect("error");

        let b =
            TypeContext::define_class(foo_tcx.clone(), ClassTypeDefinition::new(Name::new("B")))
                .expect("error");

        TypeContext::define_class(
            foo_tcx.clone(),
            ClassTypeDefinition::new(Name::new("Unused1")),
        )
        .expect("error");

        TypeContext::define_class(
            foo_tcx.clone(),
            ClassTypeDefinition::new(Name::new("Unused2")),
        )
        .expect("error");

        let mut test_env = test_env.borrow_mut();
        let mut test_ctx = test_tcx.borrow_mut();
        test_env
            .imports
            .import(&Name::new("bar::foo::A"))
            .expect("error");
        test_env
            .imports
            .import_aliased(&Name::new("bar::foo::B"), "AliasedB")
            .expect("error");
        test_env
            .imports
            .import_aliased(&Name::new("bar::foo"), "foo_alias")
            .expect("error");

        assert_eq!(test_ctx.use_class("A").expect("error"), a);
        assert_eq!(test_ctx.use_class("foo_alias::A").expect("error"), a);
        assert_eq!(test_ctx.use_class("foo_alias::B").expect("error"), b);
        assert_eq!(
            test_ctx.use_class("foo::A"),
            Err("Unknown type foo::A".to_string())
        );
        assert_eq!(
            test_ctx.use_class("foo_alias::AliasedB"),
            Err("Unknown type foo_alias::AliasedB".to_string())
        );
        assert_eq!(test_ctx.use_class("B"), Err("Unknown type B".to_string()));

        assert_eq!(test_ctx.use_class("AliasedB").expect("error"), b);

        //import all in a module should mask previous aliases in the same module
        test_env
            .imports
            .import_all_in(&Name::new("bar::foo"))
            .expect("error");
        test_env
            .imports
            .import(&Name::new("bar::foo::Unused2"))
            .expect("error");

        let mut unused_symbols = test_env.imports.list_unused();
        unused_symbols.sort_by_key(|import| import.symbol_fqn.clone());
        assert_eq!(
            unused_symbols,
            vec![
                UnusedSymbol {
                    explicitly_imported: false,
                    symbol_fqn: Name::new("bar::foo::A")
                },
                UnusedSymbol {
                    explicitly_imported: false,
                    symbol_fqn: Name::new("bar::foo::B")
                },
                UnusedSymbol {
                    explicitly_imported: false,
                    symbol_fqn: Name::new("bar::foo::Unused1")
                },
                UnusedSymbol {
                    explicitly_imported: true,
                    symbol_fqn: Name::new("bar::foo::Unused2")
                }
            ]
        );

        assert_eq!(
            test_ctx.use_class("AliasedB"),
            Err("Unknown type AliasedB".to_string())
        );
    }
}
