use std::cell::RefCell;
use std::collections::{HashMap};
use std::ops::Deref;
use std::rc::{Rc, Weak};
use crate::environment::EnvironmentContext;
use crate::name::Name;
use crate::layers::{ModuleLayers};

#[derive(Debug, Clone)]
pub struct ImportEngine {
    imported_symbols: HashMap<String, ImportedSymbol>,
    layers: Weak<RefCell<ModuleLayers>>,
}

#[derive(Debug, Clone)]
struct ImportedSymbol {
    env_fqn: Name,
    symbol_name: Name,
    used: bool,
    explicitly_imported: bool
}

#[derive(Debug, PartialEq, Eq)]
pub struct UnusedSymbol {
    pub symbol_fqn: Name,
    pub explicitly_imported: bool
}

impl ImportEngine {
    pub fn new<V, E: EnvironmentContext<V>>(layers: Rc<RefCell<ModuleLayers>>) -> Self {
        let mut s = Self {
            imported_symbols: HashMap::default(),
            layers: Rc::downgrade(&layers),
        };
        s.import_all_in::<V, E>(Name::new("lang")).expect("required lang module");
        s
    }

    pub(crate) fn empty(layers: Rc<RefCell<ModuleLayers>>) -> Self {
        Self {
            imported_symbols: HashMap::new(),
            layers: Rc::downgrade(&layers)
        }
    }

    pub(crate) fn with_imports_unchecked<V, E: EnvironmentContext<V>, const N: usize>(imports: [&str; N], layers: Rc<RefCell<ModuleLayers>>) -> Self {
        let mut s = Self::new::<V, E>(layers);
        for fqn in imports {
            let symbol_fqn = Name::new(fqn);
            s.import::<V, E>(symbol_fqn).expect("unchecked");
        }
        s
    }

    pub fn list_unused(&self) -> Vec<UnusedSymbol> {
        self.imported_symbols
            .iter()
            .filter(|(_, import)| !import.used && import.env_fqn != Name::new("lang"))
            .map(|(alias, import)| {
                //merge env and symbol names
                let mut parts = import.env_fqn.parts();
                parts.append(&mut import.symbol_name.parts());
                let symbol_fqn = Name::from(parts).with_name(alias);
                UnusedSymbol {
                    symbol_fqn,
                    explicitly_imported: import.explicitly_imported
                }
            })
            .collect()
    }

    pub fn import<V, E: EnvironmentContext<V>>(&mut self, fqn: Name) -> Result<&mut Self, String> {
        self.import_aliased::<V, E>(fqn.clone(), &fqn.name)
    }


    pub fn import_aliased<V, E: EnvironmentContext<V>>(&mut self, fqn: Name, alias: &str) -> Result<&mut Self, String> {
        let layers = self.layers.upgrade().expect("used layers got cleaned up");
        let env = layers.borrow().get_env_of(&fqn).ok_or(format!("unknown module {}", fqn))?;
        let env_fqn = env.borrow().identity.clone();
        let inner_name = fqn.relative_to(&env_fqn);

        if let Some(inner_name) = &inner_name {
            let ctx = E::from_env(env.borrow().deref());
            if ctx.borrow().find_exported(inner_name).is_none() {
                return Err(format!("symbol {} in module {} not found.", inner_name, env_fqn))
            }
        }

        self.imported_symbols.insert(alias.to_string(), ImportedSymbol {
            symbol_name: inner_name.unwrap_or(env_fqn.clone()),
            env_fqn,
            used: false,
            explicitly_imported: true,
        });
        Ok(self)
    }

    pub fn remove_all_in(&mut self, fqn: Name) {
        self.imported_symbols = HashMap::from_iter(self.imported_symbols
            .iter()
            .filter(|(_, v)| {
                let mut parts = v.env_fqn.parts();
                parts.append(&mut v.symbol_name.parts());
                fqn.path == parts
            })
            .map(|(k, v)| (k.clone(), v.clone())))
    }

    pub fn import_all_in<V, E: EnvironmentContext<V>>(&mut self, fqn: Name) -> Result<&mut Self, String> {
        let layers = self.layers.upgrade().expect("used layers got cleaned up");
        let env = layers.borrow().get_env_of(&fqn).ok_or(format!("unknown module {}", fqn))?;
        let env_fqn = &env.borrow().identity;
        let inner_name = fqn.relative_to(env_fqn);
        let ctx = E::from_env(env.borrow().deref());

        self.remove_all_in(fqn);

        for symbol_name in ctx.borrow().list_exported_names(inner_name) {
            self.imported_symbols.insert(symbol_name.name.clone(), ImportedSymbol {
                symbol_name,
                env_fqn: env_fqn.clone(),
                used: false,
                explicitly_imported: false,
            });
        }
        Ok(self)
    }

    pub fn use_element<V, E: EnvironmentContext<V>>(&mut self, name: &Name) -> Option<V> {
        let layers = match self.layers.upgrade() {
            None => return None,
            Some(layers) => layers
        };
        let layers = layers.borrow();

        match self.imported_symbols.get_mut(name.root()) {
            None => None,
            Some(import) => {
                let env = layers
                    .get_env_of(&import.env_fqn)
                    .expect(&format!("symbol {} maps to an unknown module {} but is imported", name, import.env_fqn));

                let ctx = E::from_env(env.borrow().deref());
                let ctx = ctx.borrow();

                let mut name = name.clone();

                if name.path.is_empty() {
                    name.name = import.symbol_name.name.clone();
                }
                let result = ctx.find_exported(&name.tail().unwrap_or(name.clone()));
                if result.is_some() {
                    import.used = true;
                }
                result
            }
        }
    }
}