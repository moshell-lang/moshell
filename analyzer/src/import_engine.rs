use std::cell::RefCell;
use std::collections::{HashMap};
use std::collections::hash_map::Entry;
use std::rc::{Rc, Weak};
use crate::environment::EnvironmentContext;
use crate::name::Name;
use crate::layers::ModuleLayers;

#[derive(Default, Debug, Clone)]
pub struct ImportEngine {
    tree: ImportTree,
    layers: Weak<RefCell<ModuleLayers>>,
}

#[derive(Default, Debug, Clone)]
struct ImportTree {
    roots: HashMap<String, ImportNode>,
}

impl ImportTree {
    fn add(&mut self, child: Name, kind: SymbolImportKind) -> Result<(), String> {
        match self.roots.entry(child.root().to_string()) {
            Entry::Occupied(mut o) => {
                let tail = child.tail().unwrap_or(child.clone());
                o.get_mut().add(tail, kind)
            }
            Entry::Vacant(v) => {
                v.insert(ImportNode {
                    fqn: child,
                    import: Some(kind),
                    childs: HashMap::new(),
                    used: false,
                });
                Ok(())
            }
        }
    }
}

#[derive(Default, Debug, Clone)]
struct ImportNode {
    fqn: Name,
    import: Option<SymbolImportKind>,
    childs: HashMap<String, ImportNode>,
    used: bool,
}


impl ImportNode {
    fn add(&mut self, mut child: Name, kind: SymbolImportKind) -> Result<(), String> {
        let self_tail_parts = self.fqn.tail().unwrap_or(self.fqn.clone()).parts();
        let common_path: Vec<_> = self_tail_parts
            .clone()
            .into_iter()
            .zip(child.parts())
            .take_while(|(a, b)| a == b)
            .map(|(a, _)| a)
            .collect();
        let common_path_count = common_path.len();

        if common_path_count != self_tail_parts.len() {
            //redistribute branch nodes
            let uncommon_path_self: Vec<_> = self_tail_parts.into_iter().skip(common_path_count).collect();
            let current_node = Self {
                fqn: Name::from(uncommon_path_self),
                import: self.import.clone(),
                used: self.used,
                childs: self.childs.clone(),
            };
            let new_fqn_parts: Vec<_> = vec![self.fqn.root().to_string()].into_iter().chain(common_path).collect();
            self.fqn = Name::from(new_fqn_parts);
            self.import = None;
            self.childs = HashMap::from([(current_node.fqn.root().to_string(), current_node)]);

            if common_path_count != 0 {
                let uncommon_path_child: Vec<_> = child.parts().into_iter().skip(common_path_count).collect();
                child = Name::from(uncommon_path_child)
            }
        }


        match self.childs.entry(child.root().to_string()) {
            Entry::Occupied(mut o) => {
                let tail = child.tail().unwrap_or(child.clone());
                o.get_mut().add(tail, kind)
            },
            Entry::Vacant(v) => {
                v.insert(ImportNode {
                    fqn: child,
                    import: Some(kind),
                    childs: HashMap::new(),
                    used: false,
                });
                Ok(())
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum SymbolImportKind {
    Symbol,
    AllChildren,
    AliasOf(String),
}

impl ImportEngine {
    pub fn new(layers: Rc<RefCell<ModuleLayers>>) -> Self {
        let mut s = Self {
            tree: ImportTree::default(),
            layers: Rc::downgrade(&layers),
        };
        s.import(Name::new("lang"), SymbolImportKind::AllChildren)
            .expect("lang import in empty engine cannot fail");
        s
    }

    pub(crate) fn with_imports_unchecked<const N: usize>(imports: [(&str, SymbolImportKind); N], layers: Rc<RefCell<ModuleLayers>>) -> Self {
        let mut s = Self::new(layers);
        for (fqn, kind) in imports {
            let symbol_fqn = Name::new(fqn);
            s.tree.add(symbol_fqn, kind).expect("unchecked");
        }
        s
    }

    pub fn import(&mut self, fqn: Name, kind: SymbolImportKind) -> Result<&mut Self, String> {
        self.tree.add(fqn, kind)?;
        Ok(self)
    }

    pub fn lookup_element<V, E: EnvironmentContext<V>>(&self, _name: &Name) -> Option<V> {
        /*let layers = match self.layers.upgrade() {
            None => return None,
            Some(layers) => layers
        };

        self.tree

        for import in &self.imports {
            let env = layers
                .borrow()
                .get_env(<ModuleImport as Import<V, E>>::module(&import))
                //this is an internal error that should not occur thanks to the validity
                // checks of the imports in `add_import`
                .unwrap_or_else(|| panic!("import contains an unknown module {}", <ModuleImport as Import<V, E>>::module(&import)));

            let ctx = E::from_env(env.borrow().deref());
            let value = import.find_element(ctx.borrow().deref(), name);
            if value.is_none() {
                continue
            }
            return value
        }

        None*/
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use crate::import_engine::{ImportTree, SymbolImportKind};
    use crate::name::Name;

    #[test]
    fn tree_insertion() {
        let mut tree = ImportTree::default();
        tree.add(Name::new("a::b::c::d"), SymbolImportKind::Symbol).expect("error");
        tree.add(Name::new("a::b::c::e"), SymbolImportKind::Symbol).expect("error");
        tree.add(Name::new("a::x::y::z"), SymbolImportKind::Symbol).expect("error");

        print!("");
    }
}