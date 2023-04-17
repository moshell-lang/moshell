use std::cell::RefCell;
use std::collections::{HashMap};
use std::collections::hash_map::Entry;
use std::ops::Deref;
use std::rc::{Rc, Weak};
use crate::environment::EnvironmentContext;
use crate::import_engine::SymbolImportKind::{AliasOf, AllChildren};
use crate::name::Name;
use crate::layers::{Module, ModuleLayers};

#[derive(Default, Debug, Clone)]
pub struct ImportEngine {
    imports: ImportTree,
    layers: Weak<RefCell<ModuleLayers>>,
}

#[derive(Default, Debug, Clone)]
struct ImportTree {
    roots: HashMap<String, ImportNode>,
}

impl ImportTree {
    fn insert(&mut self, child: Name, kind: SymbolImportKind) {
        push_import(child, kind, &mut self.roots)
    }

    fn use_import<V, E: EnvironmentContext<V>>(&mut self, layers: &ModuleLayers, name: Name) -> Option<V> {
        for node in self.roots.values_mut() {
            let env = layers
                .get_module_of(node.fqn.clone())
                .expect(&format!("symbol {} links to an unknown module but is imported", node.fqn));
            let result = node.use_import::<V, E>(env, name.clone());
            if result.is_some() {
                return result
            }
        }
        None
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
    fn insert(&mut self, mut child: Name, kind: SymbolImportKind) {
        if self.fqn.name == child.name && child.path.is_empty() {
            self.import = Some(kind);
            return;
        }

        if self.fqn.path.is_empty() {
            push_import(child, kind, &mut self.childs);
            return;
        }

        let self_tail_parts = self.fqn.tail().map(|n| n.parts()).unwrap_or_default();
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
            let axis = Self {
                fqn: Name::from(uncommon_path_self),
                import: self.import.clone(),
                used: self.used,
                childs: self.childs.clone(),
            };
            let new_fqn_parts: Vec<_> = vec![self.fqn.root().to_string()].into_iter().chain(common_path).collect();
            self.fqn = Name::from(new_fqn_parts);
            self.import = None;
            self.childs = HashMap::from([(axis.fqn.root().to_string(), axis)]);
        }

        if common_path_count < child.part_count() {
            let uncommon_path_child: Vec<_> = child.parts()
                .into_iter()
                .skip(common_path_count)
                .collect();
            child = Name::from(uncommon_path_child)
        }

        push_import(child, kind, &mut self.childs)
    }

    fn use_import<V, E: EnvironmentContext<V>>(&mut self, module: &Module, name: Name) -> Option<V> {
        for node in self.childs.values_mut() {
            match node.use_import::<V, E>(module, name.clone()) {
                Some(val) => return Some(val),
                None => ()
            }
        }

        let import = match self.import.clone() {
            None => return None,
            Some(import) => import
        };

        if import != AllChildren && self.fqn.name != name.root() {
            return None
        }

        let env = module.env.clone()
            .expect(&format!("symbol {} links to an unknown module {} but is imported", self.fqn, module.full_name));
        let ctx = E::from_env(env.borrow().deref());
        let ctx = ctx.borrow();

        let name = name.tail().unwrap_or(name);

        let result = match import {
            AliasOf(aliased) => ctx.find(&name.with_name(&aliased)),
            _ => ctx.find(&name),
        };

        if result.is_some() {
            self.used = true;
        }
        result
    }
}

fn push_import(child: Name, kind: SymbolImportKind, map: &mut HashMap<String, ImportNode>) {
    match map.entry(child.root().to_string()) {
        Entry::Occupied(mut o) => {
            let tail = child.tail().unwrap_or(child.clone());
            o.get_mut().insert(tail, kind)
        },
        Entry::Vacant(v) => {
            v.insert(ImportNode {
                fqn: child,
                import: Some(kind),
                childs: HashMap::new(),
                used: false,
            });
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
        s.import(Name::new("lang"), SymbolImportKind::AllChildren);
        s
    }

    pub(crate) fn with_imports_unchecked<const N: usize>(imports: [(&str, SymbolImportKind); N], layers: Rc<RefCell<ModuleLayers>>) -> Self {
        let mut s = Self::new(layers);
        for (fqn, kind) in imports {
            let symbol_fqn = Name::new(fqn);
            s.tree.insert(symbol_fqn, kind);
        }
        s
    }

    pub fn import(&mut self, fqn: Name, kind: SymbolImportKind) -> &mut Self {
        self.tree.insert(fqn, kind);
        self
    }

    pub fn use_element<V, E: EnvironmentContext<V>>(&mut self, name: &Name) -> Option<V> {
        let layers = match self.layers.upgrade() {
            None => return None,
            Some(layers) => layers
        };
        let layers = layers.borrow();

        self.tree.use_import::<V, E>(layers.deref(), name.clone())
    }
}

#[cfg(test)]
mod tests {
    use crate::import_engine::{ImportTree, SymbolImportKind};
    use crate::name::Name;

    #[test]
    fn tree_insertion() {
        let mut tree = ImportTree::default();
        tree.insert(Name::new("a::b::c::d"), SymbolImportKind::Symbol);
        tree.insert(Name::new("a::b::c::e"), SymbolImportKind::Symbol);
        tree.insert(Name::new("a::x::y::z"), SymbolImportKind::Symbol);
        tree.insert(Name::new("a::x::y::f"), SymbolImportKind::Symbol);
        tree.insert(Name::new("a::x::y::g"), SymbolImportKind::Symbol);

        println!();
    }
}