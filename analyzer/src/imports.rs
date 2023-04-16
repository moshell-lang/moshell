use std::collections::{HashMap, HashSet};
use crate::environment::EnvironmentContext;
use crate::identity::Name;

#[derive(Debug, Clone, PartialEq)]
pub struct SpecificImports {
    module: Name,
    imported_classes: HashSet<String>,
    aliased_classes: HashMap<String, String>,
}

impl SpecificImports {
    pub fn new(module: Name,
               allowed_classes: HashSet<&str>,
               aliased_classes: HashMap<&str, &str>) -> Self {
        SpecificImports {
            module,
            imported_classes: allowed_classes.into_iter().map(str::to_owned).collect(),
            aliased_classes: aliased_classes.into_iter().map(|(k, v)| (k.to_owned(), v.to_owned())).collect(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AllImports {
    module: Name,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ModuleImport {
    Specifics(SpecificImports),
    All(AllImports)
}

impl ModuleImport {
    pub fn all(module: Name) -> Self {
        Self::All(AllImports {
            module
        })
    }

    pub fn specifics(module: Name,
                     allowed_classes: HashSet<&str>,
                     aliases: HashMap<&str, &str>) -> Self {
        Self::Specifics(SpecificImports::new(module, allowed_classes, aliases))
    }
}


pub trait Import<V, E: EnvironmentContext<V>> {
    fn module(&self) -> Name;

    fn find_element(&self, ctx: &E, name: &Name) -> Option<V>;
}

impl<V, E: EnvironmentContext<V>> Import<V, E> for ModuleImport {
    fn module(&self) -> Name {
        match self {
            ModuleImport::Specifics(s) => <SpecificImports as Import<V, E>>::module(s),
            ModuleImport::All(a) => <AllImports as Import<V, E>>::module(a)
        }
    }
    fn find_element(&self, ctx: &E, name: &Name) -> Option<V> {
        match self {
            ModuleImport::Specifics(s) => s.find_element(ctx, name),
            ModuleImport::All(a) => a.find_element(ctx, name)
        }
    }
}

impl<V, E: EnvironmentContext<V>> Import<V, E> for SpecificImports {
    fn module(&self) -> Name {
        self.module.clone()
    }

    fn find_element(&self, ctx: &E, name: &Name) -> Option<V> {
        if self.imported_classes.contains(name.root()) {
            return ctx.find(name)
        }

        if let Some(root_unaliased) = self.aliased_classes.get(name.root()) {
            let mut parts = name.parts();
            parts[0] = root_unaliased.clone(); //cannot be empty

            return ctx.find(&Name::from(parts))
        }
        None
    }
}

impl<V, E: EnvironmentContext<V>> Import<V, E> for AllImports {
    fn module(&self) -> Name {
        self.module.clone()
    }

    fn find_element(&self, ctx: &E, name: &Name) -> Option<V> {
        ctx.find(name)
    }
}