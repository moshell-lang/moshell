use std::rc::Rc;
use crate::environment::EnvironmentContext;
use crate::imports::{Import, ModuleImport};
use crate::module::Roots;

#[derive(Default, Debug, PartialEq, Clone)]
pub struct ImportEngine {
    imports: Vec<ModuleImport>,
    roots: Rc<Roots>
}

impl ImportEngine {

    pub fn new(imports: Vec<ModuleImport>, roots: Rc<Roots>) -> Self {
        Self {
            imports,
            roots
        }
    }

    pub fn add_import(&mut self, import: ModuleImport) {
        self.imports.push(import)
    }

    pub fn lookup_element<V, E: EnvironmentContext<V>>(&self, name: &str) -> Option<V> {
        for import in &self.imports {
            match import.find_element(context, name) {
                s @ Some(_) => return s,
                None => ()
            }
        }
        None
    }
}
