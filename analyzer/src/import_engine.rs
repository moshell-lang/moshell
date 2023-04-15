use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;
use crate::environment::EnvironmentContext;
use crate::imports::{Import, ModuleImport};
use crate::module::ModuleLayers;

#[derive(Default, Debug, Clone)]
pub struct ImportEngine {
    imports: Vec<ModuleImport>,
    layers: Rc<RefCell<ModuleLayers>>,
}

impl ImportEngine {

    pub(crate) fn new(imports: Vec<ModuleImport>, layers: Rc<RefCell<ModuleLayers>>) -> Self {
        Self {
            imports,
            layers,
        }
    }

    pub fn add_import(&mut self, import: ModuleImport) {
        self.imports.push(import)
    }

    pub fn lookup_element<V, E: EnvironmentContext<V>>(&self, name: &str) -> Option<V> {
        for import in &self.imports {
            if !<ModuleImport as Import<V, E>>::is_imported(import, name) {
                continue;
            }

            let env = self.layers
                .borrow()
                .get_env(<ModuleImport as Import<V, E>>::module(&import))
                //this is an internal error that should not occur thanks to the validity
                // checks of the imports in `add_import`
                .unwrap_or_else(|| panic!("import contains an unknown module {}", <ModuleImport as Import<V, E>>::module(&import)));

            let ctx = E::from_env(env.borrow().deref());
            let value = ctx.borrow().find(name);
            return value
        }

        None
    }
}
