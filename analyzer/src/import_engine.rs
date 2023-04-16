use std::cell::RefCell;
use std::collections::linked_list::LinkedList;
use std::ops::Deref;
use std::rc::{Rc, Weak};
use crate::environment::EnvironmentContext;
use crate::identity::Name;
use crate::imports::{Import, ModuleImport};
use crate::layers::ModuleLayers;

#[derive(Default, Debug, Clone)]
pub struct ImportEngine {
    imports: LinkedList<ModuleImport>,
    layers: Weak<RefCell<ModuleLayers>>,
}

impl ImportEngine {

    pub fn new(layers: Rc<RefCell<ModuleLayers>>) -> Self {
        let mut imports = LinkedList::new();
        imports.push_back(ModuleImport::all(Name::new("lang")));
        Self {
            imports,
            layers: Rc::downgrade(&layers),
        }
    }

    pub(crate) fn with_imports_unchecked<const N: usize>(imports: [ModuleImport; N], layers: Rc<RefCell<ModuleLayers>>) -> Self {
        let mut imports = LinkedList::from(imports);
        imports.push_back(ModuleImport::all(Name::new("lang")));
        Self {
            imports,
            layers: Rc::downgrade(&layers),
        }
    }

    pub fn add_import(&mut self, import: ModuleImport) {
        self.imports.push_front(import)
    }

    pub fn lookup_element<V, E: EnvironmentContext<V>>(&self, name: &Name) -> Option<V> {
        let layers = match self.layers.upgrade() {
            None => return None,
            Some(layers) => layers
        };

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

        None
    }
}
