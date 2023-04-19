use std::collections::HashMap;
use std::rc::Rc;
use crate::import_engine::FixedImportEngine;
use crate::locals::local::Local;
use crate::name::Name;

struct LocalContext {
    /// Records the type of each class by their basename.
    classes: HashMap<String, Rc<Local>>,

    ///View of the environment's engine.
    imports: FixedImportEngine,

    ///Environment's fully qualified name
    pub fqn: Name,
}