use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use crate::environment::Environment;
use crate::identity::Identity;

#[derive(Debug, Clone, PartialEq)]
pub struct EnvSpecificImports {
    env: Rc<RefCell<Environment>>,
    imports: HashMap<String, Identity>
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnvAllImports {
    env: Rc<RefCell<Environment>>
}

#[derive(Debug, Clone, PartialEq)]
pub enum EnvImport {
    Specifics(EnvSpecificImports),
    All(EnvAllImports)
}

impl<'a> EnvImport {
    pub fn all(env: Rc<RefCell<Environment>>) -> EnvImport {
        EnvImport::All(EnvAllImports {
            env
        })
    }

    pub fn specifics(env: Rc<RefCell<Environment>>,
                     map: HashMap<String, Identity>) -> EnvImport {
        EnvImport::Specifics(EnvSpecificImports {
            env,
            imports: map
        })
    }
}

pub trait Import {
    fn find_class(&self, name: String) -> Option<Identity>;
}

impl Import for EnvAllImports {
    fn find_class(&self, name: String) -> Option<Identity> {
        self.env.borrow().type_context.borrow().lookup_name()
    }
}