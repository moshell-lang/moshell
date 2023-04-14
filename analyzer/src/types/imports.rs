use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use crate::types::class::TypeClass;
use crate::types::context::TypeContext;

#[derive(Debug, Clone, PartialEq)]
pub struct SpecificCtxImports {
    ctx: Rc<RefCell<TypeContext>>,
    allowed_classes: HashSet<String>,
    aliases: HashMap<String, String>
}

#[derive(Debug, Clone, PartialEq)]
pub struct AllCtxImports {
    ctx: Rc<RefCell<TypeContext>>
}

#[derive(Debug, Clone, PartialEq)]
pub enum CtxImport {
    Specifics(SpecificCtxImports),
    All(AllCtxImports)
}

impl<'a> CtxImport {
    pub fn all(ctx: Rc<RefCell<TypeContext>>) -> CtxImport {
        CtxImport::All(AllCtxImports {
            ctx
        })
    }

    pub fn specifics(env: Rc<RefCell<TypeContext>>,
                     allowed_classes: HashSet<String>,
                     aliases: HashMap<String, String>) -> CtxImport {
        CtxImport::Specifics(SpecificCtxImports {
            ctx: env,
            allowed_classes,
            aliases
        })
    }
}

pub trait Import {
    fn find_class(&self, name: &str) -> Option<Rc<TypeClass>>;
}

impl Import for CtxImport {
    fn find_class(&self, name: &str) -> Option<Rc<TypeClass>> {
        match self {
            CtxImport::Specifics(s) => s.find_class(name),
            CtxImport::All(a) => a.find_class(name)
        }
    }
}

impl Import for SpecificCtxImports {
    fn find_class(&self, name: &str) -> Option<Rc<TypeClass>> {
        let ctx = self.ctx.borrow();
        if self.allowed_classes.contains(name) {
            return ctx.find_class(name)
        }

        if let Some(name_unaliased) = self.aliases.get(name) {
            return ctx.find_class(name_unaliased)
        }
        None
    }
}

impl Import for AllCtxImports {
    fn find_class(&self, name: &str) -> Option<Rc<TypeClass>> {
        self.ctx.borrow().find_class(name)
    }
}