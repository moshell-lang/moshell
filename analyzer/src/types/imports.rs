use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use crate::types::class::TypeClass;
use crate::types::context::TypeContext;

#[derive(Debug, Clone, PartialEq)]
pub struct SpecificCtxImports {
    ctx: Rc<RefCell<TypeContext>>,
    imported_classes: HashSet<String>,
    aliased_classes: HashMap<String, String>,
}

impl SpecificCtxImports {
    pub fn new(ctx: Rc<RefCell<TypeContext>>,
               allowed_classes: HashSet<&str>,
               aliased_classes: HashMap<&str, &str>) -> Result<SpecificCtxImports, String> {
        let mut unknown_classes = Vec::new();

        for name in aliased_classes.values().chain(&allowed_classes) {
            let ctx = ctx.borrow();
            if ctx.find_class(name).is_none() {
                unknown_classes.push(ctx.identity.to_string() + "::" + name)
            }
        }

        if let Some((head, tail)) = unknown_classes.split_first() {
            let names = tail
                .into_iter()
                .fold(head.to_string(), |acc, s| acc + ", " + s);
            return Err(format!("cannot find {}", names))
        }

        Ok(SpecificCtxImports {
            ctx,
            imported_classes: allowed_classes.into_iter().map(str::to_owned).collect(),
            aliased_classes: aliased_classes.into_iter().map(|(k, v)| (k.to_owned(), v.to_owned())).collect(),
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AllCtxImports {
    ctx: Rc<RefCell<TypeContext>>,
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

    pub fn specifics(ctx: Rc<RefCell<TypeContext>>,
                     allowed_classes: HashSet<&str>,
                     aliases: HashMap<&str, &str>) -> Result<CtxImport, String> {
        SpecificCtxImports::new(ctx, allowed_classes, aliases).map(CtxImport::Specifics)
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
        if self.imported_classes.contains(name) {
            return ctx.find_class(name)
        }

        if let Some(name_unaliased) = self.aliased_classes.get(name) {
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