use crate::relations::SourceObjectId;
use crate::types::hir::TypeId;
use crate::types::{BOOL, FLOAT, INT, NOTHING, STRING};
use std::collections::HashMap;

pub struct TypeContext<'p> {
    pub(crate) source: SourceObjectId,
    names: HashMap<String, TypeId>,
    pub(crate) locals: Vec<TypeId>,
    parent: Option<&'p TypeContext<'p>>,
}

impl<'p> TypeContext<'p> {
    pub(crate) fn lang() -> Self {
        Self {
            source: SourceObjectId(0),
            names: HashMap::from([
                ("Nothing".to_owned(), NOTHING),
                ("Bool".to_owned(), BOOL),
                ("Int".to_owned(), INT),
                ("Float".to_owned(), FLOAT),
                ("String".to_owned(), STRING),
            ]),
            locals: Vec::new(),
            parent: None,
        }
    }

    pub(crate) fn resolve(&self, type_annotation: &ast::r#type::Type) -> Option<TypeId> {
        match type_annotation {
            ast::r#type::Type::Parametrized(param) => {
                if !param.path.is_empty() || !param.params.is_empty() {
                    return None;
                }
                self.names.get(param.name).copied()
            }
            ast::r#type::Type::Callable(_) => None,
            ast::r#type::Type::ByName(_) => None,
        }
    }

    fn fork(&'p self, source: SourceObjectId) -> Self {
        Self {
            source,
            names: HashMap::new(),
            locals: Vec::new(),
            parent: Some(self),
        }
    }
}
