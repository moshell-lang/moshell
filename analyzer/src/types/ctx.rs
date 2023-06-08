use crate::relations::{Relations, SourceObjectId, Symbol};
use crate::types::hir::TypeId;
use crate::types::{BOOL, FLOAT, INT, NOTHING, STRING};
use std::collections::HashMap;

/// Holds the symbol to type mapping.
///
/// The actual type definition is in the [`crate::types::Typing`] struct.
pub struct TypeContext {
    pub(crate) source: SourceObjectId,
    names: HashMap<String, TypeId>,
    locals: HashMap<SourceObjectId, Vec<TypeId>>,
}

impl TypeContext {
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
            locals: HashMap::new(),
        }
    }

    /// Updates the context to prepare for a new source object.
    pub(crate) fn prepare(&mut self, source: SourceObjectId) {
        self.source = source;
        self.locals.insert(source, Vec::new());
    }

    /// Returns the type id of a symbol.
    pub(crate) fn get(&self, relations: &Relations, symbol: Symbol) -> Option<TypeId> {
        match symbol {
            Symbol::Local(index) => self.locals.get(&self.source).unwrap().get(index).copied(),
            Symbol::Global(index) => {
                let resolved = relations.objects[index]
                    .resolved
                    .expect("Unresolved symbol");
                self.locals
                    .get(&resolved.module)
                    .unwrap()
                    .get(resolved.object_id)
                    .copied()
            }
        }
    }

    /// Defines the type of a currently explored symbol.
    ///
    /// This must be in sync with the symbol in the environment.
    pub(crate) fn push_local_type(&mut self, type_id: TypeId) {
        self.locals.get_mut(&self.source).unwrap().push(type_id);
    }

    /// Finds the type from an annotation.
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
}
