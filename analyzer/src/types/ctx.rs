use crate::relations::{ObjectId, Relations, SourceObjectId, Symbol};
use crate::types::hir::TypeId;
use crate::types::{BOOL, FLOAT, INT, NOTHING, STRING};
use std::collections::HashMap;

/// Holds the symbol to type mapping.
///
/// The actual type definition is in the [`crate::types::Typing`] struct.
pub struct TypeContext {
    names: HashMap<String, TypeId>,
    locals: HashMap<SourceObjectId, Vec<TypeId>>,
}

impl TypeContext {
    pub(crate) fn new() -> Self {
        Self {
            names: HashMap::new(),
            locals: HashMap::new(),
        }
    }

    pub(crate) fn lang() -> Self {
        let mut ctx = Self::new();
        ctx.fill_lang();
        ctx
    }

    pub(crate) fn fill_lang(&mut self) {
        self.names.extend([
            ("Nothing".to_owned(), NOTHING),
            ("Bool".to_owned(), BOOL),
            ("Int".to_owned(), INT),
            ("Float".to_owned(), FLOAT),
            ("String".to_owned(), STRING),
        ]);
    }

    /// Returns the type id of a symbol.
    pub(crate) fn get(
        &self,
        relations: &Relations,
        source: SourceObjectId,
        symbol: Symbol,
    ) -> Option<TypeId> {
        match symbol {
            Symbol::Local(index) => self.locals.get(&source).unwrap().get(index).copied(),
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
    pub(crate) fn push_local_type(&mut self, source: SourceObjectId, type_id: TypeId) -> ObjectId {
        let locals = self.locals.entry(source).or_default();
        let index = locals.len();
        locals.push(type_id);
        index
    }

    /// Finds the type from an annotation.
    pub(crate) fn resolve(&self, type_annotation: &ast::r#type::Type) -> Option<TypeId> {
        match type_annotation {
            ast::r#type::Type::Parametrized(param) => {
                if !param.path.is_empty() || !param.params.is_empty() {
                    unimplemented!();
                }
                self.names.get(param.name).copied()
            }
            ast::r#type::Type::Callable(_) => unimplemented!(),
            ast::r#type::Type::ByName(_) => unimplemented!(),
        }
    }
}
