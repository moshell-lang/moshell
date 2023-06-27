use crate::relations::{LocalId, Relations, SourceId, Symbol};
use crate::types::hir::TypeId;
use crate::types::{BOOL, EXIT_CODE, FLOAT, INT, NOTHING, STRING};
use std::collections::HashMap;

/// Holds the symbol to type mapping.
///
/// The actual type definition is in the [`crate::types::Typing`] struct.
pub struct TypeContext {
    names: HashMap<String, TypeId>,
    locals: HashMap<SourceId, Vec<TypedVariable>>,
}

impl TypeContext {
    /// Constructs a new typing context that already contains the built-in type names.
    pub(crate) fn with_lang() -> Self {
        Self {
            names: HashMap::from([
                ("Nothing".to_owned(), NOTHING),
                ("Bool".to_owned(), BOOL),
                ("Exitcode".to_owned(), EXIT_CODE),
                ("Int".to_owned(), INT),
                ("Float".to_owned(), FLOAT),
                ("String".to_owned(), STRING),
            ]),
            locals: HashMap::new(),
        }
    }

    /// Returns the type id of a symbol.
    pub(crate) fn get(
        &self,
        relations: &Relations,
        source: SourceId,
        symbol: Symbol,
    ) -> Option<TypedVariable> {
        match symbol {
            Symbol::Local(index) => self.locals.get(&source).unwrap().get(index.0).copied(),
            Symbol::External(index) => {
                let resolved = relations[index].state.expect_resolved("Unresolved symbol");
                self.locals
                    .get(&resolved.source)
                    .unwrap()
                    .get(resolved.object_id.0)
                    .copied()
            }
        }
    }

    /// Defines the type of a currently explored symbol.
    ///
    /// This must be in sync with the symbol in the environment.
    pub(crate) fn push_local_type(&mut self, source: SourceId, type_id: TypeId) -> LocalId {
        self.push_local(source, TypedVariable::immutable(type_id))
    }

    /// Defines the identity of a currently explored symbol.
    ///
    /// This must be in sync with the symbol in the environment.
    pub(crate) fn push_local(&mut self, source: SourceId, obj: TypedVariable) -> LocalId {
        let locals = self.locals.entry(source).or_default();
        let index = locals.len();
        locals.push(obj);
        LocalId(index)
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

/// The identity of a variable.
///
/// The main purpose of this struct is to hold the type of a variable,
/// but it also holds if the variable can be reassigned.
#[derive(Debug, Clone, Copy)]
pub(crate) struct TypedVariable {
    pub(crate) type_id: TypeId,
    pub(crate) can_reassign: bool,
}

impl TypedVariable {
    /// Constructs a new mutable variable identity.
    pub(crate) fn assignable(type_id: TypeId) -> Self {
        Self {
            type_id,
            can_reassign: true,
        }
    }

    /// Constructs a new immutable variable identity.
    pub(crate) fn immutable(type_id: TypeId) -> Self {
        Self {
            type_id,
            can_reassign: false,
        }
    }
}
