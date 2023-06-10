use crate::relations::SourceObjectId;
use crate::types::builtin::lang;
use crate::types::hir::{TypeId, TypedExpr};
use crate::types::ty::{MethodType, Parameter, TypeDescription};
use crate::types::NOTHING;

/// A typed [`crate::engine::Engine`].
///
/// This engine is used to store individual chunks of typed code, such as
/// functions and scripts.
#[derive(Debug)]
pub struct TypedEngine {
    entries: Vec<Option<Chunk>>,
    descriptions: Vec<TypeDescription>,
}

/// A chunk of typed code.
#[derive(Debug)]
pub struct Chunk {
    /// The expression that is evaluated when the chunk is called.
    pub expression: TypedExpr,

    /// The input parameters of the chunk.
    ///
    /// If the chunk is a script, there are no parameters.
    pub parameters: Vec<Parameter>,

    /// The return type of the chunk.
    pub return_type: TypeId,
}

impl Chunk {
    /// Creates a new script chunk.
    pub fn script(expression: TypedExpr) -> Self {
        Self {
            expression,
            parameters: Vec::new(),
            return_type: NOTHING,
        }
    }

    /// Creates a new function chunk.
    pub fn function(
        expression: TypedExpr,
        parameters: Vec<Parameter>,
        return_type: TypeId,
    ) -> Self {
        Self {
            expression,
            parameters,
            return_type,
        }
    }
}

impl TypedEngine {
    /// Initializes a new typed engine with the given capacity.
    ///
    /// In most cases, the capacity is equal to the number of source objects in
    /// the source engine.
    pub fn with_lang(capacity: usize) -> Self {
        let mut builder = Self {
            entries: Vec::new(),
            descriptions: Vec::new(),
        };
        builder.entries.resize_with(capacity, || None);
        builder
            .descriptions
            .resize_with(10 /* Just a random number for now */, Default::default);
        lang(&mut builder);
        builder
    }

    /// Returns the chunk with the given id.
    pub fn get(&self, id: SourceObjectId) -> Option<&Chunk> {
        self.entries.get(id.0).and_then(|entry| entry.as_ref())
    }

    /// Inserts a chunk into the engine.
    pub fn insert(&mut self, id: SourceObjectId, entry: Chunk) {
        self.entries[id.0] = Some(entry);
    }

    pub fn find_method(&self, type_id: TypeId, name: &str) -> Option<&MethodType> {
        self.descriptions[type_id.0].methods.get(name)
    }

    pub fn add_method(&mut self, type_id: TypeId, name: &str, method: MethodType) {
        self.descriptions[type_id.0]
            .methods
            .insert(name.to_owned(), method);
    }

    /// Inserts a chunk into the engine if it is not already present.
    ///
    /// This may be used to insert semi-accurate chunks into the engine.
    pub fn insert_if_absent(&mut self, id: SourceObjectId, entry: Chunk) {
        if self.entries[id.0].is_none() {
            self.entries[id.0] = Some(entry);
        }
    }
}
