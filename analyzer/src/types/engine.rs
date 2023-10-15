use context::source::ContentId;

use crate::engine::Engine;
use crate::environment::Environment;
use crate::relations::{ObjectId, SourceId};
use crate::types::hir::TypedExpr;
use crate::types::ty::{Field, FunctionDesc, MethodType, StructureDesc, Type, TypeId, TypeRef};

/// A typed [`Engine`].
///
/// This engine is used to store individual chunks of typed code, such as
/// functions and scripts.
#[derive(Debug, Default)]
pub struct TypedEngine {
    /// The user defined chunks of code.
    ///
    /// At the end of the compilation, this vector has replaced all its `None` values.
    entries: Vec<Option<Chunk>>,

    /// All functions definitions. Indexed by a [`FunctionId`] identifier.
    functions: Vec<FunctionDesc>,

    /// All structures definitions. Indexed by a [`StructureId`] identifier.
    structures: Vec<StructureDesc>,
}

/// A function identifier, that points to a [`FunctionDesc`] inside a [`TypedEngine`]
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct FunctionId(pub ObjectId);

/// A structure identifier, that points to a [`StructureDesc`] inside a [`TypedEngine`]
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct StructureId(pub ObjectId);

impl TypedEngine {
    /// Initializes a new typed engine with the given capacity.
    ///
    /// In most cases, the capacity is equal to the number of source objects in
    /// the source engine.
    pub fn new(capacity: usize) -> Self {
        let mut engine = Self {
            entries: Vec::new(),
            functions: Vec::new(),
            structures: Vec::new(),
        };
        engine.entries.resize_with(capacity, || None);
        engine
    }

    pub fn init_empty_structure(&mut self) -> StructureId {
        let id = StructureId(self.structures.len());
        self.structures.push(StructureDesc::default());
        id
    }

    pub(crate) fn get_structure_mut(&mut self, id: StructureId) -> Option<&mut StructureDesc> {
        self.structures.get_mut(id.0)
    }

    pub fn get_structure(&self, id: StructureId) -> Option<&StructureDesc> {
        self.structures.get(id.0)
    }

    pub fn iter_structures(&self) -> impl Iterator<Item = &StructureDesc> {
        self.structures.iter()
    }

    pub fn get_function(&self, id: FunctionId) -> Option<&FunctionDesc> {
        self.functions.get(id.0)
    }

    pub(crate) fn get_function_mut(&mut self, id: FunctionId) -> Option<&mut FunctionDesc> {
        self.functions.get_mut(id.0)
    }

    /// Returns the chunk with the given source id.
    ///
    /// If the chunk is not a user defined chunk, [`None`] is returned.
    /// Use [`Self::get`] to get both user defined and native chunks.
    pub fn get_user(&self, id: SourceId) -> Option<&Chunk> {
        self.entries.get(id.0)?.as_ref()
    }

    pub fn take_user(&mut self, id: SourceId) -> Option<Chunk> {
        self.entries.get_mut(id.0)?.take()
    }

    /// Inserts a chunk into the engine.
    pub fn insert(&mut self, id: SourceId, entry: Chunk) {
        self.entries[id.0] = Some(entry);
    }

    /// Lists methods with a given name of a given type.
    ///
    /// If the type is unknown or doesn't have any methods with the given name,
    /// [`None`] is returned.
    pub fn get_methods(&self, structure_id: StructureId, name: &str) -> Option<&Vec<FunctionId>> {
        self.structures.get(structure_id.0)?.methods.get(name)
    }

    /// Gets the method that matches exactly the given arguments and return type.
    pub fn get_method_exact(
        &self,
        structure_id: StructureId,
        name: &str,
        args: &[TypeRef],
        return_type: TypeRef,
    ) -> Option<(&MethodType, FunctionId)> {
        self.get_methods(structure_id, name).and_then(|methods| {
            methods
                .iter()
                .find(|function_id| {
                    let method = &self.functions[function_id.0];
                    method.return_type == return_type
                        && method.parameters.iter().map(|p| &p.ty).eq(args)
                })
                .map(|function_id| (&self.functions[function_id.0], *function_id))
        })
    }

    /// Adds a new method to a type.
    ///
    /// The method may not conflict with any existing methods.
    pub fn add_method(
        &mut self,
        struct_id: StructureId,
        name: &str,
        method: MethodType,
    ) -> FunctionId {
        let function_id = self.add_function(method);

        self.structures
            .get_mut(struct_id.0)
            .expect("structure not initialized")
            .methods
            .entry(name.to_owned())
            .or_default()
            .push(function_id);

        function_id
    }

    pub fn add_function(&mut self, function: FunctionDesc) -> FunctionId {
        let function_id = FunctionId(self.functions.len());
        self.functions.push(function);
        function_id
    }

    /// Adds a new generic type parameter to a structure.
    pub(crate) fn add_generic(&mut self, struct_id: StructureId, generic: TypeId) {
        self.structures
            .get_mut(struct_id.0)
            .expect("structure not initialized")
            .type_parameters
            .push(generic);
    }

    pub(crate) fn bind_field(&mut self, struct_id: StructureId, name: String, field: Field) {
        self.structures
            .get_mut(struct_id.0)
            .expect("structure not initialized")
            .fields
            .insert(name, field);
    }

    /// returns an iterator over all contained chunks with their identifier
    pub fn iter_chunks(&self) -> impl Iterator<Item = (SourceId, &Chunk)> {
        self.entries
            .iter()
            .enumerate()
            .filter_map(|(id, chunk)| chunk.as_ref().map(|chunk| (SourceId(id), chunk)))
    }

    pub fn len(&self) -> usize {
        self.entries.len()
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    /// Returns an iterator over all contained chunks grouped by they original content source.
    pub fn group_by_content<'a>(
        &'a self,
        engine: &'a Engine,
        starting_page: SourceId,
    ) -> ContentIterator {
        ContentIterator {
            typed: self,
            engine,
            next: starting_page,
        }
    }
}

/// A chunk of typed code.
#[derive(Debug)]
pub struct Chunk {
    /// The expression that is evaluated when the chunk is called.
    /// if this expression is set to None, the chunk is associated to a native function declaration
    pub expression: Option<TypedExpr>,

    /// Associated chunk type
    pub tpe: TypeId,
}

/// A group of chunks that were defined in the same content.
#[derive(Debug, Copy, Clone)]
pub struct EncodableContent {
    /// The content identifier the chunks are defined in.
    pub content_id: ContentId,
    start_inclusive: SourceId,
    end_exclusive: SourceId,
}

pub struct ContentIterator<'a> {
    typed: &'a TypedEngine,
    engine: &'a Engine<'a>,
    next: SourceId,
}

impl<'a> Iterator for ContentIterator<'a> {
    type Item = EncodableContent;

    fn next(&mut self) -> Option<Self::Item> {
        // Verify that there is a next chunk.
        if self.next.0 >= self.engine.len() {
            return None;
        }

        // Get the content id of the next chunk.
        let start = self.next;
        let content_id = self
            .engine
            .get_original_content(self.next)
            .expect("Invalid source id");

        // Walk over all chunks that have the same content id.
        while let Some(next_content_id) = self.engine.get_original_content({
            self.next.0 += 1;
            self.next
        }) {
            if next_content_id != content_id {
                break;
            }
        }

        // Return a cursor over the chunks of this content.
        Some(EncodableContent {
            content_id,
            start_inclusive: start,
            end_exclusive: self.next,
        })
    }
}

impl EncodableContent {
    pub fn main_chunk<'a>(
        self,
        it: &'a ContentIterator<'a>,
    ) -> (SourceId, &'a Environment, &'a Chunk) {
        let id = self.start_inclusive.0;
        let chunk = it.typed.entries[id]
            .as_ref()
            .expect("Typed engine not properly filled");
        let environment = it.engine.origins[id]
            .2
            .as_ref()
            .expect("Engine not properly filled");
        (self.start_inclusive, environment, chunk)
    }

    pub fn defined_functions<'a, 'b, F>(
        self,
        type_supplier: F,
        it: &'a ContentIterator<'a>,
    ) -> impl Iterator<Item = (SourceId, &'a Environment, &'a Chunk)>
    where
        F: Fn(TypeId) -> &'b Type + 'a,
    {
        self.function_chunks(it).filter(move |(_, _, chunk)| {
            let chunk_type = type_supplier(chunk.tpe);
            matches!(chunk_type, Type::Function(_, _)) && chunk.expression.is_some()
        })
    }

    pub fn structures<'a, 'b, F>(
        self,
        type_supplier: F,
        it: &'a ContentIterator<'a>,
    ) -> impl Iterator<Item = (SourceId, &'a Environment, &'a Chunk)>
    where
        F: Fn(TypeId) -> &'b Type + 'a,
    {
        self.function_chunks(it).filter(move |(_, _, chunk)| {
            let chunk_type = type_supplier(chunk.tpe);
            matches!(chunk_type, Type::Structure(_, _))
        })
    }

    pub fn function_chunks<'a>(
        self,
        it: &'a ContentIterator<'a>,
    ) -> impl Iterator<Item = (SourceId, &'a Environment, &'a Chunk)> {
        let start = self.start_inclusive.0 + 1;
        let end = self.end_exclusive.0;
        let chunks = it.typed.entries[start..end]
            .iter()
            .map(|chunk| chunk.as_ref().expect("Typed engine not properly filled"));
        let environments = it.engine.origins[start..end]
            .iter()
            .map(|(_, _, env)| env.as_ref().expect("Engine not properly filled"));
        environments
            .zip(chunks)
            .enumerate()
            .map(move |(i, (env, chunk))| (SourceId(start + i), env, chunk))
    }

    pub fn function_count(&self) -> usize {
        self.end_exclusive.0 - self.start_inclusive.0 - 1
    }
}
