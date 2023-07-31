use crate::engine::Engine;
use crate::environment::Environment;
use crate::relations::{Definition, SourceId};
use crate::types::builtin::lang;
use crate::types::hir::{TypeId, TypedExpr};
use crate::types::ty::{FunctionType, MethodType, Parameter, TypeDescription};
use crate::types::UNIT;
use context::source::ContentId;

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

    /// Descriptions of types.
    descriptions: Vec<TypeDescription>,

    /// Native code that powers the language.
    ///
    /// It powers primitives types, and is indexed by [`NativeObjectId`]s.
    natives: Vec<FunctionType>,
}

pub enum CodeEntry<'a> {
    User(&'a Chunk),
    Native(&'a FunctionType),
}

impl CodeEntry<'_> {
    pub fn parameters(&self) -> &[Parameter] {
        match self {
            CodeEntry::User(chunk) => chunk.parameters.as_slice(),
            CodeEntry::Native(native) => native.parameters.as_slice(),
        }
    }

    pub fn return_type(&self) -> TypeId {
        match self {
            CodeEntry::User(chunk) => chunk.return_type,
            CodeEntry::Native(native) => native.return_type,
        }
    }
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

    pub is_script: bool,
}

impl Chunk {
    /// Creates a new script chunk.
    pub fn script(expression: TypedExpr) -> Self {
        Self {
            expression,
            parameters: Vec::new(),
            return_type: UNIT,
            is_script: true,
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
            is_script: false,
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
            natives: Vec::new(),
        };
        builder.entries.resize_with(capacity, || None);
        lang(&mut builder);
        builder
    }

    /// Returns the chunk with the given id.
    pub fn get(&self, def: Definition) -> Option<CodeEntry> {
        match def {
            Definition::User(id) => self.get_user(id).map(CodeEntry::User),
            Definition::Native(id) => self.natives.get(id.0).map(CodeEntry::Native),
        }
    }

    /// Returns the chunk with the given source id.
    ///
    /// If the chunk is not a user defined chunk, [`None`] is returned.
    /// Use [`Self::get`] to get both user defined and native chunks.
    pub fn get_user(&self, id: SourceId) -> Option<&Chunk> {
        self.entries.get(id.0)?.as_ref()
    }

    /// Inserts a chunk into the engine.
    pub fn insert(&mut self, id: SourceId, entry: Chunk) {
        self.entries[id.0] = Some(entry);
    }

    /// Lists methods with a given name of a given type.
    ///
    /// If the type is unknown or doesn't have any methods with the given name,
    /// [`None`] is returned.
    pub fn get_methods(&self, type_id: TypeId, name: &str) -> Option<&Vec<MethodType>> {
        self.descriptions.get(type_id.0)?.methods.get(name)
    }

    /// Gets the method that matches exactly the given arguments and return type.
    pub fn get_method_exact(
        &self,
        type_id: TypeId,
        name: &str,
        args: &[TypeId],
        return_type: TypeId,
    ) -> Option<&MethodType> {
        self.get_methods(type_id, name).and_then(|methods| {
            methods.iter().find(|method| {
                method.parameters.iter().map(|p| &p.ty).eq(args)
                    && method.return_type == return_type
            })
        })
    }

    /// Adds a new method to a type.
    ///
    /// The method may not conflict with any existing methods.
    pub fn add_method(&mut self, type_id: TypeId, name: &str, method: MethodType) {
        // Extend the vector of type descriptions if necessary.
        if type_id.0 >= self.descriptions.len() {
            self.descriptions
                .resize_with(type_id.0 + 1, Default::default);
        }
        self.descriptions[type_id.0]
            .methods
            .entry(name.to_owned())
            .or_default()
            .push(method);
    }

    /// Inserts a chunk into the engine if it is not already present.
    ///
    /// This may be used to insert semi-accurate chunks into the engine.
    pub fn insert_if_absent(&mut self, id: SourceId, entry: Chunk) {
        if self.entries[id.0].is_none() {
            self.entries[id.0] = Some(entry);
        }
    }

    /// returns an iterator over all contained chunks with their identifier
    pub fn iter_chunks(&self) -> impl Iterator<Item = (SourceId, &Chunk)> {
        self.entries
            .iter()
            .enumerate()
            .filter_map(|(id, chunk)| chunk.as_ref().map(|chunk| (SourceId(id), chunk)))
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
