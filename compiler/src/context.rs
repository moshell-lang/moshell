use analyzer::engine::Engine;
use analyzer::environment::Environment;
use analyzer::reef::{Externals, ReefId};
use analyzer::relations::SourceId;
use analyzer::types::engine::{FunctionId, StructureId, TypedEngine};
use analyzer::types::ty::{FunctionDesc, Type, TypeRef};
use analyzer::types::Typing;

use crate::externals::CompilerExternals;
use crate::structure::StructureLayout;
use crate::Captures;

pub struct EmitterContext<'a, 'e> {
    pub(crate) current_reef: ReefId,
    pub(crate) typing: &'a Typing,
    pub(crate) engine: &'a Engine<'e>,
    pub(crate) typed_engine: &'a TypedEngine,
    pub(crate) externals: &'a Externals<'e>,
    pub(crate) compiler_externals: &'a CompilerExternals,

    /// The currently emitted environment.
    ///
    /// It may be used to get the name of the current environment or to get the
    /// current environment's variables.
    pub(crate) environment: &'a Environment,

    /// The captures variables.
    pub(crate) captures: &'a Captures,

    /// The current chunk id.
    pub(crate) chunk_id: SourceId,

    /// Computed layouts of the current reef
    pub(crate) layouts: &'a Vec<StructureLayout>,
}

impl<'a, 'e> EmitterContext<'a, 'e> {
    pub fn get_function(&self, reef: ReefId, id: FunctionId) -> Option<&FunctionDesc> {
        if reef == self.current_reef {
            self.typed_engine.get_function(id)
        } else {
            self.externals
                .get_reef(reef)
                .unwrap()
                .typed_engine
                .get_function(id)
        }
    }

    pub fn get_layout(&self, reef: ReefId, structure_id: StructureId) -> &StructureLayout {
        if reef == self.current_reef {
            &self.layouts[structure_id.0]
        } else {
            &self
                .compiler_externals
                .get_compiled_reef(reef)
                .unwrap()
                .layouts[structure_id.0]
        }
    }

    pub fn get_type(&self, tpe: TypeRef) -> &Type {
        if tpe.reef == self.current_reef {
            self.typing.get_type(tpe.type_id).unwrap()
        } else {
            self.externals
                .get_reef(tpe.reef)
                .unwrap()
                .typing
                .get_type(tpe.type_id)
                .unwrap()
        }
    }

    pub fn get_engine(&self, reef: ReefId) -> Option<&'a Engine<'e>> {
        if self.current_reef == reef {
            Some(self.engine)
        } else {
            self.externals.get_reef(reef).map(|r| &r.engine)
        }
    }
}
