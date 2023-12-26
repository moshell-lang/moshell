use analyzer::reef::{ReefId, LANG_REEF};

use crate::structure::StructureLayout;

#[derive(Debug, Clone, PartialEq, Default)]
pub struct CompiledReef {
    pub layouts: Vec<StructureLayout>,
}

/// contains information about external reefs.
/// This structure is an overload of the analyzer's external with
/// additional information required for compilation.
/// ReefIds starts at: the lang reef is not compiled, and its structures are intrisics to the compiler
/// thus it will never try to access a compiled reef for the lang reef.
#[derive(Default)]
pub struct CompilerExternals {
    compiled_reefs: Vec<CompiledReef>,
}

impl CompilerExternals {
    pub fn get_compiled_reef(&self, id: ReefId) -> Option<&CompiledReef> {
        debug_assert!(
            id != LANG_REEF,
            "lang reef does not have compilation information"
        );
        self.compiled_reefs.get(id.0 - 1)
    }

    pub fn set(&mut self, id: ReefId, reef: CompiledReef) {
        if self.compiled_reefs.len() < id.0 {
            self.compiled_reefs.resize(id.0, CompiledReef::default())
        }
        self.compiled_reefs[id.0 - 1] = reef
    }
}
