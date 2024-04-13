use crate::structure::StructureLayout;
use analyzer::typing::registry::Registry;
use analyzer::typing::user::TypeArena;

pub(crate) struct EmitterContext<'a> {
    pub(crate) types: &'a TypeArena,
    pub(crate) registry: &'a Registry,

    /// Computed layouts of the current reef
    pub(crate) layouts: &'a Vec<StructureLayout>,
}
