use crate::Analyzer;
use std::collections::HashMap;

use crate::engine::Engine;
use crate::relations::{ObjectId, Relations};
use crate::types::builtin::lang_reef;
use crate::types::ctx::TypeContext;
use crate::types::engine::TypedEngine;
use crate::types::Typing;

#[derive(Debug)]
pub struct Reef<'e> {
    pub name: String,

    pub engine: Engine<'e>,
    pub relations: Relations,

    pub typed_engine: TypedEngine,
    pub typing: Typing,
    pub type_context: TypeContext,
}

impl<'e> Reef<'e> {
    pub fn new(name: String, analyzer: Analyzer<'e>) -> Self {
        Self {
            name,
            engine: analyzer.resolution.engine,
            relations: analyzer.resolution.relations,
            typed_engine: analyzer.engine,
            typing: analyzer.typing,
            type_context: analyzer.type_context,
        }
    }

    pub fn new_partial(name: String, engine: Engine<'e>, relations: Relations) -> Self {
        Self {
            name,
            engine,
            relations,
            typed_engine: TypedEngine::default(),
            typing: Typing::default(),
            type_context: TypeContext::default(),
        }
    }
}

#[derive(Clone, Copy, Hash, Eq, PartialEq, Debug)]
pub struct ReefId(pub ObjectId);

pub struct Externals<'a> {
    pub current: ReefId,
    names: HashMap<String, ReefId>,
    reefs: Vec<Reef<'a>>,
}

pub const LANG_REEF: ReefId = ReefId(0);

impl Default for Externals<'_> {
    /// Creates a Reefs set with the required `lang` reef with id 0
    fn default() -> Self {
        Self {
            current: ReefId(1),
            names: HashMap::from([("lang".to_string(), LANG_REEF)]),
            reefs: vec![lang_reef()],
        }
    }
}

impl<'e> Externals<'e> {
    /// Return the lang's reef
    pub fn lang(&self) -> &Reef<'e> {
        &self.reefs[LANG_REEF.0]
    }

    pub fn get_reef(&self, id: ReefId) -> Option<&Reef<'e>> {
        self.reefs.get(id.0)
    }

    pub fn get_reef_by_name(&self, name: &str) -> Option<(&Reef<'e>, ReefId)> {
        self.names
            .get(name)
            .and_then(|id| self.get_reef(*id).map(|reef| (reef, *id)))
    }

    fn get_reef_mut(&mut self, id: ReefId) -> Option<&mut Reef<'e>> {
        self.reefs.get_mut(id.0)
    }

    pub fn register(&mut self, reef: Reef<'e>) -> ReefId {
        let id = ReefId(self.reefs.len());
        if self.names.insert(reef.name.clone(), id).is_some() {
            panic!("Reef with name {} already registered", reef.name);
        }
        self.reefs.push(reef);
        self.current.0 += 1;
        id
    }
}
