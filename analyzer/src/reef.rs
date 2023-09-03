use std::collections::hash_map::Entry;
use std::collections::HashMap;

use crate::engine::Engine;
use crate::relations::{ObjectId, Relations};
use crate::types::builtin::lang_reef;
use crate::types::ctx::TypeContext;
use crate::types::engine::TypedEngine;
use crate::types::Typing;

pub struct Reef<'e> {
    pub name: String,

    pub engine: Engine<'e>,
    pub relations: Relations,

    pub typed_engine: TypedEngine,
    pub typing: Typing,
    pub type_context: TypeContext,
}

#[derive(Clone, Copy, Hash, Eq, PartialEq, Debug)]
pub struct ReefId(pub ObjectId);

/// An absolute set of reef.
/// This structure is the highest structure in which the reefs can refer to, because reef's external
/// relations are relative to this set content.
///
/// The reef with id `0` is the lang reef. It is required and is part of the default reef's state.
///
/// [ReefId] identifiers are unique and reef names also needs to be. This way, if a new reef is inserted with a
/// conflicting name, a panic would occur.
pub struct Reefs<'e> {
    names: HashMap<String, ReefId>,
    reefs: Vec<Reef<'e>>,
}

pub const LANG_REEF: ReefId = ReefId(0);

impl Default for Reefs<'_> {
    /// Creates a Reefs set with the required `lang` reef with id 0
    fn default() -> Self {
        Self {
            names: HashMap::from([("lang".to_string(), LANG_REEF)]),
            reefs: vec![lang_reef()],
        }
    }
}

impl<'e> Reefs<'e> {
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

    /// Declares a new empty reef with given name.
    ///
    /// ## Panic
    /// If the name is already reserved by another reef
    fn mk_reef(&mut self, name: String) -> ReefId {
        match self.names.entry(name.clone()) {
            Entry::Occupied(_) => {
                panic!("reef named {} already exists", name)
            }
            Entry::Vacant(v) => {
                let id = ReefId(self.reefs.len());
                self.reefs.push(Reef {
                    name,
                    engine: Engine::default(),
                    relations: Relations::default(),
                    typed_engine: TypedEngine::default(),
                    typing: Typing::default(),
                    type_context: TypeContext::default(),
                });
                v.insert(id);
                id
            }
        }
    }

    fn get_reef_mut(&mut self, id: ReefId) -> Option<&mut Reef<'e>> {
        self.reefs.get_mut(id.0)
    }
}

pub struct ReefContext<'a, 'e> {
    reefs: &'a mut Reefs<'e>,
    pub reef_id: ReefId,
}

impl<'a, 'e> ReefContext<'a, 'e> {
    pub fn declare_new(reefs: &'a mut Reefs<'e>, name: impl Into<String>) -> Self {
        let id = reefs.mk_reef(name.into());
        Self { reefs, reef_id: id }
    }

    pub fn reefs(&self) -> &Reefs<'e> {
        self.reefs
    }

    pub fn current_reef_mut(&mut self) -> &mut Reef<'e> {
        self.reefs.get_reef_mut(self.reef_id).unwrap()
    }

    pub fn current_reef(&self) -> &Reef<'e> {
        self.reefs.get_reef(self.reef_id).unwrap()
    }
}
