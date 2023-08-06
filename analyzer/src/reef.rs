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

pub struct Reefs<'e> {
    names: HashMap<String, ReefId>,
    reefs: Vec<Reef<'e>>,
}

pub trait ReefAccessor<'e> {
    fn lang(&self) -> &Reef<'e>;
    fn get_reef(&self, id: ReefId) -> Option<&Reef<'e>>;
    fn get_reef_by_name(&self, name: &str) -> Option<(&Reef<'e>, ReefId)>;
}

pub const LANG_REEF: ReefId = ReefId(0);

impl Default for Reefs<'_> {
    fn default() -> Self {
        Self {
            names: HashMap::from([("lang".to_string(), ReefId(0))]),
            reefs: vec![lang_reef()],
        }
    }
}

impl<'e> Reefs<'e> {
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

impl<'e> ReefAccessor<'e> for Reefs<'e> {
    fn lang(&self) -> &Reef<'e> {
        &self.reefs[0]
    }

    fn get_reef(&self, id: ReefId) -> Option<&Reef<'e>> {
        self.reefs.get(id.0)
    }

    fn get_reef_by_name(&self, name: &str) -> Option<(&Reef<'e>, ReefId)> {
        self.names
            .get(name)
            .and_then(|id| self.get_reef(*id).map(|reef| (reef, *id)))
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
