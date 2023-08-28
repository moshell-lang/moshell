use std::collections::hash_map::Entry;
use std::collections::HashMap;

use crate::engine::Engine;
use crate::relations::{ObjectId, Relations};
use crate::types::engine::TypedEngine;

pub struct Reef<'e> {
    pub name: String,
    pub engine: Engine<'e>,
    pub relations: Relations,
    pub types: TypedEngine,
}

#[derive(Clone, Copy, Hash, Eq, PartialEq, Debug)]
pub struct ReefId(pub ObjectId);

#[derive(Default)]
pub struct Reefs<'e> {
    names: HashMap<String, ReefId>,
    reefs: Vec<Reef<'e>>,
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
                    types: TypedEngine::default(),
                });
                v.insert(id);
                id
            }
        }
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
        self.reefs
            .get_reef_mut(self.reef_id)
            .expect("reefs does not contains current reef")
    }

    pub fn current_reef(&self) -> &Reef<'e> {
        self.reefs
            .get_reef(self.reef_id)
            .expect("reefs does not contains current reef")
    }
}
