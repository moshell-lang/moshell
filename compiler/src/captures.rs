use analyzer::reef::ReefId;
use analyzer::relations::{ResolvedSymbol, SourceId};
use std::collections::HashMap;

#[derive(Default, Debug, Eq, PartialEq)]
pub struct Captures {
    captures: Vec<Option<Vec<ResolvedSymbol>>>,
}

impl Captures {
    pub fn new(captures: Vec<Option<Vec<ResolvedSymbol>>>) -> Self {
        Self { captures }
    }

    pub fn get(&self, source: SourceId) -> Option<&Vec<ResolvedSymbol>> {
        self.captures[source.0].as_ref()
    }
}

#[derive(Default, Debug)]
pub struct ReefsCaptures {
    map: HashMap<ReefId, Captures>,
}

impl ReefsCaptures {
    pub fn insert(&mut self, reef: ReefId, captures: Captures) {
        self.map.insert(reef, captures);
    }

    pub fn get(&self, reef: &ReefId) -> Option<&Captures> {
        self.map.get(&reef)
    }
}
