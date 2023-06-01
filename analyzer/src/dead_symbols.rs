use context::source::SourceSegment;
use std::collections::HashMap;

use crate::name::Name;
use crate::relations::SourceObjectId;

#[derive(PartialEq, Eq, Hash, Clone)]
pub enum DeadCause {
    InvalidSymbol,
    Unresolved,
}

#[derive(PartialEq, Eq, Hash)]
struct DeadMark {
    env_source: SourceObjectId,
    name: Name,
    cause: DeadCause,
}

#[derive(Default)]
pub struct DeadSymbolsOccurrences {
    dead_symbols_occurrences: HashMap<DeadMark, Vec<SourceSegment>>,
}

impl DeadSymbolsOccurrences {
    pub fn add_occurrence(
        &mut self,
        name: &Name,
        env_id: SourceObjectId,
        cause: DeadCause,
        occurrence: SourceSegment,
    ) {
        let key = DeadMark {
            env_source: env_id,
            name: name.clone(),
            cause,
        };
        let occurrences = self
            .dead_symbols_occurrences
            .entry(key)
            .or_insert_with(|| Vec::new());
        occurrences.push(occurrence)
    }

    pub fn remove_dead_occurrences(
        &mut self,
        name: &Name,
        cause: DeadCause,
        env_id: SourceObjectId,
    ) -> Option<Vec<SourceSegment>> {
        let key = DeadMark {
            env_source: env_id,
            name: name.clone(),
            cause,
        };
        self.dead_symbols_occurrences.remove(&key)
    }
}
