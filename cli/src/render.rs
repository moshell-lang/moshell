use std::collections::HashMap;
use std::fmt::{Debug, Display};

use ariadne::Cache;

use analyzer::relations::SourceId;
use context::source::Source;

pub(crate) mod diagnostic;
pub(crate) mod parse_error;

pub struct SourcesCache<S> {
    supplier: S,
    names: HashMap<SourceId, String>,
    sources: HashMap<String, ariadne::Source>,
}

impl<'a, S> SourcesCache<S> {
    pub fn new(supplier: S) -> Self
    where
        S: Fn(SourceId) -> Source<'a>,
    {
        Self {
            supplier,
            names: HashMap::new(),
            sources: HashMap::new(),
        }
    }
}

impl<'a, S> Cache<SourceId> for SourcesCache<S>
where
    S: Fn(SourceId) -> Source<'a>,
{
    fn fetch(&mut self, id: &SourceId) -> Result<&ariadne::Source, Box<dyn Debug + '_>> {
        let pos = self.names.get(id);
        match pos {
            Some(name) => Ok(self.sources.get(name).expect("unable to find source")),
            None => {
                let source = (self.supplier)(*id);
                self.names.insert(*id, source.name.to_string());
                let src = self
                    .sources
                    .entry(source.name.to_string())
                    .or_insert_with(|| ariadne::Source::from(source.source));
                Ok(src)
            }
        }
    }

    fn display<'b>(&self, id: &'b SourceId) -> Option<Box<dyn Display + 'b>> {
        let source_name = self.names.get(id).expect("unable to find source").clone();
        Some(Box::new(source_name))
    }
}
