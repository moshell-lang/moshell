use analyzer_system::name::Name;
use context::source::{OwnedSource, Source};
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fs::read_to_string;
use std::io;
use std::path::PathBuf;

/// An importer is responsible for holding source code from a given import name.
pub trait Importer<'a> {
    /// Gets a source reference from the given import name.
    fn import(&mut self, name: &Name) -> Result<Source, io::Error>;
}

/// An importer that reads files from a given root directory.
pub struct FileImporter {
    /// The root directory from which to read files.
    root: PathBuf,

    /// A cache that holds the source code of files that have already been read.
    ///
    /// The main purpose the importer is to be the owner of the source, so it should be
    /// assumed that entries are never removed from the cache.
    cache: HashMap<Name, OwnedSource>,
}

impl FileImporter {
    pub fn new(root: PathBuf) -> Self {
        Self {
            root,
            cache: HashMap::new(),
        }
    }
}

impl<'a> Importer<'a> for FileImporter {
    fn import(&mut self, name: &Name) -> Result<Source, io::Error> {
        match self.cache.entry(name.clone()) {
            Entry::Occupied(entry) => Ok(unsafe {
                // SAFETY: A source is only removed from the cache when the importer is dropped.
                // Because a source is never dropped before, the reference is valid for Self's lifetime.
                std::mem::transmute::<Source, Source>(entry.get().as_source())
            }),
            Entry::Vacant(entry) => {
                let mut path = self.root.clone();
                path.push(name.parts().to_owned().join("/"));
                path.set_extension("msh");
                let source = read_to_string(&path)
                    .or_else(|_| {
                        path.pop();
                        path.set_extension("msh");
                        read_to_string(&path)
                    })
                    .map(|content| OwnedSource::new(content, path.to_string_lossy().to_string()))?;
                Ok(entry.insert(source).as_source())
            }
        }
    }
}
