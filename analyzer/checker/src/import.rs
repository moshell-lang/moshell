use analyzer_system::name::Name;
use context::source::{OwnedSource, Source};
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fs::read_to_string;
use std::io;
use std::path::PathBuf;

#[derive(Debug)]
pub enum ImportError {
    IO(io::Error),
    Message(String),
}

impl PartialEq for ImportError {
    fn eq(&self, other: &Self) -> bool {
        if let ImportError::Message(m1) = other {
            if let ImportError::Message(m2) = self {
                return m1 == m2;
            }
        }
        false
    }
}

/// An importer is responsible for holding source code from a given import name.
pub trait Importer<'a> {
    /// Gets a source reference from the given import name.
    fn import(&mut self, name: &Name) -> Result<Source<'a>, ImportError>;
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
    fn import(&mut self, name: &Name) -> Result<Source<'a>, ImportError> {
        let source = match self.cache.entry(name.clone()) {
            Entry::Occupied(entry) => unsafe {
                // SAFETY: Source refers to the String behind the entry.
                // It is owned by the importer and is such not bound by the lifetime of the entry.
                std::mem::transmute::<Source, Source<'a>>(entry.get().as_source())
            },
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
                    .map(|content| OwnedSource::new(content, path.to_string_lossy().to_string()))
                    .map_err(ImportError::IO)?;
                entry.insert(source).as_source()
            }
        };
        Ok(unsafe {
            // SAFETY: A source is owned by the importer.
            // 'a is used here to disambiguate the lifetime of the source and the mutable borrow.
            std::mem::transmute::<Source, Source<'a>>(source)
        })
    }
}

pub struct StaticImporter<'a> {
    sources: HashMap<Name, Source<'a>>,
}

impl<'a> StaticImporter<'a> {
    pub fn new<const N: usize>(sources: [(Name, Source<'a>); N]) -> Self {
        Self {
            sources: HashMap::from(sources),
        }
    }
}

impl<'a> Importer<'a> for StaticImporter<'a> {
    fn import(&mut self, name: &Name) -> Result<Source<'a>, ImportError> {
        self.sources
            .get(name)
            .cloned()
            .ok_or_else(|| ImportError::Message(format!("unknown cached source {name}.")))
    }
}
