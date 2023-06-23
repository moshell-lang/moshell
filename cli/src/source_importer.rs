use std::collections::HashMap;
use std::fs::read_to_string;
use std::io;
use std::path::PathBuf;

use analyzer::name::Name;
use context::source::{OwnedSource, Source};

const MOSHELL_EXTENSION: &str = "msh";

/// An importer that reads files from a given root directory.
pub struct FileSourceImporter {
    /// The root directory from which to read files.
    root: PathBuf,

    /// A cache that holds the source code of files that have already been read.
    ///
    /// The main purpose the importer is to be the owner of the source, so it should be
    /// assumed that entries are never removed from the cache.
    cache: HashMap<PathBuf, OwnedSource>,
}

impl<'a> FileSourceImporter {
    pub fn new(root: PathBuf) -> Self {
        Self {
            root,
            cache: HashMap::new(),
        }
    }

    pub fn list_sources(&self) -> impl Iterator<Item = Source<'a>> + '_ {
        self.cache.values().map(|source| {
            unsafe {
                // SAFETY: A source is owned by the importer.
                // 'a is used here to disambiguate the lifetime of the source and the mutable borrow.
                std::mem::transmute::<Source, Source<'a>>(source.as_source())
            }
        })
    }

    pub fn get_already_imported_name(&self, name: &Name) -> Option<Source<'a>> {
        let mut path = self.root.clone();
        path.push(name.parts().to_owned().join("/"));
        path = path.with_extension(MOSHELL_EXTENSION);
        self.get_already_imported(path).map(|source| {
            unsafe {
                // SAFETY: A source is owned by the importer.
                // 'a is used here to disambiguate the lifetime of the source and the mutable borrow.
                std::mem::transmute::<Source, Source<'a>>(source.as_source())
            }
        })
    }

    fn get_already_imported(&self, mut path: PathBuf) -> Option<&OwnedSource> {
        self.cache.get(&path).or_else(|| {
            path.pop();
            path.set_extension(MOSHELL_EXTENSION);
            self.cache.get(&path)
        })
    }

    pub fn import_source(&mut self, name: &Name) -> Result<Source<'a>, io::Error> {
        let mut path = self.root.clone();
        path.push(name.parts().to_owned().join("/"));
        path.set_extension(MOSHELL_EXTENSION);
        if let Some(source) = self.get_already_imported(path.clone()) {
            return Ok(unsafe {
                // SAFETY: A source is owned by the importer.
                // 'a is used here to disambiguate the lifetime of the source and the mutable borrow.
                std::mem::transmute::<Source, Source<'a>>(source.as_source())
            });
        }
        let source = read_to_string(&path)
            .or_else(|_| {
                path.pop();
                path.set_extension(MOSHELL_EXTENSION);
                read_to_string(&path)
            })
            .map(|content| OwnedSource::new(content, path.to_string_lossy().to_string()))?;

        let source = self.cache.entry(path.clone()).or_insert(source).as_source();
        Ok(unsafe {
            // SAFETY: A source is owned by the importer.
            // 'a is used here to disambiguate the lifetime of the source and the mutable borrow.
            std::mem::transmute::<Source, Source<'a>>(source)
        })
    }
}
