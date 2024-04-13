//! Explores a whole Moshell source tree and verify it.
//!
//! Moshell is a modular and statically typed language. Sources files are organized in libraries
//! called "reefs" and reefs are composed of a hierarchy of modules that contains code. [`Reef`]s
//! are analyzed one at a time and may have non-cyclic dependencies between them. The analysis
//! fills a [`Database`] with a type-checked representation of the code.
//!
//! The analysis is done in a pipeline:
//! 1. *Importing*: the whole project is parsed and indexed in a list of exports and imports.
//! 2. *Hoisting*: the types and symbols are discovered and placed in the global scope of each
//!   module.
//! 3. *Type checking*: the types are checked for consistency and errors are reported.
//!
//! Each phase uses the results of the previous ones, but each phase take can work with partial
//! results. When the input has traversed the whole pipeline, it may be considered as valid to
//! pass to a compiler.

pub mod hir;
mod hoist;
mod module;
pub mod symbol;
pub mod typing;

use crate::hoist::hoist_files;
use crate::module::{import_multi, ModuleTree};
use crate::symbol::SymbolTable;
use crate::typing::{type_check, TypeChecker, TypeError};
use context::source::Span;
use parser::err::ParseError;
use std::collections::HashMap;
use std::ffi::OsString;
use std::io;
use std::path::{Path, PathBuf};

/// A byte range in a file.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SourceLocation {
    /// The path to the file, relative to the project root.
    pub path: PathBuf,

    /// The byte indices of the start and end of the span.
    pub span: Span,
}

impl SourceLocation {
    /// Creates a new [`SourceLocation`].
    pub fn new(path: PathBuf, span: Span) -> Self {
        Self { path, span }
    }
}

#[derive(Debug)]
pub enum PipelineError {
    Import {
        path: PathBuf,
        error: io::Error,
        cause: Option<SourceLocation>,
    },
    Parse {
        path: PathBuf,
        error: ParseError,
    },
    Type(TypeError),
}

/// Fetches a content string given a path.
pub trait Filesystem {
    /// Reads the content of a file.
    fn read(&self, path: &Path) -> io::Result<String>;
}

/// A [`Filesystem`] that stores files in memory.
pub(crate) struct MemoryFilesystem<'a> {
    files: HashMap<PathBuf, &'a str>,
}

impl Filesystem for MemoryFilesystem<'_> {
    fn read(&self, path: &Path) -> io::Result<String> {
        self.files
            .get(path)
            .map(|&content| content.to_owned())
            .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "file not found"))
    }
}

impl<'a> MemoryFilesystem<'a> {
    pub(crate) fn new(files: HashMap<PathBuf, &'a str>) -> Self {
        Self { files }
    }
}

impl<'a, P: AsRef<Path>> FromIterator<(P, &'a str)> for MemoryFilesystem<'a> {
    fn from_iter<T: IntoIterator<Item = (P, &'a str)>>(iter: T) -> Self {
        Self::new(
            iter.into_iter()
                .map(|(path, content)| (path.as_ref().to_path_buf(), content))
                .collect(),
        )
    }
}

/// A global storage of pre-analyzed modules and their types.
#[derive(Default)]
pub struct Database {
    exports: HashMap<OsString, ModuleTree>,
    pub checker: TypeChecker,
}

/// A yet-to-be-analyzed set of files.
pub struct Reef {
    /// The parsed abstract syntax trees of the files.
    files: HashMap<PathBuf, parser::Root>,

    /// The export tree representing each module.
    exports: ModuleTree,

    /// The symbols that have been found in each file.
    symbols: HashMap<PathBuf, SymbolTable>,

    /// The high-level typed intermediate representation of the code.
    hir: HashMap<PathBuf, hir::Module>,
}

impl Reef {
    /// Creates a new empty library with a given name.
    pub fn new(name: OsString) -> Self {
        Self {
            files: HashMap::new(),
            exports: ModuleTree::new(name),
            symbols: HashMap::new(),
            hir: HashMap::new(),
        }
    }
}

impl Database {
    pub fn new() -> Self {
        Self::default()
    }
}

/// Populates the database with a fail-fast strategy.
pub fn analyze_multi(
    database: &mut Database,
    reef: &mut Reef,
    fs: &dyn Filesystem,
    entrypoint: &str,
) -> Vec<PipelineError> {
    let mut errors = Vec::<PipelineError>::new();
    errors.extend(
        import_multi(reef, fs, entrypoint)
            .into_iter()
            .map(PipelineError::from),
    );
    if !errors.is_empty() {
        return errors;
    }
    let hoist_result = hoist_files(&database.exports, reef, &mut database.checker);
    errors.extend(hoist_result.errors.into_iter().map(PipelineError::from));
    if !errors.is_empty() {
        return errors;
    }
    errors.extend(
        type_check(reef, database, hoist_result.sorted)
            .into_iter()
            .map(PipelineError::from),
    );
    errors
}
