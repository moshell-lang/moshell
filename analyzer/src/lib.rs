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
//!    module.
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
use crate::module::{append, import_multi, ModuleTree};
use crate::symbol::SymbolTable;
use crate::typing::{type_check, TypeChecker, TypeError};
use context::source::Span;
use parser::err::ParseError;
use std::collections::HashMap;
use std::ffi::OsString;
use std::path::{Path, PathBuf};
use std::{fmt, io};

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
    files: FileMemory,

    /// The export tree representing each module.
    exports: ModuleTree,

    /// The symbols that have been found in each file.
    symbols: HashMap<PathBuf, SymbolTable>,

    /// The high-level typed intermediate representation of the code.
    hir: Vec<hir::Module>,
}

struct FileMemory {
    files: HashMap<PathBuf, parser::Root>,
}

impl FileMemory {
    fn get(&self, key: &UnitKey) -> Option<&[ast::Expr]> {
        self.files
            .get(&key.path)
            .map(|root| &root.expressions[key.offset..])
    }
}

pub struct FileImporter {
    root: PathBuf,
}

impl FileImporter {
    pub fn new(root: PathBuf) -> Self {
        Self { root }
    }
}

impl Filesystem for FileImporter {
    fn read(&self, path: &Path) -> io::Result<String> {
        let mut path = self.root.join(path);
        path.set_extension("msh");
        std::fs::read_to_string(path)
    }
}

pub(crate) struct UnitKey {
    pub(crate) path: PathBuf,
    offset: usize,
}

impl fmt::Debug for UnitKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}+{}", self.path, self.offset)
    }
}

impl Reef {
    /// Creates a new empty library with a given name.
    pub fn new(name: OsString) -> Self {
        Self {
            files: FileMemory {
                files: HashMap::new(),
            },
            exports: ModuleTree::new(name),
            symbols: HashMap::new(),
            hir: Vec::new(),
        }
    }

    pub fn clear_cache(&mut self) {
        self.files.files.clear();
        self.hir.clear();
    }
}

impl Database {
    pub fn new() -> Self {
        Self::default()
    }
}

/// Populates the database with a fail-fast strategy.
///
/// An exploration of the source tree will be started from the entrypoint, and the result will be
/// passed to subsequent phases. If an error is encountered during any phase, this function will
/// return early and will not continue to the next phase.
pub fn analyze_multi(
    database: &mut Database,
    reef: &mut Reef,
    fs: &dyn Filesystem,
    entrypoint: &str,
) -> Vec<PipelineError> {
    let mut errors = Vec::<PipelineError>::new();
    let import_result = import_multi(reef, fs, entrypoint);
    errors.extend(import_result.errors.into_iter().map(PipelineError::from));
    if !errors.is_empty() {
        return errors;
    }
    let hoist_result = hoist_files(
        &database.exports,
        reef,
        &mut database.checker,
        import_result.keys,
    );
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

/// Adds or extends a source file to the reef.
///
/// The analysis will be run only on the added content, while reusing the previous symbols and types
/// that may were already defined.
pub fn append_source(
    database: &mut Database,
    reef: &mut Reef,
    fs: &dyn Filesystem,
    path: PathBuf,
    source: &str,
) -> Vec<PipelineError> {
    let mut errors = Vec::<PipelineError>::new();
    let import_result = append(reef, fs, path, source);
    errors.extend(import_result.errors.into_iter().map(PipelineError::from));
    if !errors.is_empty() {
        return errors;
    }
    let hoist_result = hoist_files(
        &database.exports,
        reef,
        &mut database.checker,
        import_result.keys,
    );
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

pub fn freeze_exports(database: &mut Database, mut reef: Reef) {
    let name = reef.exports.name.clone();
    let tree = reef.exports.children.pop().expect("no root module");
    assert!(
        reef.exports.children.is_empty(),
        "root module shouldn't have multiple children"
    );
    assert_eq!(
        tree.name, name,
        "root module name should match the reef name"
    );
    database.exports.insert(name, tree);
}
