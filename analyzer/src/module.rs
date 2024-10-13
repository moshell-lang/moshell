//! A source explorer to create an in-memory representation of the units of a reef.
//!
//! # Overview
//! In order to perform a full static analysis of a program, the analyzer needs to know about all
//! the files and their symbols. The importer starts from an entrypoint and recursively imports all
//! the referred names. While there is content to discover, it will ask the [`Filesystem`] to read
//! what's behind the path. Each reef may contain multiple files, grouped in different modules, that
//! themselves may contain other modules.
//!
//! A path may refer to a module, i.e. a complete file, or a precise symbol within a module. This
//! first analysis step seeks to concretize these paths, because the analyzer needs the full source
//! code to construct a virtual representations of the symbols and their modules. When encountering
//! a path, it will try to import it as file. It will find nothing if the path ends with the symbol
//! that should be in that file. So it will try again, but with the last component removed. If it
//! is still not found, it will continue to pop the path components until it finds a file to parse.

use crate::symbol::{SymbolDesc, SymbolRegistry, UndefinedSymbol};
use crate::typing::user::{TypeId, UNKNOWN_TYPE};
use crate::typing::{TypeError, TypeErrorKind};
use crate::{Filesystem, PipelineError, Reef, SourceLocation, UnitKey};
use ast::call::ProgrammaticCall;
use ast::function::FunctionDeclaration;
use ast::r#use::{Import as ImportExpr, ImportList, ImportedSymbol, InclusionPathItem, Use};
use ast::variable::VarDeclaration;
use ast::Expr;
use context::source::{SourceSegment, SourceSegmentHolder, Span};
use parser::err::ParseError;
use parser::Root;
use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};
use std::ffi::{OsStr, OsString};
use std::io;
use std::path::{Path, PathBuf};

/// A symbol that can be accessed from other modules.
pub(super) struct Export {
    /// The name from which this symbol can be accessed.
    pub(super) name: String,

    /// The span where this symbol is defined.
    pub(super) span: Span,

    /// The kind of symbol that it is.
    pub(super) registry: SymbolRegistry,

    /// The type of the symbol.
    ///
    /// It may be an [`UNKNOWN_TYPE`] if the type is not known yet.
    pub(super) ty: TypeId,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) struct ResolvedImport {
    pub(crate) path: PathBuf,
    pub(crate) export_idx: usize,
}

/// An instruction to import a module.
#[derive(Debug)]
struct Import {
    path: PathBuf,
    origin: Option<SourceLocation>,
}

#[derive(Debug)]
pub(super) enum ModuleError {
    /// A requested module cannot be accessed or read.
    ///
    /// The module may be directly requested or indirectly imported by another module.
    Import {
        /// The inner error that caused the import to fail.
        error: io::Error,

        /// The location of the import statement that caused the error.
        ///
        /// It may be `None` if the error comes from the entrypoint, as there is no span to point to.
        cause: Option<SourceLocation>,
    },

    /// A module encountered a syntax error while being parsed.
    Parse {
        /// The path to the module that caused the error.
        path: PathBuf,

        /// The syntax error that occurred while parsing the module.
        error: ParseError,
    },
    Duplicate {
        /// The name of the symbol that is exported multiple times.
        name: String,
        path: PathBuf,
        first: Span,
        second: Span,
    },
}

impl PartialEq for ModuleError {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Import { error: a, cause: b }, Self::Import { error: x, cause: y }) => {
                a.kind() == x.kind() && b == y
            }
            (Self::Parse { path: a, error: b }, Self::Parse { path: x, error: y }) => {
                a == x && b == y
            }
            (
                Self::Duplicate {
                    name: a,
                    path: b,
                    first: c,
                    second: d,
                },
                Self::Duplicate {
                    name: x,
                    path: y,
                    first: z,
                    second: w,
                },
            ) => a == x && b == y && c == z && d == w,
            _ => false,
        }
    }
}

impl From<ModuleError> for PipelineError {
    fn from(error: ModuleError) -> Self {
        match error {
            ModuleError::Import { error, cause } => PipelineError::Import {
                path: PathBuf::new(),
                error,
                cause,
            },
            ModuleError::Parse { path, error } => PipelineError::Parse { path, error },
            ModuleError::Duplicate {
                name,
                path,
                first,
                second,
            } => PipelineError::Type(TypeError::new(
                TypeErrorKind::DuplicateSymbol {
                    name,
                    previous: first,
                },
                SourceLocation::new(path, second),
            )),
        }
    }
}

/// A module entry in a tree of modules.
///
/// Modules abstract over the filesystem where each entry exposes a list of exports, i.e. symbols
/// that can be accessed from other modules. Those symbols may refer to functions, types, constants
/// or other modules.
pub struct ModuleTree {
    /// The local name of the module.
    pub name: OsString,

    /// The public symbols that this module exports.
    ///
    /// Each visitor should access this vector to find the symbols that can be used.
    pub exports: Vec<Export>,

    /// The submodules that this module contains.
    ///
    /// Part of them may be present in the exports list.
    pub children: Vec<ModuleTree>,
}

impl ModuleTree {
    pub fn new(name: OsString) -> Self {
        Self {
            name,
            exports: Vec::new(),
            children: Vec::new(),
        }
    }

    pub fn insert(&mut self, path: &Path, exports: Vec<Export>) {
        let mut current = self;
        for component in path.iter() {
            let found = current
                .children
                .iter()
                .position(|module| module.name == component);
            match found {
                Some(index) => {
                    current = &mut current.children[index];
                }
                None => {
                    let module = ModuleTree {
                        name: component.to_os_string(),
                        children: Vec::new(),
                        exports: Vec::new(),
                    };
                    current.children.push(module);
                    current = current.children.last_mut().expect("module just added");
                }
            }
        }
        current.exports.extend(exports);
    }

    pub fn get(&self, part: &OsStr) -> Option<&ModuleTree> {
        self.children.iter().find(|module| module.name == part)
    }

    pub fn get_full(&self, path: &Path) -> Option<&ModuleTree> {
        let mut current = self;
        for component in path.iter() {
            current = current
                .children
                .iter()
                .find(|module| module.name == component)?;
        }
        Some(current)
    }

    pub fn get_full_mut(&mut self, path: &Path) -> Option<&mut ModuleTree> {
        let mut current = self;
        for component in path.iter() {
            current = current
                .children
                .iter_mut()
                .find(|module| module.name == component)?;
        }
        Some(current)
    }

    pub fn take_exports(&mut self, path: &Path) -> Vec<Export> {
        let mut current = self;
        for component in path.iter() {
            if let Some(module) = current
                .children
                .iter_mut()
                .find(|module| module.name == component)
            {
                current = module;
            } else {
                return Vec::new();
            }
        }
        std::mem::take(&mut current.exports)
    }

    pub fn find_export(
        &self,
        name: &str,
        registry: SymbolRegistry,
    ) -> Result<&Export, UndefinedSymbol> {
        let mut other_export: Option<&Export> = None;
        for export in self.exports.iter().rev() {
            if export.name == name {
                if export.registry == registry {
                    return Ok(export);
                } else {
                    other_export = Some(export);
                }
            }
        }
        Err(match other_export {
            Some(export) => UndefinedSymbol::WrongRegistry(SymbolDesc::from(export)),
            None => UndefinedSymbol::NotFound,
        })
    }
}

pub(crate) struct ImportResult {
    pub(crate) errors: Vec<ModuleError>,
    pub(crate) keys: Vec<UnitKey>,
}

#[derive(Clone, Copy)]
pub(crate) struct ModuleView<'a> {
    pub(crate) current: &'a ModuleTree,
    pub(crate) foreign: &'a HashMap<OsString, ModuleTree>,
}

impl<'a> ModuleView<'a> {
    pub(crate) fn new(current: &'a ModuleTree, foreign: &'a HashMap<OsString, ModuleTree>) -> Self {
        Self { current, foreign }
    }

    pub(crate) fn get(&self, item: &InclusionPathItem) -> Option<&'a ModuleTree> {
        match item {
            InclusionPathItem::Symbol(ident) => self.foreign.get(OsStr::new(ident.value.as_str())),
            InclusionPathItem::Reef(_) => Some(self.current),
        }
    }

    pub(crate) fn get_direct(&self, path: &[InclusionPathItem]) -> Option<&'a ModuleTree> {
        let (first, rest) = path.split_first().expect("path should not be empty");
        let mut tree = self.get(first)?;
        for item in rest {
            tree = match item {
                InclusionPathItem::Symbol(ident) => tree.get(OsStr::new(ident.value.as_str()))?,
                InclusionPathItem::Reef(_) => return None,
            };
        }
        Some(tree)
    }
}

/// Access all related files starting from the entrypoint.
pub(super) fn import_multi(reef: &mut Reef, fs: &dyn Filesystem, entrypoint: &str) -> ImportResult {
    let imports = vec![Import {
        path: PathBuf::from(entrypoint),
        origin: None,
    }];
    explore(reef, fs, imports, Vec::new())
}

pub(crate) fn append(
    reef: &mut Reef,
    fs: &dyn Filesystem,
    path: PathBuf,
    source: &str,
) -> ImportResult {
    let report = parser::parse(source);
    let mut unit = UnitKey {
        path: path.clone(),
        offset: 0,
    };
    match reef.files.files.entry(path) {
        Entry::Occupied(mut existing) => {
            unit.offset = existing.get().expressions.len();
            existing.get_mut().expressions.extend(report.expr);
        }
        Entry::Vacant(vacant) => {
            vacant.insert(Root {
                expressions: report.expr,
            });
        }
    }
    explore(reef, fs, Vec::<Import>::new(), vec![unit])
}

fn explore(
    reef: &mut Reef,
    fs: &dyn Filesystem,
    mut imports: Vec<Import>,
    mut keys: Vec<UnitKey>,
) -> ImportResult {
    let mut errors = Vec::<ModuleError>::new();
    let mut visited = HashSet::<PathBuf>::new();
    while let Some(Import { mut path, origin }) = imports.pop() {
        if !visited.insert(path.clone()) {
            continue;
        }
        let source = match fs.read(path.as_path()) {
            Ok(source) => source,
            Err(error) => {
                if error.kind() == io::ErrorKind::NotFound && path.pop() {
                    imports.push(Import { path, origin });
                    continue;
                }
                errors.push(ModuleError::Import {
                    error,
                    cause: origin,
                });
                continue;
            }
        };
        let report = parser::parse(&source);
        let root = Root {
            expressions: report.expr,
        };
        errors.extend(report.errors.into_iter().map(|error| ModuleError::Parse {
            path: path.clone(),
            error,
        }));
        let mut exports = Vec::<Export>::new();
        for duplicated in hoist_exports(&root, &mut exports) {
            errors.push(ModuleError::Duplicate {
                name: duplicated.name,
                path: path.clone(),
                first: duplicated.first,
                second: duplicated.second,
            });
        }
        list_imports(&root, &path, &mut imports);
        reef.exports.insert(&path, exports);
        keys.push(UnitKey {
            path: path.clone(),
            offset: 0,
        });
        reef.files.files.insert(path, root);
    }
    ImportResult { errors, keys }
}

#[derive(Debug, PartialEq, Eq)]
struct Duplicated {
    name: String,
    first: Span,
    second: Span,
}

fn hoist_exports(root: &Root, exports: &mut Vec<Export>) -> Vec<Duplicated> {
    let mut duplicates = Vec::<Duplicated>::new();
    for expr in &root.expressions {
        if let Expr::FunctionDeclaration(FunctionDeclaration { name, segment, .. }) = expr {
            if let Some(exported) = exports.iter().find(|export| export.name == name.value) {
                duplicates.push(Duplicated {
                    name: name.to_string(),
                    first: exported.span.clone(),
                    second: segment.clone(),
                });
            } else {
                exports.push(Export {
                    name: name.to_string(),
                    span: segment.clone(),
                    registry: SymbolRegistry::Function,
                    ty: UNKNOWN_TYPE,
                });
            }
        } else if let Expr::VarDeclaration(VarDeclaration { var, segment, .. }) = expr {
            let export = Export {
                name: var.name.to_string(),
                span: segment.clone(),
                registry: SymbolRegistry::Variable,
                ty: UNKNOWN_TYPE,
            };
            if let Some(exported_idx) = exports
                .iter()
                .position(|export| export.name == var.name.value.as_str())
            {
                // if the root variable was already declared, shadow it with the most recent variable declaration
                exports[exported_idx] = export;
            } else {
                exports.push(export);
            }
        } else if let Expr::StructDeclaration(decl) = expr {
            if let Some(exported) = exports
                .iter()
                .find(|export| export.name == decl.name.value.as_str())
            {
                duplicates.push(Duplicated {
                    name: decl.name.to_string(),
                    first: exported.span.clone(),
                    second: decl.name.segment().clone(),
                });
            } else {
                exports.push(Export {
                    name: decl.name.to_string(),
                    span: decl.name.segment().clone(),
                    registry: SymbolRegistry::Type,
                    ty: UNKNOWN_TYPE,
                });
            }
        }
    }
    duplicates
}

fn list_imports(root: &Root, path: &Path, imports: &mut Vec<Import>) {
    for expr in &root.expressions {
        list_imports_expr(expr, path, imports);
    }
}

fn list_imports_expr(expr: &Expr, path: &Path, imports: &mut Vec<Import>) {
    match expr {
        Expr::Use(Use { import, segment }) => {
            add_import(import, path, segment.clone(), imports);
        }
        Expr::VarDeclaration(VarDeclaration {
            initializer: Some(initializer),
            ..
        }) => {
            list_imports_expr(initializer, path, imports);
        }
        Expr::ProgrammaticCall(ProgrammaticCall {
            path: include_path,
            arguments,
            ..
        }) => {
            if let [InclusionPathItem::Reef(_), include_path @ ..] = include_path.as_slice() {
                let span = include_path
                    .last()
                    .expect("at least one item")
                    .segment()
                    .start
                    ..include_path
                        .last()
                        .expect("at least one item")
                        .segment()
                        .end;
                add_import_tree(include_path, path, span, imports);
            }
            for arg in arguments {
                list_imports_expr(arg, path, imports);
            }
        }
        Expr::FunctionDeclaration(FunctionDeclaration {
            body: Some(body), ..
        }) => {
            list_imports_expr(body, path, imports);
        }
        _ => {}
    }
}

fn add_import(import: &ImportExpr, origin: &Path, span: SourceSegment, imports: &mut Vec<Import>) {
    match import {
        ImportExpr::Symbol(ImportedSymbol { path, .. })
        | ImportExpr::AllIn(path, _)
        | ImportExpr::List(ImportList { root: path, .. }) => {
            let [InclusionPathItem::Reef(_), rest @ ..] = path.as_slice() else {
                return;
            };
            add_import_tree(rest, origin, span, imports);
            // TODO: List items
        }
        ImportExpr::Environment(_) => {}
    }
}

fn add_import_tree(
    items: &[InclusionPathItem],
    origin: &Path,
    span: SourceSegment,
    imports: &mut Vec<Import>,
) {
    let mut path = PathBuf::new();
    for item in items {
        match item {
            InclusionPathItem::Symbol(ident) => {
                path.push(ident.value.as_str());
            }
            InclusionPathItem::Reef(_) => {
                return; // The path is not usable, the error will be reported later
            }
        }
    }
    imports.push(Import {
        path,
        origin: Some(SourceLocation {
            path: origin.to_path_buf(),
            span,
        }),
    });
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::MemoryFilesystem;
    use std::str::FromStr;

    fn import_multi<const N: usize>(sources: [(PathBuf, &str); N]) -> Vec<ModuleError> {
        let entrypoint = sources
            .first()
            .expect("at least one source")
            .0
            .display()
            .to_string();
        let mut reef = Reef::new(OsString::from("test"));
        let fs = MemoryFilesystem::from_iter(sources);
        super::import_multi(&mut reef, &fs, &entrypoint).errors
    }

    #[test]
    fn find_duplicate_functions() {
        let root = Root::from_str("fun a() = {}\nfun a() = {}").unwrap();
        let duplicates = hoist_exports(&root, &mut Vec::<Export>::new());
        assert_eq!(
            duplicates,
            [Duplicated {
                name: "a".to_owned(),
                first: 0..12,
                second: 13..25
            }]
        );
    }

    #[test]
    fn unknown_file_import() {
        let errors = import_multi([(PathBuf::from("main"), "use reef::foo")]);
        assert_eq!(
            errors.as_slice(),
            [ModuleError::Import {
                error: io::Error::new(io::ErrorKind::NotFound, "file not found"),
                cause: Some(SourceLocation {
                    path: PathBuf::from("main"),
                    span: 0..13
                })
            }]
        );
    }

    #[test]
    fn valid_file_import() {
        let errors = import_multi([
            (PathBuf::from("main"), "use test"),
            (PathBuf::from("test"), ""),
        ]);
        assert_eq!(errors, []);
    }

    #[test]
    fn valid_symbol_import_import() {
        let errors = import_multi([
            (PathBuf::from("main"), "use test::bar"),
            (PathBuf::from("test"), ""),
        ]);
        assert_eq!(errors, []);
    }
}
