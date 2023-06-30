use std::io;
use std::io::BufRead;
use std::io::Write;

use analyzer::importer::{ASTImporter, ImportError, Imported};
use analyzer::name::Name;
use context::source::{ContentId, OwnedSource, Source};
use parser::parse;

use crate::cli::resolve_and_execute;
use crate::pipeline::{ErrorReporter, FileImportError, FileImporter};
use crate::report::print_flush;

/// A wrapper around a [`FileImporter`] that short-circuits the file system for
/// a single source.
struct InputImporter {
    /// The next source to always import, independently of the name.
    last: Option<OwnedSource>,

    /// The inner file importer.
    files: FileImporter,
}

impl InputImporter {
    fn new(file_importer: FileImporter) -> Self {
        Self {
            last: None,
            files: file_importer,
        }
    }

    fn reserve(&mut self, source: OwnedSource) {
        self.last = Some(source);
    }
}

impl<'a> ASTImporter<'a> for InputImporter {
    fn import(&mut self, name: &Name) -> Result<Option<Imported<'a>>, ImportError> {
        match self.last.take() {
            Some(last) => self.files.insert(last),
            None => self.files.import(name),
        }
    }
}

impl ErrorReporter for InputImporter {
    fn take_errors(&mut self) -> Vec<FileImportError> {
        self.files.take_errors()
    }

    fn get_source(&self, id: ContentId) -> Option<Source> {
        self.files.get_source(id)
    }
}

/// Indefinitely prompts a new expression from stdin and executes it.
pub fn prompt(importer: FileImporter) {
    let mut importer = InputImporter::new(importer);
    while let Some(source) = parse_input() {
        let name = Name::new(&source.name);
        importer.reserve(source);
        resolve_and_execute(name, &mut importer);
    }
}

/// Parses stdin until the user's input forms a source code with no unclosed delimiters
/// and return the source.
fn parse_input() -> Option<OwnedSource> {
    let stdin = io::stdin();
    let lines = stdin.lock().lines();
    let mut content = String::new();
    print_flush!("=> ");
    for line in lines {
        let line = line.expect("couldn't read line from stdin");
        content.push_str(&line);
        if line.ends_with('\\') {
            content.push('\n');
            print_flush!("-> ");
            continue;
        }

        let source = Source::new(&content, "stdin");
        let report = parse(source);
        if !report.stack_ended {
            content.push('\n');
            print_flush!("-> ");
            continue; // Silently ignore incomplete input
        }

        return Some(OwnedSource::new(
            source.source.to_string(),
            source.name.to_string(),
        ));
    }
    None
}
