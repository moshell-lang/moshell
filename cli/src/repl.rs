use std::collections::HashMap;
use std::io::stderr;
use std::process::exit;

use rustyline::config::Configurer;
use rustyline::error::ReadlineError;
use rustyline::history::DefaultHistory;
use rustyline::{DefaultEditor, Editor};

use analyzer::diagnostic::Diagnostic;
use analyzer::engine::Engine;
use analyzer::importer::ASTImporter;
use analyzer::imports::Imports;
use analyzer::name::Name;
use analyzer::relations::{Relations, SourceId};
use analyzer::steps::typing::apply_types;
use ast::group::Block;
use ast::Expr;
use compiler::compile;
use context::source::{OwnedSource, Source, SourceSegmentHolder};
use lexer::lexer::lex;
use parser::parse;

use crate::cli::{display_exprs, display_tokens, execute, Configuration};
use crate::disassemble::display_bytecode;
use crate::render::diagnostic::render_diagnostic;
use crate::render::parse_error::render_parse_error;
use crate::render::SourcesCache;

type REPLEditor = Editor<(), DefaultHistory>;

#[derive(Default)]
struct REPLImporter<'a> {
    sources: Vec<OwnedSource>,
    imported_modules: HashMap<Name, Expr<'a>>,
}

impl<'a, const N: usize> From<[(Name, Expr<'a>); N]> for REPLImporter<'a> {
    fn from(value: [(Name, Expr<'a>); N]) -> Self {
        Self {
            imported_modules: HashMap::from(value),
            sources: Vec::new(),
        }
    }
}

impl<'a> ASTImporter<'a> for REPLImporter<'a> {
    fn import(&mut self, name: &Name) -> Option<Expr<'a>> {
        self.imported_modules.get(name).cloned()
    }
}

impl<'a> REPLImporter<'a> {
    pub fn take_source(&mut self, source: OwnedSource) -> Source<'a> {
        self.sources.push(source);
        let src = self.sources[self.sources.len() - 1].as_source();
        unsafe {
            // SAFETY: The sources will never be removed from the self.sources vector as the REPLImporter's
            // role is to be the owner of the user's sources.
            // The reference behind Box does not change and is valid for the lifetime of the importer.
            std::mem::transmute::<Source, Source<'a>>(src)
        }
    }
}

/// Indefinitely prompts a new expression to the stdin,
/// displaying back the errors if any and the formed AST
pub fn repl(config: Configuration) {
    let mut editor: REPLEditor =
        DefaultEditor::new().expect("unable to instantiate terminal editor");
    editor.set_history_ignore_dups(true).unwrap();
    editor.set_history_ignore_space(true);

    let mut relations = Relations::default();
    let mut imports = Imports::default();
    let mut importer = REPLImporter::default();

    loop {
        let mut engine = Engine::default();

        let source = parse_input(&mut editor);
        handle_source(
            source,
            &config,
            &mut engine,
            &mut importer,
            &mut imports,
            &mut relations,
        );
    }
}

/// Parses stdin until the user's input forms a source code with no unclosed delimiters
/// and return the source.
fn parse_input(editor: &mut REPLEditor) -> OwnedSource {
    let mut content = String::new();
    let mut prompt_prefix = "=> ".to_string();
    let mut indent_prefix = "";
    loop {
        let line = editor.readline_with_initial(&prompt_prefix, (indent_prefix, ""));
        let mut line = match line {
            Ok(line) => line,
            Err(ReadlineError::Eof | ReadlineError::Interrupted) => exit(0),
            e => e.expect("error when reading next line from editor"),
        };
        // Re-add the newline stripped by readline
        line.push('\n');

        // Take the indent prefix from the buffer since the current line will be dropped at the end of the iteration
        let trimmed_len = line.trim_start().len();
        let content_start = content.len();
        content.push_str(&line);
        indent_prefix = &content[content_start..content.len() - trimmed_len];

        if line.ends_with('\\') {
            prompt_prefix = "-> ".to_string();
            continue;
        }

        let source = Source::new(&content, "stdin");
        let report = parse(source);
        if let Some(delimiter) = report.unclosed_delimiter {
            prompt_prefix = format!(
                "{}> ",
                delimiter.str().expect("Invalid delimiter passed to stack")
            );
            continue; // Silently ignore incomplete input
        }

        editor.add_history_entry(source.source.to_string()).ok();

        return OwnedSource::new(source.source.to_string(), source.name.to_string());
    }
}

fn display_diagnostics(diagnostics: Vec<Diagnostic>, source: Source) {
    let mut cache = SourcesCache::new(|_| source);
    for diagnostic in diagnostics {
        render_diagnostic(diagnostic, &mut cache, &mut stderr())
            .expect("IO errors when reporting diagnostic");
    }
}

/// Parses and display errors / diagnostics coming from the given source.
/// Returning true if the source had at least one error or diagnostic.
fn handle_source<'e>(
    source: OwnedSource,
    config: &Configuration,
    engine: &mut Engine<'e>,
    importer: &mut REPLImporter<'e>,
    imports: &mut Imports,
    relations: &mut Relations,
) {
    let source = importer.take_source(source);
    let name = Name::new(source.name);

    if config.lexer_visualisation {
        display_tokens(lex(source.source))
    }

    let report = parse(source);

    let errors: Vec<_> = report.errors;

    if !errors.is_empty() {
        for error in errors {
            render_parse_error(source, error, &mut stderr())
                .expect("IO error when reporting diagnostics");
        }
        return;
    }

    if config.parser_visualization {
        display_exprs(&report.expr)
    }

    let expr = Expr::Block(Block {
        expressions: report.expr,
        segment: source.segment(),
    });

    importer.imported_modules.insert(name.clone(), expr);

    let mut diagnostics =
        analyzer::make_full_resolution(name, importer, engine, relations, imports);

    if !diagnostics.is_empty() {
        display_diagnostics(diagnostics, source);
        return;
    }

    let typed_engine = apply_types(engine, relations, &mut diagnostics);
    if !diagnostics.is_empty() {
        display_diagnostics(diagnostics, source);
        return;
    }

    let mut bytecode = Vec::new();
    let root_expr = &typed_engine.get_user(SourceId(0)).unwrap().expression;
    compile(root_expr, &mut bytecode).unwrap();

    if config.bytecode_visualisation {
        display_bytecode(&bytecode);
    }

    execute(bytecode);
}
