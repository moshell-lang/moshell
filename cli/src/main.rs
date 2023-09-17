use crate::cli::{use_pipeline, Cli};
use crate::pipeline::{ErrorReporter, FileImporter, PipelineStatus, SourcesCache};
use crate::repl::repl;
use crate::std::build_std;
use ::std::ffi::OsStr;
use ::std::path::Path;
use analyzer::name::Name;
use analyzer::reef::Externals;
use analyzer::relations::SourceId;
use analyzer::Analyzer;
use clap::Parser;
use miette::{Context, IntoDiagnostic, MietteHandlerOpts};
use vm::VM;

mod cli;
mod disassemble;
mod pipeline;
mod repl;
mod report;
mod std;

fn main() -> Result<PipelineStatus, miette::Error> {
    if cfg!(unix) && !cfg!(miri) {
        // Override Rust's default `SIGPIPE` signal handler that ignores the signal.
        // `println!` will no longer panic since the process will be killed before
        // trying to write something. Restoring this Unix behavior is also important
        // because this process will be forked to invoke externals command, meaning
        // that the children processes will inherit from the ignored `SIGPIPE` signal,
        // causing repercussions on coreutils commands for instance, that usually
        // expect to be killed if a pipe is closed.
        unsafe {
            // SAFETY: It is an Unix environment.
            libc::signal(libc::SIGPIPE, libc::SIG_DFL);
        }
    }

    let cli = Cli::parse();

    miette::set_hook(Box::new(|_| {
        Box::new(MietteHandlerOpts::new().tab_width(2).build())
    }))
    .expect("miette options setup");

    let mut externals = Externals::default();
    let mut sources = SourcesCache::default();
    let mut vm = VM::new();

    build_std(&mut externals, &mut vm, &mut sources, &cli);

    if let Some(source) = &cli.source {
        return run(source, &cli, sources, externals, vm);
    }
    let current_dir = ::std::env::current_dir()
        .into_diagnostic()
        .context("Could not locate working directory")?;

    Ok(repl(current_dir, &cli, sources, externals, vm))
}

fn run(
    source: &Path,
    cli: &Cli,
    mut sources: SourcesCache,
    externals: Externals,
    mut vm: VM,
) -> Result<PipelineStatus, miette::Error> {
    let name = Name::new(
        source
            .file_name()
            .and_then(OsStr::to_str)
            .expect("Incompatible filename"),
    );

    let folder_path = {
        let mut path = source.to_path_buf();
        path.pop();
        path
    };

    let mut importer = FileImporter::new(folder_path);
    importer.add_redirection(name.clone(), source.to_path_buf());

    let mut analyzer = Analyzer::new();
    analyzer.process(name.clone(), &mut importer, &externals);

    let diagnostics = analyzer.take_diagnostics();
    let errors = importer.take_errors();
    sources.set(externals.current, importer.take_sources());

    Ok(use_pipeline(
        &name,
        SourceId(0),
        &analyzer,
        &externals,
        &mut vm,
        diagnostics,
        errors,
        &sources,
        cli,
    ))
}
