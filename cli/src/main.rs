use ::std::ffi::OsStr;
use ::std::path::PathBuf;
use analyzer::Analyzer;
use clap::Parser;
use miette::{IntoDiagnostic, MietteHandlerOpts, WrapErr};

use analyzer::name::Name;
use analyzer::reef::Externals;
use analyzer::relations::SourceId;
use compiler::captures::ReefsCaptures;
use vm::VM;

use crate::cli::{use_pipeline, Cli};
use crate::pipeline::{FileImporter, PipelineStatus, SourcesCache};
use crate::repl::prompt;
use crate::std::build_std;

mod cli;
mod disassemble;
mod pipeline;
mod repl;
mod report;
mod std;

fn main() -> Result<PipelineStatus, miette::Error> {
    #[cfg(unix)]
    {
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

    if let Some(source) = &cli.source {
        return run(source, &cli);
    }
    let current_dir = ::std::env::current_dir()
        .into_diagnostic()
        .context("Could not locate working directory")?;

    Ok(prompt(current_dir, &cli))
}

fn run(source: &PathBuf, cli: &Cli) -> Result<PipelineStatus, miette::Error> {
    let name = Name::new(
        source
            .file_name()
            .and_then(OsStr::to_str)
            .expect("Incompatible filename"),
    );

    let mut sources = SourcesCache::default();

    let mut externals = Externals::default();
    let mut vm = VM::new();
    let mut reefs_captures = ReefsCaptures::default();
    build_std(
        &mut externals,
        &mut vm,
        &mut sources,
        &mut reefs_captures,
        cli,
    );

    let folder_path = {
        let mut path = source.clone();
        path.pop();
        path
    };

    let mut importer = FileImporter::new(&mut sources, folder_path);

    importer.add_redirection(name.clone(), source.clone());

    let mut analyzer = Analyzer::new();

    analyzer.process(name.clone(), &mut importer, &externals);

    let diagnostics = analyzer.take_diagnostics();
    return Ok(use_pipeline(
        SourceId(0),
        &analyzer,
        &externals,
        &mut vm,
        &mut reefs_captures,
        diagnostics,
        &mut importer,
        cli,
    ));
}
