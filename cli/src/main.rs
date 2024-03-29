use crate::cli::{use_pipeline, Cli};
use crate::pipeline::{ErrorReporter, PipelineStatus, SourcesCache};
use crate::repl::{code, repl};
use crate::std::build_std;
use crate::terminal::signal_hook;
use ::std::ffi::OsStr;
use ::std::io;
use ::std::path::Path;
use analyzer::name::Name;
use analyzer::reef::Externals;
use analyzer::relations::SourceId;
use analyzer::Analyzer;
use clap::{CommandFactory, Parser};
use compiler::externals::CompilerExternals;
use miette::{Context, IntoDiagnostic, MietteHandlerOpts};
use nix::sys::signal;
use vm::VM;

mod cli;
mod complete;
mod disassemble;
mod pipeline;
mod repl;
mod report;
mod std;
mod terminal;

fn main() -> Result<PipelineStatus, miette::Error> {
    if cfg!(unix) && !cfg!(miri) {
        // Override Rust's default `SIGPIPE` signal handler that ignores the signal.
        // `println!` will no longer panic since the process will be killed before
        // trying to write something. Restoring this Unix behavior is also important
        // because this process will be forked to invoke externals command, meaning
        // that the children processes will inherit from the ignored `SIGPIPE` signal,
        // causing repercussions on coreutils commands for instance, that usually
        // expect to be killed if a pipe is closed.
        signal_hook(signal::Signal::SIGPIPE, signal::SigHandler::SigDfl);
    }

    let cli = Cli::parse();

    if let Some(generator) = cli.completions {
        let mut cmd = Cli::command();
        eprintln!("Generating completion file for {generator}...");
        clap_complete::generate(
            generator,
            &mut cmd,
            env!("CARGO_BIN_NAME"),
            &mut io::stdout(),
        );
        return Ok(PipelineStatus::Success);
    }

    miette::set_hook(Box::new(|_| {
        Box::new(MietteHandlerOpts::new().tab_width(2).build())
    }))
    .expect("miette options setup");

    let mut externals = Externals::default();
    let mut compiler_externals = CompilerExternals::default();
    let mut sources = SourcesCache::default();
    let mut vm = VM::new(
        cli.source
            .iter()
            .flat_map(|p| p.to_str())
            .map(ToOwned::to_owned)
            .chain(cli.program_arguments.clone())
            .collect(),
    );

    let current_dir = ::std::env::current_dir()
        .into_diagnostic()
        .context("Could not locate working directory")?;

    build_std(
        &mut externals,
        &mut compiler_externals,
        &mut vm,
        &mut sources,
        &cli,
    );

    if let Some(source) = &cli.source {
        return run(source, &cli, sources, externals, compiler_externals, vm);
    }
    if let Some(source) = cli.code.clone() {
        return code(
            source,
            current_dir,
            &cli,
            sources,
            externals,
            compiler_externals,
            vm,
        );
    }

    repl(
        current_dir,
        &cli,
        sources,
        externals,
        compiler_externals,
        vm,
    )
}

fn run(
    source: &Path,
    cli: &Cli,
    mut sources: SourcesCache,
    externals: Externals,
    mut compiler_externals: CompilerExternals,
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

    sources.register(folder_path);
    let importer = sources.last_mut();
    importer.add_redirection(name.clone(), source.to_path_buf());

    let mut analyzer = Analyzer::new();
    analyzer.process(name.clone(), importer, &externals);

    let diagnostics = analyzer.take_diagnostics();
    let errors = importer.take_errors();

    Ok(use_pipeline(
        &name,
        SourceId(0),
        &analyzer,
        &externals,
        &mut compiler_externals,
        &mut vm,
        diagnostics,
        errors,
        &sources,
        cli,
    ))
}
