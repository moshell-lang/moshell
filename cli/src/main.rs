use crate::cli::{use_pipeline, Cli};
use crate::library::build_std;
use crate::pipeline::{Pipeline, PipelineStatus, REPLFilesystem};
use crate::repl::repl;
use crate::terminal::signal_hook;
use analyzer::{analyze_multi, Database, Reef};
use clap::{CommandFactory, Parser};
use compiler::CompilerState;
use nix::sys::signal;
use std::ffi::OsString;
use std::io;
use std::path::{Path, PathBuf};
use vm::VM;

mod cli;
mod complete;
mod disassemble;
mod library;
mod pipeline;
mod repl;
mod report;
mod terminal;

fn main() -> miette::Result<PipelineStatus> {
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

    let mut cli = Cli::parse();

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

    let vm = VM::new(
        cli.source
            .iter()
            .flat_map(|p| p.to_str())
            .map(ToOwned::to_owned)
            .chain(std::mem::take(&mut cli.program_arguments))
            .collect(),
    );
    let fs = REPLFilesystem::new(
        cli.source
            .as_deref()
            .and_then(Path::parent)
            .map(Path::to_path_buf)
            .unwrap_or_default(),
    );
    let mut pipeline = Pipeline {
        filesystem: fs,
        compiler_state: CompilerState::default(),
        vm,
    };
    let mut database = Database::default();
    build_std(&mut database, &mut pipeline);
    if let Some(source) = cli.source.take() {
        return Ok(run(
            source.file_name().map(PathBuf::from).unwrap_or_default(),
            &mut database,
            &mut pipeline,
            &cli,
        ));
    }
    repl(&cli, &mut database, &mut pipeline)
}

fn run(
    source: PathBuf,
    database: &mut Database,
    pipeline: &mut Pipeline,
    config: &Cli,
) -> PipelineStatus {
    let mut reef = Reef::new(OsString::from("foo"));
    let errors = analyze_multi(
        database,
        &mut reef,
        &pipeline.filesystem,
        &source.display().to_string(),
    );
    use_pipeline(database, &reef, pipeline, errors, config)
}
