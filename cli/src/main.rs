use crate::cli::{use_pipeline, Cli};
use crate::pipeline::RealFilesystem;
use crate::repl::repl;
use crate::terminal::signal_hook;
use ::cli::pipeline::PipelineStatus;
use analyzer::{analyze_multi, Database, Reef};
use clap::{CommandFactory, Parser};
use nix::sys::signal;
use std::ffi::OsString;
use std::io;
use std::path::PathBuf;
use vm::VM;

mod cli;
mod complete;
mod disassemble;
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

    let mut vm = VM::new(
        cli.source
            .iter()
            .flat_map(|p| p.to_str())
            .map(ToOwned::to_owned)
            .chain(std::mem::take(&mut cli.program_arguments))
            .collect(),
    );
    let fs = RealFilesystem {
        root: PathBuf::new(),
    };
    let mut database = Database::default();
    if let Some(source) = cli.source.take() {
        return Ok(run(source, &mut database, &fs, &mut vm, &cli));
    }
    repl(&cli, &mut database, &fs, &mut vm)
}

fn run(
    source: PathBuf,
    database: &mut Database,
    fs: &RealFilesystem,
    vm: &mut VM,
    config: &Cli,
) -> PipelineStatus {
    let mut reef = Reef::new(OsString::from("foo"));
    let errors = analyze_multi(database, &mut reef, fs, &source.display().to_string());
    use_pipeline(database, &reef, fs, vm, errors, config)
}
