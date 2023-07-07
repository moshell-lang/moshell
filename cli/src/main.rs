#![allow(dead_code)]

use std::io;
use std::process::exit;

use clap::Parser;
use miette::MietteHandlerOpts;

use analyzer::name::Name;

use crate::cli::{resolve_and_execute, Cli};
use crate::pipeline::FileImporter;
use crate::repl::prompt;

mod cli;
mod disassemble;
mod pipeline;
mod repl;
mod report;

fn main() -> io::Result<()> {
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
        let name = Name::new(
            source
                .file_name()
                .and_then(|name| name.to_str())
                .expect("Incompatible filename"),
        );
        let mut importer = FileImporter::new({
            let mut root = source.clone();
            root.pop();
            root
        });
        importer.add_redirection(name.clone(), source.clone());
        let has_error = resolve_and_execute(name, &mut importer, &cli);
        exit(i32::from(has_error))
    }
    let importer = FileImporter::new(std::env::current_dir()?);
    prompt(importer, &cli);
    Ok(())
}
