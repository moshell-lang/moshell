#![allow(dead_code)]

use std::process::exit;

use clap::Parser;

use crate::cli::{Cli, CliConfiguration};
use crate::repl::repl;
use crate::runner::run;

mod cli;
mod disassemble;
mod render;
mod repl;
mod runner;
mod source_importer;

fn main() {
    let cli = Cli::parse();

    let config = CliConfiguration::from(&cli);

    if let Some(source) = cli.source {
        let source_dir = std::env::current_dir().expect("No working dir set");
        exit(!run(source, source_dir, config) as i32)
    }
    repl(config);
}
