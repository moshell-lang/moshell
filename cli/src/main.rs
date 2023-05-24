#![allow(dead_code)]

use std::ops::Deref;
use std::process::exit;

use clap::Parser;
use miette::MietteHandlerOpts;

use crate::cli::{Cli, Configuration};
use crate::repl::repl;
use crate::runner::run;

mod cli;
mod repl;
mod report;
mod runner;
mod source_importer;

fn main() {
    let cli = Cli::parse();

    miette::set_hook(Box::new(|_| {
        Box::new(MietteHandlerOpts::new().tab_width(2).build())
    }))
        .expect("miette options setup");


    let config = Configuration::from(cli.clone());

    if let Some(source) = cli.source {
        let current_dir = std::env::current_dir().expect("Unable to retrieve current working directory.");

        let mut working_dir = cli.working_dir.unwrap_or(current_dir.clone());
        if !working_dir.is_absolute() {
            let mut absolute_wd = current_dir;
            absolute_wd.push(working_dir);
            working_dir = absolute_wd;
        }

        if !working_dir.exists() {
            eprintln!("working directory {} does not exists", working_dir.to_string_lossy().deref());
            exit(1);
        }

        exit(run(source, working_dir, config) as i32)
    }
    repl(config);
}

fn assert_simple(test: bool, msg: String) {
    if test {
        return;
    }
    eprintln!("{}", msg);
    exit(1)
}