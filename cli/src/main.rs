#![allow(dead_code)]

use std::ops::Deref;
use std::process::exit;
use ariadne::{Color, StdoutFmt};

use clap::Parser;

use crate::cli::{Cli, Configuration};
use crate::repl::repl;
use crate::runner::run;

mod cli;
mod formatted_diagnostic;
mod formatted_parse_error;
mod repl;
mod runner;
mod source_importer;

fn main() {
    let cli = Cli::parse();

    let config = Configuration::from(cli.clone());

    if let Some(source) = cli.source {
        let current_dir =
            std::env::current_dir().expect("Unable to retrieve current working directory.");

        let mut working_dir = cli.working_dir.unwrap_or(current_dir.clone());
        if !working_dir.is_absolute() {
            let mut absolute_wd = current_dir;
            absolute_wd.push(working_dir);
            working_dir = absolute_wd;
        }

        if !working_dir.exists() {
            eprintln!(
                "working directory {} does not exists",
                working_dir.to_string_lossy().deref()
            );
            exit(1);
        }

        exit(run(source, working_dir, config) as i32)
    }
    repl(config);
}

/// https://github.com/zesterer/ariadne/issues/51
pub fn fix_ariadne_report(report_str: String) -> String {
    let mut fixed_report = String::new();

    let blank_line = format!(" {}  ", "  │".fg(Color::Fixed(240)));
    let mut did_skip_lines = false;
    for line in report_str.lines() {

        if !line.starts_with(&blank_line) {
            if did_skip_lines {
                did_skip_lines = false;
                fixed_report.push_str(&blank_line);
                fixed_report.push('\n');
            }
            fixed_report.push_str(line);
            fixed_report.push('\n')
        } else {
            did_skip_lines = true;
        }
    }

    fixed_report
}
