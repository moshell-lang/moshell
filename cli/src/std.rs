use crate::cli::{use_pipeline, Cli};
use crate::pipeline::{ErrorReporter, PipelineStatus, SourcesCache};
use analyzer::analyze;
use analyzer::name::Name;
use analyzer::reef::{Externals, Reef};
use analyzer::relations::SourceId;
use directories::ProjectDirs;
use std::path::PathBuf;
use vm::VM;

pub fn build_std(externals: &mut Externals, vm: &mut VM, sources: &mut SourcesCache, config: &Cli) {
    let std_file = find_std();
    sources.register(std_file);
    let importer = sources.last_mut();

    let name = Name::new("std");
    let mut analyzer = analyze(name.clone(), importer, externals);
    let diagnostics = analyzer.take_diagnostics();

    let status = use_pipeline(
        &name,
        SourceId(0),
        &analyzer,
        externals,
        vm,
        diagnostics,
        importer.take_errors(),
        sources,
        config,
    );
    externals.register(Reef::new("std".to_string(), analyzer));

    match status {
        PipelineStatus::Success => {}
        PipelineStatus::IoError => panic!(
            "Unable to find the standard library, check the MOSHELL_STD environment variable"
        ),
        _ => panic!("std build did not succeed"),
    }
}

fn find_std() -> PathBuf {
    if let Ok(path) = std::env::var("MOSHELL_STD") {
        return PathBuf::from(path);
    }

    if let Some(proj_dirs) = ProjectDirs::from("", "", "moshell") {
        let lib = proj_dirs.data_dir().join("lib");
        return if lib.exists() {
            lib
        } else {
            let mut dir = std::env::current_dir().expect("Could not get current directory");
            dir.push("lib");
            dir
        };
    }
    panic!("could not find stdlib path")
}
