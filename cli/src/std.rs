use crate::cli::{use_pipeline, Cli};
use crate::pipeline::{ErrorReporter, FileImporter, PipelineStatus, SourcesCache};
use analyzer::analyze;
use analyzer::name::Name;
use analyzer::reef::{Externals, Reef};
use analyzer::relations::SourceId;
use directories::ProjectDirs;
use std::path::PathBuf;
use vm::VM;

pub fn build_std(externals: &mut Externals, vm: &mut VM, sources: &mut SourcesCache, config: &Cli) {
    let std_file = find_std();
    let mut importer = FileImporter::new(sources.len(), std_file);

    let name = Name::new("std");
    let mut analyzer = analyze(name.clone(), &mut importer, externals);
    let diagnostics = analyzer.take_diagnostics();

    sources.extend(importer.take_sources());

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

    if status != PipelineStatus::Success {
        panic!("std build did not succeed")
    }
}

fn find_std() -> PathBuf {
    if let Ok(path) = std::env::var("MOSHELL_STD") {
        return PathBuf::from(path);
    }

    if let Some(proj_dirs) = ProjectDirs::from("", "", "moshell") {
        return proj_dirs.data_dir().join("lib").to_path_buf();
    }
    panic!("could not find stdlib path")
}
