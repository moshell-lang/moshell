use crate::cli::{use_pipeline, Cli};
use crate::pipeline::{FileImporter, PipelineStatus};
use analyzer::analyze;
use analyzer::name::Name;
use analyzer::reef::{Externals, Reef};
use analyzer::relations::SourceId;
use std::path::PathBuf;
use vm::VM;

pub fn build_std(externals: &mut Externals, vm: &mut VM, config: &Cli) -> PipelineStatus {
    let mut importer = FileImporter::new(PathBuf::from("lib/std"));

    let mut analyzer = analyze(Name::new("std"), &mut importer, externals);
    let diagnostics = analyzer.take_diagnostics();

    let status = use_pipeline(
        SourceId(0),
        &analyzer,
        externals,
        vm,
        diagnostics,
        &mut importer,
        config,
    );
    externals.register(Reef::new("std".to_string(), analyzer));

    status
}
