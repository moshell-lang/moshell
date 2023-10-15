use crate::cli::{use_pipeline, Cli};
use crate::pipeline::{ErrorReporter, PipelineStatus, SourcesCache};
use analyzer::analyze;
use analyzer::name::Name;
use analyzer::reef::{Externals, Reef};
use analyzer::relations::SourceId;
use cli::project_dir;
use compiler::externals::CompilerExternals;
use std::path::{Path, PathBuf};
use vm::VM;

pub fn build_std(
    externals: &mut Externals,
    compiler_externals: &mut CompilerExternals,
    vm: &mut VM,
    sources: &mut SourcesCache,
    config: &Cli,
) {
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
        compiler_externals,
        vm,
        diagnostics,
        importer.take_errors(),
        sources,
        config,
    );

    match status {
        Ok(compiler_reef) => {
            externals.register(Reef::new("std".to_string(), analyzer));
            compiler_externals.register(compiler_reef)
        }
        Err(PipelineStatus::IoError) => panic!(
            "Unable to find the standard library, check the MOSHELL_STD environment variable"
        ),
        _ => panic!("std build did not succeed"),
    }
}

fn find_std() -> PathBuf {
    if let Ok(path) = std::env::var("MOSHELL_STD") {
        return PathBuf::from(path);
    }

    let mut dir = std::env::current_dir().expect("Could not get current directory");
    dir.push("lib");
    dir.push("std.msh");
    if dir.exists() {
        dir.pop();
        return dir;
    }

    if let Some(proj_dirs) = project_dir() {
        let lib = proj_dirs.data_dir().join("lib");
        if lib.exists() {
            return lib;
        }
    }

    #[cfg(unix)]
    {
        for path in ["/usr/local/share/moshell/lib", "/usr/share/moshell/lib"] {
            let path = Path::new(path);
            if path.exists() {
                return path.to_path_buf();
            }
        }
    }
    panic!("Could not determine a valid std emplacement. Please provide a valid stdlib path under a MOSHELL_STD=<path> env variable.")
}
