use crate::cli::{use_pipeline, Cli};
use crate::pipeline::{Pipeline, PipelineStatus, REPLFilesystem};
use analyzer::{analyze_multi, freeze_exports, Database, Reef};
use cli::project_dir;
use std::ffi::OsString;
use std::path::{Path, PathBuf};

pub(crate) fn build_std(database: &mut Database, pipeline: &mut Pipeline) {
    let std_file = find_std();
    let mut reef = Reef::new(OsString::from("std"));
    let old_fs = std::mem::replace(&mut pipeline.filesystem, REPLFilesystem::new(std_file));
    let errors = analyze_multi(database, &mut reef, &pipeline.filesystem, "std");
    match use_pipeline(database, &reef, pipeline, errors, &Cli::default()) {
        PipelineStatus::Success => {}
        PipelineStatus::IoError => panic!(
            "Unable to find the standard library, check the MOSHELL_STD environment variable"
        ),
        status => panic!("Could not build std: {:?}", status),
    }
    pipeline.filesystem = old_fs;
    freeze_exports(database, reef);
}

fn find_std() -> PathBuf {
    if let Ok(path) = std::env::var("MOSHELL_STD") {
        return PathBuf::from(path);
    }

    // let mut dir = std::env::current_dir().expect("Could not get current directory");
    // dir.push("lib");
    // dir.push("std.msh");
    // if dir.exists() {
    //     dir.pop();
    //     return dir;
    // }

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
