use lang_tester::LangTester;
use std::env::current_dir;
use std::{fs::read_to_string, process::Command};
use tempfile::TempDir;

const COMMENT_PREFIX: &str = "//";

fn main() {
    let tempdir = TempDir::new().unwrap();
    LangTester::new()
        .test_dir("lang_tests")
        .test_file_filter(|p| p.extension().unwrap().to_str().unwrap() == "msh")
        // Extract the first sequence of commented line(s) as the tests.
        .test_extract(|p| {
            read_to_string(p)
                .unwrap()
                .lines()
                // Skip non-commented lines at the start of the file.
                .skip_while(|l| !l.starts_with(COMMENT_PREFIX))
                // Extract consecutive commented lines.
                .take_while(|l| l.starts_with(COMMENT_PREFIX))
                .map(|l| &l[COMMENT_PREFIX.len()..])
                .collect::<Vec<_>>()
                .join("\n")
        })
        .test_cmds(move |p| {
            let mut runtime = Command::new(env!("CARGO_BIN_EXE_cli"));
            let stdlib = current_dir().unwrap().with_file_name("lib");
            runtime
                .env("MOSHELL_STD", stdlib)
                .args(&["-s", p.to_str().unwrap()])
                .current_dir(tempdir.path());
            vec![("Run", runtime)]
        })
        .run();
}
