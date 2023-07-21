use std::env;

fn main() {
    let mut config = cmake::Config::new("../vm");
    if env::var("CARGO_TERM_COLOR").as_deref() == Ok("always") {
        config.env("CMAKE_COLOR_DIAGNOSTICS", "ON");
    }

    // Statically link to the VM library.
    let dst = config.build();
    println!("cargo:rustc-link-search=native={}", dst.display());
    println!("cargo:rustc-link-lib=static=vm");

    // Link to the C++ standard library.
    let target_os = env::var("CARGO_CFG_TARGET_OS").unwrap();
    let target_env = env::var("CARGO_CFG_TARGET_ENV").unwrap();
    match (target_os.as_str(), target_env.as_str()) {
        ("linux", _) | ("windows", "gnu") => {
            println!("cargo:rustc-link-lib=dylib=stdc++");
        }
        ("macos" | "ios", _) => {
            println!("cargo:rustc-link-lib=dylib=c++");
        }
        (_, _) => {
            println!("cargo:warning=moshell: unconditional linking of C++ runtime on {target_os}");
        }
    }

    // Hook the build script to re-run if the VM library changes.
    println!("cargo:rerun-if-changed=../vm/CMakeLists.txt");
    println!("cargo:rerun-if-changed=../vm/src");
}
