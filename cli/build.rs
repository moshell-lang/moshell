use cmake::Config;

fn main() {
    let dst = Config::new("../vm").build();
    println!("cargo:rustc-link-search=native={}", dst.display());
    println!("cargo:rustc-link-lib=static=vm");
    println!("cargo:rustc-link-lib=dylib=stdc++");
}
