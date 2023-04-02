#![allow(dead_code)]

pub mod environment;
pub mod lang_types;
pub mod local;
pub mod types;


#[derive(Debug, PartialEq)]
pub struct Diagnostic {
    pub message: String,
}
