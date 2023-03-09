#![allow(dead_code)]

#[derive(Debug, PartialEq)]
pub struct LexError {
    pub message: String,
    pub offset: usize,
}