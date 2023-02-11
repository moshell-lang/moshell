mod linter;
mod token;
mod test;

use std::fs;
use logos::Logos;
use crate::token::TokenType;

fn main() {
    println!("TEST");
    let contents = fs::read_to_string("sample.msh")
        .expect("Cannot open file sample.msh");
    println!("TEST2");

    let mut lexer = TokenType::lexer(&contents);
    println!("TEST3");

    while let Some(token) = lexer.next() {
        println!("{:?} {:?}", token, lexer.slice());
    }
}