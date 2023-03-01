#![allow(dead_code)]
#![deny(warnings)]

use lexer::token::Token;

use crate::ast::Expr;
use crate::parser::{Parser};

///! The parser crate contains the parser for the Moshell scripting language.
mod aspects;
pub mod ast;
mod cursor;
mod moves;
mod parser;
mod source;

pub fn parse(tokens: Vec<Token>) -> Vec<Expr> {
    Parser::new(tokens).parse()
}
