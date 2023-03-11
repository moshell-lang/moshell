#![allow(dead_code)]
#![deny(warnings)]

use std::fmt::Debug;
use ::context::poller::Poller;
use crate::err::ParseReport;
use lexer::token::Token;
use crate::context::ParserContext;

use crate::parser::Parser;

///! The parser crate contains the parser for the Moshell scripting language.
mod aspects;
pub mod ast;
mod cursor;
pub mod err;
mod moves;
mod parser;
mod context;

pub fn parse<'a, P: Poller<'a, Token<'a>> + Debug>(ctx: &'a mut ParserContext<'a, P>) -> ParseReport {
    Parser::new(ctx).parse()
}
