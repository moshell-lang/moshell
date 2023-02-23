use crate::aspects::literal_parser::LiteralParser;
//use crate::aspects::literal_parser::LiteralParser;
use crate::ast::Expr;
use crate::parser::{Parser, ParseResult};

pub(crate) trait BinaryOps<'p> {
    fn value_expression(&mut self) -> ParseResult<Expr<'p>>;
}

impl<'p> BinaryOps<'p> for Parser<'p> {
    fn value_expression(&mut self) -> ParseResult<Expr<'p>> {
        self.argument()
    }
}