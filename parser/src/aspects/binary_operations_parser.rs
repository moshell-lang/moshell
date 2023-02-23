use crate::aspects::literal_parser::LiteralParser;
use crate::ast::Expr;
use crate::parser::{Parser, ParseResult};

pub(crate) trait BinaryOps {
    fn value_expression<'a>(&mut self) -> ParseResult<Expr<'a>>;
}

impl<'p> BinaryOps for Parser<'p> {
    fn value_expression<'a>(&mut self) -> ParseResult<Expr<'a>> {
        let left = self.argument();
        todo!()
    }
}