use std::string::ParseError;

trait CallParser<'a> {
    fn call(&mut self) -> Result<Expr<'a>, ParseError>;
}