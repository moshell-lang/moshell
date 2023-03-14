/*use core::slice::SlicePattern;
use ast::function::FunctionDeclaration;
use lexer::token::TokenType;
use lexer::token::TokenType::{Fun, Identifier};
use crate::aspects::literal::LiteralAspect;
use crate::err::ParseErrorKind;

use crate::moves::{blank, blanks, like, MoveOperations, not, of_type};
use crate::parser::{Parser, ParseResult};

pub trait FunctionDeclarationAspect<'a> {
    fn parse_function_declaration(&mut self) -> ParseResult<FunctionDeclaration<'a>>;
}

impl<'a> FunctionDeclarationAspect<'a> for Parser<'a> {
    fn parse_function_declaration(&mut self) -> ParseResult<FunctionDeclaration<'a>> {
        self.cursor.force(of_type(Fun), "expected 'fun' keyword at start of function declaration.")?;

        //consume blanks
        self.cursor.advance(blanks());


        let token_name = self.parse_fn_declaration_name()?;
        let tparams = 
        todo!()
    }
}

impl<'a> Parser<'a> {
    fn parse_fn_declaration_name(&mut self) -> ParseResult<&str> {
        self.cursor
            .advance(like(TokenType::is_valid_function_name))
            .ok_or_else(|| {
                let wrong_name_slice = self.cursor.select(not(blank().or(like(TokenType::is_ponctuation)))).as_slice();
                self.mk_parse_error(
                    "function name is invalid.",
                    wrong_name_slice,
                    ParseErrorKind::Unexpected,
                )
            }).map(|t| t.value)
    }
}

*/