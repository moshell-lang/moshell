use ast::function::{FunctionDeclaration, FunctionParameter};
use ast::function::FunctionParameter::Variadic;
use ast::r#type::Type;
use lexer::token::TokenType;
use lexer::token::TokenType::{Arrow, Comma, Equal, Fun, RoundedLeftBracket, RoundedRightBracket, Vararg};
use crate::aspects::r#type::TypeAspect;
use crate::aspects::var_declaration::VarDeclarationAspect;
use crate::err::ParseErrorKind;

use crate::moves::{blank, blanks, eod, like, MoveOperations, not, of_type};
use crate::parser::{Parser, ParseResult};

pub trait FunctionDeclarationAspect<'a> {
    fn parse_function_declaration(&mut self) -> ParseResult<FunctionDeclaration<'a>>;
}

impl<'a> FunctionDeclarationAspect<'a> for Parser<'a> {
    fn parse_function_declaration(&mut self) -> ParseResult<FunctionDeclaration<'a>> {
        self.cursor.force(of_type(Fun), "expected 'fun' keyword at start of function declaration.")?;

        //consume blanks
        self.cursor.advance(blanks());


        let name = self.parse_fn_declaration_name()?;
        let tparams = self.parse_type_parameter_list()?;
        let params = self.parse_fn_parameter_list()?;
        let rtype = self.parse_fn_return_type()?;
        let body = self.cursor
            .force(blanks().then(of_type(Equal)), "expected '='")
            .and_then(|_| self.statement())?;

        Ok(FunctionDeclaration {
            name,
            type_parameters: tparams,
            parameters: params,
            return_type: rtype,
            body,
        })
    }
}

impl<'a> Parser<'a> {


    fn parse_fn_return_type(&mut self) -> ParseResult<Option<Type<'a>>> {
        if self.cursor.advance(blanks().then(of_type(Arrow))).is_none() {
            return Ok(None)
        }
        self.cursor.advance(blanks()); // consume blanks
        self.parse_type().map(Some)
    }

    fn parse_fn_parameter(&mut self) -> ParseResult<FunctionParameter<'a>> {
        self.cursor.advance(blanks()); //consume blanks

        self.parse_typed_var()
            .map(FunctionParameter::Named)
            .or_else(|_| self.parse_type()
                .and_then(|t|
                    self.cursor.force(
                        blanks().then(of_type(Vararg)),
                        "expected vararg declaration or ",
                    ).and(Ok(Variadic(Some(t))))
                )
            )
    }

    fn parse_fn_parameter_list(&mut self) -> ParseResult<Vec<FunctionParameter<'a>>> {
        self.cursor.force(of_type(RoundedLeftBracket), "expected start of parameter list ('(')")?;

        let mut params = Vec::new();
        while self.cursor.advance(blanks().then(eod())).is_some() {
            let param = self.parse_fn_parameter()?;
            params.push(param);
            self.cursor.force(
                blanks().then(of_type(Comma)),
                "expected ','",
            )?;
        }

        self.cursor.advance(blanks()); //consume blanks
        self.expect_delimiter(RoundedRightBracket)?;

        Ok(params)
    }

    fn parse_fn_declaration_name(&mut self) -> ParseResult<&'a str> {
        self.cursor
            .advance(like(TokenType::is_valid_function_name))
            .ok_or_else(|| {
                let wrong_name_slice = self.cursor.select(not(blank().or(like(TokenType::is_ponctuation)))).to_owned();
                self.mk_parse_error(
                    "function name is invalid.",
                    wrong_name_slice.as_slice(),
                    ParseErrorKind::Unexpected,
                )
            }).map(|t| t.value)
    }
}