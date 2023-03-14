use crate::Expr;
use crate::variable::NamedDeclaration;

pub struct FunctionDeclaration<'a> {
    name: &'a str,
    type_parameters: Vec<&'a str>,
    return_type: Option<&'a str>,
    body: Expr<'a>,
}

pub enum FunctionParameter<'a> {
    Named(NamedDeclaration<'a>),
    ///argument is the type of the variable (if any).
    Variadic(Option<&'a str>)
}