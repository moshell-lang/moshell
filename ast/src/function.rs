use crate::variable::NamedDeclaration;
use crate::Expr;
use crate::r#type::Type;

pub struct FunctionDeclaration<'a> {
    name: &'a str,
    type_parameters: Vec<Type<'a>>,
    return_type: Option<&'a str>,
    body: Expr<'a>,
}

pub enum FunctionParameter<'a> {
    Named(NamedDeclaration<'a>),
    ///argument is the type of the variable (if any).
    Variadic(Option<Type<'a>>),
}
