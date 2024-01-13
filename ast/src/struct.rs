use crate::Expr;
use src_macros::segment_holder;

use crate::function::FunctionDeclaration;
use crate::r#type::{Type, TypeParameter};
use crate::variable::Identifier;

#[segment_holder]
#[derive(Debug, Clone, PartialEq)]
pub struct StructDeclaration<'a> {
    pub name: Identifier<'a>,
    pub parameters: Vec<TypeParameter<'a>>,
    pub fields: Vec<FieldDeclaration<'a>>,
}

#[segment_holder]
#[derive(Debug, Clone, PartialEq)]
pub struct FieldDeclaration<'a> {
    pub name: Identifier<'a>,
    pub tpe: Type<'a>,
}

#[segment_holder]
#[derive(Debug, Clone, PartialEq)]
pub struct StructImpl<'a> {
    pub type_parameters: Vec<TypeParameter<'a>>,
    pub impl_type: Type<'a>,
    pub functions: Vec<FunctionDeclaration<'a>>,
}

#[segment_holder]
#[derive(Debug, Clone, PartialEq)]
pub struct FieldAccess<'a> {
    pub expr: Box<Expr<'a>>,
    pub field: Identifier<'a>,
}
