use crate::Expr;
use src_macros::segment_holder;

use crate::function::FunctionDeclaration;
use crate::r#type::{Type, TypeParameter};
use crate::variable::Identifier;

#[segment_holder]
#[derive(Debug, Clone, PartialEq)]
pub struct StructDeclaration {
    pub name: Identifier,
    pub parameters: Vec<TypeParameter>,
    pub fields: Vec<FieldDeclaration>,
}

#[segment_holder]
#[derive(Debug, Clone, PartialEq)]
pub struct FieldDeclaration {
    pub name: Identifier,
    pub tpe: Type,
}

#[segment_holder]
#[derive(Debug, Clone, PartialEq)]
pub struct StructImpl {
    pub type_parameters: Vec<TypeParameter>,
    pub impl_type: Type,
    pub functions: Vec<FunctionDeclaration>,
}

#[segment_holder]
#[derive(Debug, Clone, PartialEq)]
pub struct FieldAccess {
    pub expr: Box<Expr>,
    pub field: Identifier,
}
