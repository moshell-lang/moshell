use dbg_pls::DebugPls;

use src_macros::segment_holder;

use crate::function::FunctionDeclaration;
use crate::r#type::{Type, TypeParameter};

#[segment_holder]
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct StructDeclaration<'a> {
    pub name: &'a str,
    pub parameters: Vec<TypeParameter<'a>>,
    pub fields: Vec<FieldDeclaration<'a>>,
}

#[segment_holder]
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct FieldDeclaration<'a> {
    pub name: &'a str,
    pub tpe: Type<'a>,
}

#[segment_holder]
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct StructImpl<'a> {
    pub type_parameters: Vec<TypeParameter<'a>>,
    pub impl_type: Type<'a>,
    pub functions: Vec<FunctionDeclaration<'a>>,
}
