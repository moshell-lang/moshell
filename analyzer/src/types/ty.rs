use crate::relations::SourceObjectId;
use crate::types::hir::TypeId;
use context::source::SourceSegment;
use std::collections::HashMap;
use std::fmt::Display;

/// An instantiated type representation.
///
/// A type description has usually a single instance, but it can have more than one
/// if it is a generic type description.
#[derive(Clone, Debug, Default, PartialEq)]
pub enum Type {
    /// Reports a previous type error that is propagated.
    Error,

    /// A type that have not been inferred yet.
    #[default]
    Unknown,

    /// A void type, that contains no value.
    Nothing,

    /// A boolean type, either `true` or `false`.
    Bool,

    /// An integer type, that contains a 32-bit signed integer.
    Int,

    /// A floating point type, that contains a 32-bit floating point number.
    Float,

    /// A string type, that contains a UTF-8 string.
    String,

    /// A callable type, that have a separate environment.
    Function(SourceObjectId),
}

#[derive(Clone, Debug, PartialEq)]
pub struct UserFunction {
    pub(crate) parameters: Vec<Parameter>,
    pub(crate) return_type: TypeId,
    chunk: Option<SourceObjectId>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct NativeFunction {
    pub(crate) parameters: Vec<TypeId>,
    pub(crate) return_type: TypeId,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionType {
    pub(crate) parameters: Vec<Parameter>,
    pub(crate) return_type: TypeId,
    pub(crate) chunk: Option<SourceObjectId>,
}

/// A function parameter.
#[derive(Clone, Debug, PartialEq)]
pub struct Parameter {
    pub(crate) segment: Option<SourceSegment>,
    pub(crate) ty: TypeId,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Error => write!(f, "Error"),
            Type::Unknown => write!(f, "Unknown"),
            Type::Nothing => write!(f, "Nothing"),
            Type::Bool => write!(f, "Bool"),
            Type::Int => write!(f, "Int"),
            Type::Float => write!(f, "Float"),
            Type::String => write!(f, "String"),
            Type::Function(id) => write!(f, "fun#{}", id.0),
        }
    }
}

impl FunctionType {
    pub fn native(parameters: Vec<TypeId>, return_type: TypeId) -> Self {
        Self {
            parameters: parameters
                .into_iter()
                .map(|ty| Parameter { segment: None, ty })
                .collect(),
            return_type,
            chunk: None,
        }
    }
}

/// The attributes and methods of a class.
///
/// This describes how a class behaves, while the [`Type`] describes the
/// instanciation of a type description. If a description is generic, it can
/// be instantiated multiple times with different [`Type`] parameters.
#[derive(Clone, Debug, PartialEq, Default)]
pub struct TypeDescription {
    pub(crate) methods: HashMap<String, MethodType>,
}

pub type MethodType = FunctionType;
