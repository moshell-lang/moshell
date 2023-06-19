use crate::relations::{Definition, NativeId};
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

    /// An exit code type, on a single byte.
    ExitCode,

    /// An integer type, that contains a 32-bit signed integer.
    Int,

    /// A floating point type, that contains a 32-bit floating point number.
    Float,

    /// A string type, that contains a UTF-8 string.
    String,

    /// A callable type, that have a separate definition.
    Function(Definition),
}

impl Type {
    /// Returns whether the type is callable.
    pub fn is_callable(&self) -> bool {
        matches!(self, Self::Function(_))
    }
}

/// A callable function signature.
#[derive(Clone, Debug, PartialEq)]
pub struct FunctionType {
    /// The exact parameters that are expected by the function.
    pub(crate) parameters: Vec<Parameter>,

    /// The return type of the function.
    pub(crate) return_type: TypeId,

    /// The environment of the function, or the native function ID.
    pub(crate) definition: Definition,
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
            Type::ExitCode => write!(f, "ExitCode"),
            Type::Int => write!(f, "Int"),
            Type::Float => write!(f, "Float"),
            Type::String => write!(f, "String"),
            Type::Function(id) => write!(
                f,
                "fun#{}",
                match id {
                    Definition::User(id) => id.0,
                    Definition::Native(id) => id.0,
                }
            ),
        }
    }
}

impl FunctionType {
    /// Creates a new native function.
    ///
    /// Natives functions cannot be defined by the user, since it is a
    /// chicken-and-egg problem. They are defined by the language host,
    /// usually in a Rust or C++ VM. They are identified by a dedicated
    /// [`NativeId`], so that the compiler can quickly identify them.
    pub fn native(parameters: Vec<TypeId>, return_type: TypeId, id: NativeId) -> Self {
        Self {
            parameters: parameters
                .into_iter()
                .map(|ty| Parameter { segment: None, ty })
                .collect(),
            return_type,
            definition: Definition::Native(id),
        }
    }
}

/// The attributes and methods of a class.
///
/// This describes how a class behaves, while the [`Type`] describes the
/// instantiation of a type description. If a description is generic, it can
/// be instantiated multiple times with different [`Type`] parameters.
#[derive(Clone, Debug, PartialEq, Default)]
pub struct TypeDescription {
    pub(crate) methods: HashMap<String, Vec<MethodType>>,
}

/// A method is a function that only exists on a given type.
pub type MethodType = FunctionType;
