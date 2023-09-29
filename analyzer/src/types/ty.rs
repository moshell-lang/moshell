use std::collections::HashMap;

use crate::diagnostic::SourceLocation;
use crate::reef::ReefId;
use crate::relations::{Definition, NativeId, ObjectId};
use crate::types::{BOOL, ERROR, EXITCODE, FLOAT, INT, NOTHING, UNIT};

/// A type identifier in a [`Typing`] instance.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(pub ObjectId);

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct TypeRef {
    pub reef: ReefId,
    pub type_id: TypeId,
}

impl TypeRef {
    pub const fn new(reef: ReefId, tpe: TypeId) -> Self {
        Self { reef, type_id: tpe }
    }

    pub fn is_nothing(self) -> bool {
        self == NOTHING
    }

    pub fn is_something(self) -> bool {
        self != NOTHING
    }

    pub fn is_ok(self) -> bool {
        self != ERROR
    }

    pub fn is_err(self) -> bool {
        self == ERROR
    }

    pub fn is_obj(self) -> bool {
        !matches!(self, NOTHING | UNIT | BOOL | EXITCODE | INT | FLOAT | ERROR)
    }
}

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

    /// A type for nothingness, attributed to expressions that never returns
    Nothing,

    /// A void type, that contains no value.
    Unit,

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

    /// A vector type, that contains a list of elements of the same type.
    Vector,

    /// A nullable type, that can be either `null` or a value of a given type.
    Option,

    /// A generic type, that can be instantiated with concrete type parameters.
    Polytype,

    /// An instance of a generic type with concrete type parameters.
    Instantiated(TypeRef, Vec<TypeRef>),
}

impl Type {
    /// Returns whether the type is named.
    ///
    /// Named types convey a non-positional definition, such as a function.
    pub fn is_named(&self) -> bool {
        matches!(self, Self::Function(_))
    }
}

/// A callable function signature.
#[derive(Clone, Debug, PartialEq)]
pub struct FunctionType {
    /// The exact parameters that are expected by the function.
    pub(crate) parameters: Vec<Parameter>,

    /// The return type of the function.
    pub(crate) return_type: TypeRef,

    /// The environment of the function, or the native function ID.
    pub(crate) definition: Definition,
}

/// A function parameter.
#[derive(Clone, Debug, PartialEq)]
pub struct Parameter {
    pub(crate) location: Option<SourceLocation>,
    pub ty: TypeRef,
}

impl FunctionType {
    /// Creates a new native function.
    ///
    /// Natives functions cannot be defined by the user, since it is a
    /// chicken-and-egg problem. They are defined by the language host,
    /// usually in a Rust or C++ VM. They are identified by a dedicated
    /// [`NativeId`], so that the compiler can quickly identify them.
    pub fn native(parameters: Vec<TypeRef>, return_type: TypeRef, id: NativeId) -> Self {
        Self {
            parameters: parameters
                .into_iter()
                .map(|ty| Parameter { location: None, ty })
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
    pub(crate) generics: Vec<TypeRef>,
    pub(crate) methods: HashMap<String, Vec<MethodType>>,
}

/// A method is a function that only exists on a given type.
pub type MethodType = FunctionType;
