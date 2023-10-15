use std::collections::HashMap;

use crate::diagnostic::SourceLocation;
use crate::reef::ReefId;
use crate::relations::{LocalId, ObjectId, SourceId};
use crate::types::engine::{FunctionId, StructureId};
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

    /// A generic type, that can be instantiated with concrete type parameters.
    Polytype,

    /// A callable type, that have a separate definition.
    /// with a bound source, if any
    Function(Option<SourceId>, FunctionId),

    /// An instance of a generic type with concrete type parameters.
    Instantiated(TypeRef, Vec<TypeRef>),

    /// A named structured type, with its separate definition
    /// with a bound source, if any
    Structure(Option<SourceId>, StructureId),
}

impl Type {
    /// Returns whether the type is named.
    ///
    /// Named types convey a non-positional definition, such as a function.
    pub fn is_named(&self) -> bool {
        matches!(self, Self::Function(_, _))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Field {
    pub ty: TypeRef,
    pub local_id: LocalId,
}

/// A Structured type
#[derive(Clone, Debug, PartialEq, Default)]
pub struct StructureDesc {
    /// Type parameters of the structure
    pub type_parameters: Vec<TypeId>,

    /// Fields of the structure.
    pub fields: HashMap<String, Field>,

    /// methods of the structure
    pub methods: HashMap<String, Vec<FunctionId>>,
}

impl StructureDesc {
    pub fn get_fields(&self) -> Vec<&Field> {
        let mut field_refs: Vec<_> = self.fields.values().collect();
        field_refs.sort_by_key(|f| f.local_id.0);
        field_refs
    }
}

/// A callable function signature.
#[derive(Clone, Debug, PartialEq)]
pub struct FunctionDesc {
    /// Type parameters of the function
    pub type_parameters: Vec<TypeId>,

    /// The exact parameters that are expected by the function.
    pub parameters: Vec<Parameter>,

    /// The return type of the function.
    pub return_type: TypeRef,

    /// Kind of function
    pub kind: FunctionKind,
}

#[derive(Clone, Debug, PartialEq)]
pub enum FunctionKind {
    Function,
    Constructor,
}

impl FunctionDesc {
    /// Create the main function of a script
    pub(crate) fn script() -> Self {
        Self {
            type_parameters: vec![],
            parameters: vec![],
            return_type: UNIT,
            kind: FunctionKind::Function,
        }
    }

    pub fn constructor(
        type_parameters: Vec<TypeId>,
        parameters: Vec<TypeRef>,
        return_type: TypeRef,
    ) -> Self {
        Self::new(
            type_parameters,
            parameters,
            return_type,
            FunctionKind::Constructor,
        )
    }

    /// Creates a new function.
    pub fn function(
        type_parameters: Vec<TypeId>,
        parameters: Vec<TypeRef>,
        return_type: TypeRef,
    ) -> Self {
        Self::new(
            type_parameters,
            parameters,
            return_type,
            FunctionKind::Function,
        )
    }

    fn new(
        type_parameters: Vec<TypeId>,
        parameters: Vec<TypeRef>,
        return_type: TypeRef,
        kind: FunctionKind,
    ) -> Self {
        Self {
            type_parameters,
            parameters: parameters
                .into_iter()
                .enumerate()
                .map(|(param_offset, ty)| Parameter {
                    location: None,
                    ty,
                    local_id: LocalId(param_offset),
                })
                .collect(),
            return_type,
            kind,
        }
    }
}

/// A function parameter.
#[derive(Clone, Debug, PartialEq)]
pub struct Parameter {
    pub(crate) location: Option<SourceLocation>,
    pub ty: TypeRef,
    pub local_id: LocalId,
}

/// A method is a function that only exists on a given type.
pub type MethodType = FunctionDesc;
