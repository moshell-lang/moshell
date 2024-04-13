use crate::typing::registry::{self, FunctionId, SchemaId};
use crate::typing::UnifyError;
use std::ops::Index;

/// A user-defined type that can be referenced by a [`TypeId`].
#[derive(Debug, PartialEq, Eq, Clone, Default)]
pub enum UserType {
    #[default]
    Unknown,

    Error,

    Nothing,

    Unit,

    Function(FunctionId),

    Parametrized {
        schema: SchemaId,
        params: Vec<TypeId>,
    },

    Module(Vec<ast::r#use::InclusionPathItem>),

    GenericVariable(String),
}

impl From<FunctionId> for UserType {
    fn from(func: FunctionId) -> Self {
        Self::Function(func)
    }
}

impl From<SchemaId> for UserType {
    fn from(schema: SchemaId) -> Self {
        Self::Parametrized {
            schema,
            params: Vec::new(),
        }
    }
}

/// A collection of types that can be referenced by an [`TypeId`].
pub struct TypeArena {
    types: Vec<UserType>,
}

impl TypeArena {
    /// Allocates a new [`TypeId`] for the given [`UserType`].
    pub(crate) fn alloc(&mut self, ty: UserType) -> TypeId {
        let id = self.types.len();
        self.types.push(ty);
        TypeId(id)
    }

    pub(crate) fn unify(&mut self, rhs: TypeId, assign_to: TypeId) -> Result<TypeId, UnifyError> {
        match (&self[assign_to], &self[rhs]) {
            (UserType::Error, _) | (_, UserType::Error) => Ok(ERROR_TYPE),
            (_, UserType::Nothing) => Ok(assign_to),
            (UserType::Unknown, _) | (_, UserType::Unknown) => {
                panic!("Unknown type should not be unified")
            }
            (lhs, rhs) if lhs == rhs => Ok(assign_to),
            (_, _) => Err(UnifyError),
        }
    }

    /// Given a possible generic type, create a parameterized variant for the given context.
    pub(crate) fn concretize(
        &mut self,
        ty: TypeId,
        generics: &[TypeId],
        params: &[TypeId],
    ) -> TypeId {
        assert_eq!(generics.len(), params.len(), "expected same length between generics {generics:?} and their concretized counterparts {params:?}");
        match &self[ty] {
            UserType::Parametrized {
                schema,
                params: sub_params,
            } => {
                let concrete_params = sub_params
                    .iter()
                    .map(|ty| {
                        if let Some(concrete_ty) = generics.iter().position(|&pty| pty == *ty) {
                            params[concrete_ty]
                        } else {
                            *ty
                        }
                    })
                    .collect::<Vec<_>>();
                self.alloc(UserType::Parametrized {
                    schema: *schema,
                    params: concrete_params,
                })
            }
            _ => generics
                .iter()
                .position(|&pty| pty == ty)
                .map_or(ty, |idx| params[idx]),
        }
    }
}

/// An access key to one of the types in the [`TypeArena`].
///
/// An identifier should only be used with the [`TypeArena`] that it was created with.
/// Only the placeholder value [`TypeId::default`] and the predefined constants are guaranteed to be
/// valid across all [`TypeArena`]s.
#[derive(Debug, Clone, PartialEq, Eq, Copy, Default)]
pub struct TypeId(usize);

pub const UNKNOWN_TYPE: TypeId = TypeId(0);
pub const ERROR_TYPE: TypeId = TypeId(1);
pub const NOTHING_TYPE: TypeId = TypeId(2);
pub const UNIT_TYPE: TypeId = TypeId(3);
pub const INT_TYPE: TypeId = TypeId(4);
pub const BOOL_TYPE: TypeId = TypeId(5);
pub const EXITCODE_TYPE: TypeId = TypeId(6);
pub const FLOAT_TYPE: TypeId = TypeId(7);
pub const STRING_TYPE: TypeId = TypeId(8);
pub const GENERIC_TYPE: TypeId = TypeId(9);
pub const VECTOR_TYPE: TypeId = TypeId(10);
pub const GLOB_TYPE: TypeId = TypeId(11);
pub const PID_TYPE: TypeId = TypeId(13);

/// Gets the [`TypeId`] for a built-in type by its name.
pub(crate) fn lookup_builtin_type(name: &str) -> Option<TypeId> {
    match name {
        "Nothing" => Some(NOTHING_TYPE),
        "Unit" => Some(UNIT_TYPE),
        "Int" => Some(INT_TYPE),
        "Bool" => Some(BOOL_TYPE),
        "Exitcode" => Some(EXITCODE_TYPE),
        "Float" => Some(FLOAT_TYPE),
        "String" => Some(STRING_TYPE),
        "Vec" => Some(VECTOR_TYPE),
        "Glob" => Some(GLOB_TYPE),
        "Pid" => Some(PID_TYPE),
        _ => None,
    }
}

impl TypeId {
    pub fn is_ok(self) -> bool {
        self != ERROR_TYPE
    }

    pub fn is_err(self) -> bool {
        self == ERROR_TYPE
    }

    pub fn is_obj(self) -> bool {
        !matches!(
            self,
            NOTHING_TYPE
                | UNIT_TYPE
                | INT_TYPE
                | BOOL_TYPE
                | EXITCODE_TYPE
                | FLOAT_TYPE
                | PID_TYPE
                | ERROR_TYPE
        )
    }

    pub fn define_if_absent(&mut self, ty: Self) {
        if *self == UNKNOWN_TYPE {
            self.0 = ty.0;
        }
    }
}

impl Default for TypeArena {
    fn default() -> Self {
        Self {
            types: vec![
                UserType::Unknown,
                UserType::Error,
                UserType::Nothing,
                UserType::Unit,
                UserType::from(registry::INT_SCHEMA),
                UserType::from(registry::BOOL_SCHEMA),
                UserType::from(registry::EXITCODE_SCHEMA),
                UserType::from(registry::FLOAT_SCHEMA),
                UserType::from(registry::STRING_SCHEMA),
                UserType::GenericVariable("T".to_owned()),
                UserType::Parametrized {
                    schema: registry::VEC_SCHEMA,
                    params: vec![GENERIC_TYPE],
                },
                UserType::from(registry::GLOB_SCHEMA),
                UserType::from(registry::PID_SCHEMA),
            ],
        }
    }
}

impl Index<TypeId> for TypeArena {
    type Output = UserType;

    fn index(&self, index: TypeId) -> &Self::Output {
        &self.types[index.0]
    }
}
