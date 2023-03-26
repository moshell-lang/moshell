use crate::context::Context;
use crate::types::{DefinedType, Type};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ClassType {
    ///The type's constraint (if any)
    /// Can be a super type, a type on which the type can be substituted
    pub constraint_type: Option<DefinedType>,

    /// The class type's base
    pub base: DefinedType,

    ///class type's parameters
    pub params: Vec<Type>,

    ///inputs / outputs if this type is callable
    pub callable: Option<Box<CallableAspect>>,
}


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CallableAspect {
    pub inputs: Vec<ClassType>,
    pub output: ClassType,
}

impl ClassType {
    pub fn cons(tpe: DefinedType) -> Self {
        Self {
            constraint_type: None,
            base: tpe,
            params: Vec::new(),
            callable: None,
        }
    }

    pub fn specialized(constraint_type: DefinedType, tpe: DefinedType) -> Self {
        Self {
            constraint_type: Some(constraint_type),
            base: tpe,
            params: Vec::new(),
            callable: None,
        }
    }

    ///Finds largest base type possible with given class (if possible)
    pub fn unify_base(&self, ctx: &Context, other: &DefinedType) -> Result<Option<DefinedType>, String> {
        let mut self_lineage = Some(self);

        let other = ctx.lookup_definition(other)?;

        while let Some(self_lng) = self_lineage {
            if (&self_lng.base).eq(&other.base) {
                return Ok(Some((&other.base).clone()));
            }
            self_lineage = self_lng.constraint_type.as_ref().and_then(|st| ctx.lookup_definition(&st).ok());
        }

        if let Some(other_super) = &other.constraint_type {
            return self.unify_base(ctx, other_super);
        }
        Ok(None)
    }
}

