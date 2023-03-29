use std::rc::Rc;
use crate::context::Context;
use crate::types::{DefinedType};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassType<'a> {
    ///The super type of this class
    pub super_type: Option<Rc<ClassType<'a>>>,

    /// The class type's base definition
    pub base: DefinedType,

    /// The initial context of the class type
    pub ctx: Rc<Context<'a>>,
}

impl<'a> ClassType<'a> {
    ///Finds largest base type possible with given class (if possible)
    pub fn unify_base(self: &Rc<Self>, ctx: &Context, other: &DefinedType) -> Result<Option<DefinedType>, String> {
        let mut self_lineage = Some(self);

        let other = ctx.lookup_definition(other)?;

        while let Some(self_lng) = self_lineage {
            if (&self_lng.base).eq(&other.base) {
                return Ok(Some((&other.base).clone()));
            }
            self_lineage = if let Some(st) = &self_lng.super_type {
                ctx.lookup_definition(&st.base).ok()
            } else {
                None
            }
        }

        if let Some(other_super) = &other.super_type {
            return self.unify_base(ctx, &other_super.base);
        }
        Ok(None)
    }
}

