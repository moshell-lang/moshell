use std::rc::Rc;
use crate::context::Context;
use crate::types::{DefinedType};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassType {
    ///The super type of this class
    pub super_type: Option<Rc<ClassType>>,

    /// The class type's base definition
    pub base: DefinedType,

    pub identity: usize
}

impl ClassType {
    ///Finds largest base type possible with given class (if possible)
    pub fn unify_base(self: Rc<Self>, ctx: &Context, other: &DefinedType) -> Result<Option<DefinedType>, String> {
        let mut self_lineage = Some(self.clone());

        let other_class = ctx.lookup_definition(other)?;

        while let Some(self_lng) = self_lineage {
            self_lineage = if let Some(st) = &self_lng.super_type {
                Some(ctx.lookup_definition(&st.base)?)
            } else {
                None
            }
        }

        if let Some(other_super) = &other_class.super_type {
            return self.unify_base(ctx, &other_super.base);
        }
        Ok(None)
    }
}

