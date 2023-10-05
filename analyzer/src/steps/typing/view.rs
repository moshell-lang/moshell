use std::fmt;
use std::fmt::Display;

use crate::steps::typing::bounds::TypesBounds;
use crate::steps::typing::exploration::Exploration;
use crate::types::ty::{Type, TypeRef};

#[derive(Copy, Clone)]
pub(super) struct TypeView<'a> {
    pub(super) id: TypeRef,
    pub(super) exploration: &'a Exploration<'a>,
    pub(super) bounds: &'a TypesBounds,
}

impl<'a> TypeView<'a> {
    pub(super) fn new(id: TypeRef, exploration: &'a Exploration, bounds: &'a TypesBounds) -> Self {
        Self {
            id,
            exploration,
            bounds,
        }
    }
}

impl fmt::Debug for TypeView<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl Display for TypeView<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let ty = self.bounds.get_bound(self.id);

        let tpe = self.exploration.get_type(ty).unwrap_or(&Type::Error);

        if let Type::Instantiated(def, parameters) = tpe {
            write!(f, "{}[", Self::new(*def, self.exploration, self.bounds))?;
            for (i, parameter) in parameters.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(
                    f,
                    "{}",
                    &Self::new(*parameter, self.exploration, self.bounds)
                )?;
            }
            return write!(f, "]");
        }

        write!(
            f,
            "{}",
            self.exploration
                .get_type_name(ty)
                .map(String::as_str)
                .unwrap_or("<?>")
        )
    }
}

pub struct TypeInstanceVec<'a> {
    pub(super) ids: Vec<TypeRef>,
    pub(super) exploration: &'a Exploration<'a>,
    pub(super) bounds: &'a TypesBounds,
}

impl<'a> TypeInstanceVec<'a> {
    pub(super) fn new(
        ids: Vec<TypeRef>,
        exploration: &'a Exploration,
        bounds: &'a TypesBounds,
    ) -> Self {
        Self {
            ids,
            exploration,
            bounds,
        }
    }
}

impl fmt::Debug for TypeInstanceVec<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[")?;
        for (i, id) in self.ids.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", TypeView::new(*id, self.exploration, self.bounds))?;
        }
        write!(f, "]")
    }
}

impl fmt::Display for TypeInstanceVec<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, id) in self.ids.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "`{}`", TypeView::new(*id, self.exploration, self.bounds))?;
        }
        Ok(())
    }
}
