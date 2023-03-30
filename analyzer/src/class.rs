use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::rc::Rc;
use crate::type_context::TypeContext;
use crate::types::{DefinedType};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassType {
    ///The super type of this class
    pub super_type: Option<Rc<ClassType>>,

    /// The class type's base definition
    pub base: DefinedType,

    identity: u64,
}

impl ClassType {

    pub fn new(super_type: Option<Rc<ClassType>>, base: DefinedType) -> Self {
        let mut t = DefaultHasher::new();
        base.hash(&mut t);
        Self {
            super_type,
            base,
            identity: t.finish()
        }
    }

    ///Finds largest base type possible with given class (if possible)
    pub fn unify_base(self: Rc<Self>, ctx: &TypeContext, other: &DefinedType) -> Result<Option<DefinedType>, String> {
        let mut self_lineage = Some(self.clone());

        let other_class = ctx.lookup_definition(other)?;

        while let Some(self_lng) = self_lineage {
            if self_lng.identity == other_class.identity {
                return Ok(Some(self_lng.base.clone()));
            }
            self_lineage = self_lng.super_type.clone()
        }

        if let Some(other_super) = &other_class.super_type {
            return self.unify_base(ctx, &other_super.base);
        }
        Ok(None)
    }
}

#[cfg(test)]
mod tests {
    use crate::environment::Environment;
    use crate::lang_types::{any, float, int};
    use pretty_assertions::assert_eq;
    use crate::types::{DefinedType, ParameterizedType, Type};

    #[test]
    fn int_and_float_union() {
        let lang = Environment::lang();
        let res1 = lang.types.unify(&Type::Defined(int()), &Type::Defined(float())).expect("union error");
        let res2 = lang.types.unify(&Type::Defined(float()), &Type::Defined(int())).expect("union error");
        let expected = Type::Defined(float());
        assert_eq!(res1, expected);
        assert_eq!(res2, expected);
    }

    #[test]
    fn list_and_iterable_union() {
        let lang = Environment::lang();
        let mut env = lang.fork();
        //equivalent to a "Iterable[Any]" type
        let iterable = DefinedType::Parameterized(ParameterizedType::parametrized("Iterable", vec![Type::Defined(any())]));
        //equivalent to a "List[Any]" type
        let list = DefinedType::Parameterized(ParameterizedType::parametrized("List", vec![Type::Defined(any())]));

        //equivalent to a "class Iterable[A] {}" statement.
        env.types.define_class(&any(), iterable.clone()).expect("type registration");
        //equivalent to a "class List[A]: Iterable[A] {}" statement.
        env.types.define_class(&iterable, list).expect("type registration");

        let res1 = env.types.unify(&Type::Defined(int()), &Type::Defined(float())).expect("union error");
        let res2 = env.types.unify(&Type::Defined(float()), &Type::Defined(int())).expect("union error");
        let expected = Type::Defined(iterable);
        assert_eq!(res1, expected);
        assert_eq!(res2, expected);
    }
}