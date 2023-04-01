use std::hash::{Hasher};
use std::rc::Rc;
use crate::lang_types::any;
use crate::type_context::TypeContext;
use crate::types::{DefinedType, Type};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassType {
    ///The super type of this class
    pub super_type: Option<Rc<ClassType>>,

    /// The class type's name
    pub name: String,

    ///The class's generic parameters bounds.
    pub generic_parameters: Vec<GenericParam>,

    ///This Class's identity.
    pub identity: u64,
}


#[derive(Debug, Clone, PartialEq, Eq)]
struct GenericParam {
    name: String,
    erased_type: Rc<ClassType>,

    index: usize,

    ///The parent's generic parameter indexes of the hosting ClassType.
    /// Examples:
    /// - `class List[A]: Iterable[A]`   there's one generic bound to parent's parameter at index 0
    /// - `class WeirdMap[A]: Map[A, A]` the A generic is put in parent's 0 and 1 generics
    /// - `class Map[A, B]: Iterable[A]` this defines a map that is iterable only on keys, the first map's generic parameter `A`
    ///                                  is then bound on parent's generic at index 0, and the generic param `B` has no links with the parent.
    parents_index: Vec<usize>,
}

struct ClassTypeBuilder {
    super_type: Option<Rc<ClassType>>,
    name: String,
    generic_parameters: Vec<GenericParam>,
    identity: u64,

    next_ordinal: u16,
}

impl ClassTypeBuilder {
    pub fn new(name: String, identity: u64) -> Self {
        Self {
            name,
            identity,
            generic_parameters: Vec::new(),
            super_type: None,
            next_ordinal: 0,
        }
    }

    pub fn with_parent(self, parent: Rc<ClassType>) -> Self {
        Self {
            super_type: Some(parent),
            ..self
        }
    }

    pub fn with_parameter(mut self, name: String, bound: Rc<ClassType>, parent_ordinals: &[u16]) -> Self {
        self.next_ordinal += 1;
        self.generic_parameters.push(GenericParam {
            name,
            erased_type: bound,
            index: self.next_ordinal,
            parents_index: parent_ordinals.to_vec(),
        });
        self
    }

    pub fn build(self, ctx: &TypeContext) -> Result<ClassType, String> {
        ClassType::from_builder(self, ctx)
    }
}

impl ClassType {
    ///Constructs a specialized class (child class making self class a parent of the constructed one)
    pub fn mk_specialized(self: Rc<Self>, child_name: String, identity: u64) -> ClassTypeBuilder {
        ClassTypeBuilder::new(child_name, identity)
            .with_parent(self)
    }

    fn from_builder(builder: ClassTypeBuilder, ctx: &TypeContext) -> Result<Self, String> {
        let parent = builder.super_type;
        for gparam in builder.generic_parameters {
            for parent_index in gparam.parents_index {
                let parent = parent.ok_or("wrong type definition: this class type has linked parameter with a parent that does not exists.")?;
                let gparam_parent = parent.generic_parameters.get(parent_index).ok_or(format!("parent type parameter has no parameter at index {parent_index}"))?;
                if !gparam.erased_type.is_covariant_with(gparam_parent.erased_type) {
                    return Err(format!("{} is incompatible with {}'s generic parameter {}", gparam.name, parent.name, gparam_parent.name))
                }
            }
        }

        todo!()
    }

    fn is_covariant_with(self: Rc<Self>, other: Rc<ClassType>) -> bool {
        false
    }

    ///Finds largest base type with given class (Any in worst cases.)
    pub fn unify_with(self: Rc<Self>, ctx: &TypeContext, other: &DefinedType) -> DefinedType {
        let mut self_lineage = Some(self.clone());

        let other_class = ctx.lookup_definition(other)?;

        while let Some(self_lng) = self_lineage {
            if self_lng.identity == other_class.identity {
                return self_lng.unify_parameters(ctx, other);
            }
            self_lineage = self_lng.super_type.clone()
        }

        if let Some(other_super) = &other_class.super_type {
            return self.unify_with(ctx, &other_super.base);
        }

        //this line may never be reached as all types should already implement Any.
        any()
    }

    fn unify_parameters(self: Rc<Self>, ctx: &TypeContext, other: &DefinedType) -> DefinedType {}
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