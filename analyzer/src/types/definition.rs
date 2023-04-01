
use std::rc::Rc;
use crate::lang_types::any;
use crate::types::context::TypeContext;
use crate::types::types::DefinedType;

///This structures hosts the definition of a types,
///
#[derive(Debug, Clone, Eq)]
pub struct TypeDef {
    ///The super types of this types
    pub super_type: Option<Rc<TypeDef>>,

    /// The class types's name
    pub name: String,

    ///The class's generic parameters bounds.
    pub generic_parameters: Vec<GenericParam>,

    ///This Type's identity.
    pub(crate) identity: u64,
}

impl PartialEq for TypeDef {
    fn eq(&self, other: &Self) -> bool {
        self.identity == other.identity
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GenericParam {
    pub name: String,
    pub erased_type: Rc<TypeDef>,

    ///The parent's generic parameter indexes of the hosting ClassType.
    /// Examples:
    /// - `class List[A]: Iterable[A]`   there's one generic bound to parent's parameter at index 0
    /// - `class WeirdMap[A]: Map[A, A]` the A generic is put in parent's 0 and 1 generics
    /// - `class Map[A, B]: Iterable[A]` this defines a map that is iterable only on keys, the first map's generic parameter `A`
    ///                                  is then bound on parent's generic at index 0, and the generic param `B` has no links with the parent.
    pub parent_indexes: Vec<usize>,
}

pub struct ClassTypeDefinition {
    super_type: Option<Rc<TypeDef>>,
    name: String,
    generic_parameters: Vec<GenericParam>,
    identity: u64,

}

impl ClassTypeDefinition {
    pub(crate) fn new(name: String, identity: u64) -> Self {
        Self {
            name,
            identity,
            generic_parameters: Vec::new(),
            super_type: None,
        }
    }

    pub fn with_parent(self, parent: Rc<TypeDef>) -> Self {
        Self {
            super_type: Some(parent),
            ..self
        }
    }

    pub fn with_parameter(mut self, name: &str, bound: Rc<TypeDef>, parent_ordinals: &[usize]) -> Self {
        self.generic_parameters.push(GenericParam {
            name: name.to_string(),
            erased_type: bound,
            parent_indexes: parent_ordinals.to_vec(),
        });
        self
    }

    pub fn build(self, ctx: &TypeContext) -> Result<TypeDef, String> {
        TypeDef::from_builder(self, ctx)
    }
}


impl TypeDef {

    pub(crate) fn from_builder(definition: ClassTypeDefinition, ctx: &TypeContext) -> Result<Self, String> {
        let parent = definition.super_type;
        //Ensure that all generic parameters are covariant with their linked parent's generics
        for gparam in &definition.generic_parameters {
            for parent_index in &gparam.parent_indexes {
                let parent = parent.clone().ok_or("wrong types definition: this class types has linked parameter with a parent that does not exists.")?;
                let gparam_parent = parent.generic_parameters.get(parent_index.clone()).ok_or(format!("parent types parameter has no parameter at index {parent_index}"))?;

                if !gparam.clone().erased_type.is_subtype_of(gparam_parent.erased_type.clone()) {
                    return Err(format!("{} is incompatible with {}'s generic parameter {}", gparam.name, parent.name, gparam_parent.name))
                }
            }
        }
        let super_type = if let Some(st) = parent {
            st
        } else {
            ctx.lookup_defined(any())?
        };
        Ok(Self {
            super_type: Some(super_type),
            name: definition.name,
            generic_parameters: definition.generic_parameters,
            identity: definition.identity,
        })
    }

    fn is_subtype_of(self: Rc<Self>, other: Rc<TypeDef>) -> bool {
        let mut self_lineage = Some(self.clone());

        //figure if self types is a subtype of other
        while let Some(self_lng) = self_lineage {
            if self_lng == other {
                return true;
            }

            self_lineage = self_lng.super_type.clone()
        }

        false
    }

    ///Finds largest base types with given class (Any in worst cases.)
    pub fn unify_with(self: Rc<Self>, ctx: &TypeContext, other: &DefinedType) -> DefinedType {
        any()
    }
}

#[cfg(test)]
mod tests {
    use crate::environment::Environment;
    use crate::lang_types::{any, float, int};
    use pretty_assertions::assert_eq;
    use crate::types::types::{DefinedType, ParameterizedType, Type};

    #[test]
    fn int_and_float_union() {
        let lang = Environment::lang();
        let res1 = lang.type_context.unify(&Type::Defined(int()), &Type::Defined(float())).expect("union error");
        let res2 = lang.type_context.unify(&Type::Defined(float()), &Type::Defined(int())).expect("union error");
        let expected = Type::Defined(float());
        assert_eq!(res1, expected);
        assert_eq!(res2, expected);
    }

    #[test]
    fn list_and_iterable_union() {
        let lang = Environment::lang();
        let env = lang.fork();

        //equivalent to a "Iterable[Any]" types
        let iterable = DefinedType::Parameterized(ParameterizedType::parametrized("Iterable", vec![Type::Defined(any())]));
        //equivalent to a "List[Any]" types
        let list = DefinedType::Parameterized(ParameterizedType::parametrized("List", vec![Type::Defined(any())]));

        let mut ctx = env.type_context;

        let any_def = ctx.lookup_defined(any()).expect("could not get any");

        //equivalent to a "class Iterable[A] {}" statement.
        let iterable_cl =
            ctx.define_class(
                ctx.mk_definition("Iterable")
                    .with_parameter("A", any_def.clone(), &[])
            ).expect("types registration");
        //equivalent to a "class List[A]: Iterable[A] {}" statement.
        ctx.define_class(
            ctx.mk_definition("List")
                .with_parent(iterable_cl)
                .with_parameter("A", any_def, &[0])
        ).expect("types registration");

        let res1 = ctx.unify(&Type::Defined(iterable.clone()), &Type::Defined(list.clone())).expect("union error");
        let res2 = ctx.unify(&Type::Defined(list), &Type::Defined(iterable.clone())).expect("union error");
        let expected = Type::Defined(iterable);
        assert_eq!(res1, expected);
        assert_eq!(res2, expected);
    }
}