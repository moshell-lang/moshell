use std::collections::HashMap;
use std::fmt::{Display, Formatter, write};
use std::rc::Rc;
use crate::lang_types::any;
use crate::types::context::TypeContext;
use crate::types::types;
use crate::types::types::Type;


///This structures hosts the definition of a types,
///
#[derive(Debug, Clone, Eq)]
pub struct TypeClass {
    ///The super types of this types
    pub super_type: Option<Rc<TypeClass>>,

    /// The class types's name
    pub name: String,

    ///The class's generic parameters bounds.
    pub generic_parameters: Vec<GenericParam>,

    ///The associations between child and parent type parameters
    /// This vector must match the length of parent's type parameters.
    /// - `class List[A]: Iterable[A]`   The generic param of child (`List`) is associated with generic param of parent (`Iterable`),
    ///                                  Thus, we have `List::A => Iterable::A`
    ///
    /// - `class WeirdMap[A]: Map[A, A]` Here, we have `WeirdMap::A => Map::K` and `WeirdMap::A => Map::V`
    ///
    /// - `class Map[K, V]: Iterable[A]` This defines a map that is iterable only on keys, the first map's generic parameter `K`
    ///                                  is then bound on lone iterable's generic parameter, and the generic param `V` has no links with the parent,
    ///                                  Thus we only have `Map::K => Iterable::A` as an association set.
    ///
    /// - `class StrMap[V]: Map[Str, V]` Here, the associations are `lang::Str => Map::K` and `StrMap::V => Map::V`
    pub(in crate::types) super_params_associations: Vec<ParamAssociation>,

    ///This Type's identity.
    pub(crate) identity: u64,
}

impl Display for TypeClass {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)?;
        if let Some((first, tail)) = self.generic_parameters.split_first() {
            write!(f, "[{}", first)?;
            for gparam in tail {
                write!(f, ", {}", gparam)?;
            }
        }

        Ok(())
    }
}

///Different kind of associations between a children and its parent generic parameters
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParamAssociation {
    ///The association is a generic parameter of the children
    Generic(String),
    ///The association is a known type class
    Defined(Rc<TypeClass>),
    ///The association is nothing
    Nothing,
}

impl PartialEq for TypeClass {
    fn eq(&self, other: &Self) -> bool {
        self.identity == other.identity
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GenericParam {
    pub name: String,
    pub erased_type: Rc<TypeClass>,
}

impl Display for GenericParam {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.erased_type)
    }
}

pub struct ClassTypeDefinition {
    super_type: Option<Rc<TypeClass>>,
    name: String,
    generic_parameters: Vec<GenericParam>,
    associations: HashMap<usize, ParamAssociation>,
    identity: u64,
}

impl ClassTypeDefinition {
    pub(crate) fn new(name: String, identity: u64) -> Self {
        Self {
            name,
            identity,
            generic_parameters: Vec::new(),
            associations: HashMap::new(),
            super_type: None,
        }
    }

    pub fn with_parent(self, parent: Rc<TypeClass>) -> Self {
        Self {
            super_type: Some(parent),
            ..self
        }
    }

    pub fn with_parameter(mut self, name: &str, bound: Rc<TypeClass>) -> Self {
        self.generic_parameters.push(GenericParam {
            name: name.to_string(),
            erased_type: bound,
        });
        self
    }

    pub fn with_association(mut self, parent_param_idx: usize, assoc: ParamAssociation) -> Self {
        self.associations.insert(parent_param_idx, assoc);
        self
    }

    pub fn build(self, ctx: &TypeContext) -> Result<TypeClass, String> {
        TypeClass::from_builder(self, ctx)
    }
}

impl TypeClass {

    fn from_builder(definition: ClassTypeDefinition, ctx: &TypeContext) -> Result<Self, String> {
        let parent = definition.super_type;

        let associations = parent.clone()
            .map(|p|
                Self::verify_associations(
                    definition.name.clone(),
                    p,
                    definition.associations,
                    &definition.generic_parameters,
                ))
            .transpose()?
            .unwrap_or_default();


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
            super_params_associations: associations,
        })
    }

    fn verify_associations(self_name: String,
                           parent: Rc<TypeClass>,
                           associations: HashMap<usize, ParamAssociation>,
                           gparams: &Vec<GenericParam>) -> Result<Vec<ParamAssociation>, String> {
        //Ensure that all generic parameters are compatible with their associated parent's generics
        //This algorithm only look if the generics classes are subtypes of parent's generics.

        if associations.len() != parent.generic_parameters.len() {
            return Err(format!("Type associations between {} and {} must match {} generic parameters count.", self_name, parent.name, parent.name))
        }

        let mut validated_associations = vec![ParamAssociation::Nothing; parent.generic_parameters.len()];

        for idx in 0..associations.len() {
            let association = associations
                .get(&idx)
                .ok_or(format!("No association set for parent generic parameter {}", parent.name))?;
            let parent_gparam = &parent.generic_parameters[idx];
            match association {
                ParamAssociation::Generic(name) => {
                    let d = gparams
                        .into_iter()
                        .find(|it| it.name.eq(name))
                        .ok_or(format!("Could not find generic parameter {} inside {} definition", name, self_name))?;

                    if !d.erased_type.is_subtype_of(parent_gparam.clone().erased_type) {
                        return Err(format!(
                            "generic parameter {} is not compatible with parent's generic parameter {}",
                            d, parent_gparam
                        ))
                    }
                }
                ParamAssociation::Defined(d) => {
                    //Compatibility is just if A is subtype of B now, we do not test covariance between type parameters yet.
                    if !d.is_subtype_of(parent_gparam.erased_type.clone()) {
                        return Err(format!(
                            "type {} is not compatible with parent's generic parameter {}",
                            d.name, parent_gparam
                        ))
                    }
                }
                ParamAssociation::Nothing => ()
            }
            validated_associations.push(association.clone())
        }

        Ok(validated_associations)
    }

    pub fn get_common_parent(self: Rc<Self>, other: Rc<TypeClass>) -> Rc<TypeClass> {
        let mut self_lineage = Some(self.clone());

        //figure if self types is a subtype of other
        while let Some(self_lng) = self_lineage {
            let mut other_lineage = Some(self.clone());

            while let Some(other_lng) = other_lineage {
                if self_lng == other_lng {
                    return self_lng;
                }
                other_lineage = other_lng.super_type.clone()
            }

            self_lineage = self_lng.super_type.clone()
        }

        panic!("cannot find command parent (does {} or {} extends Any ?)", self.name, other.name)
    }

    pub fn is_subtype_of(self: &Rc<Self>, other: Rc<TypeClass>) -> bool {
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

}

#[cfg(test)]
mod tests {
    use crate::environment::Environment;
    use crate::lang_types::{any, float, int};
    use pretty_assertions::assert_eq;
    use crate::types::class::ParamAssociation;
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
                    .with_parameter("A", any_def.clone())
            ).expect("types registration");
        //equivalent to a "class List[A]: Iterable[A] {}" statement.
        ctx.define_class(
            ctx.mk_definition("List")
                .with_parent(iterable_cl)
                .with_parameter("A", any_def)
                .with_association(0, ParamAssociation::Generic("A".to_owned()))
        ).expect("types registration");

        let res1 = ctx.unify(&Type::Defined(iterable.clone()), &Type::Defined(list.clone())).expect("union error");
        let res2 = ctx.unify(&Type::Defined(list), &Type::Defined(iterable.clone())).expect("union error");
        let expected = Type::Defined(iterable);
        assert_eq!(res1, expected);
        assert_eq!(res2, expected);
    }
}