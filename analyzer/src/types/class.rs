use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use crate::lang_types::any;
use crate::types::context::TypeContext;
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

    pub context: Rc<RefCell<TypeContext>>,
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
    ///The association is a known type class (can be a generic)
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
    pub name: String,
    generic_parameters: Vec<GenericParam>,
    associations: HashMap<usize, Type>,
    identity: u64,
}

impl ClassTypeDefinition {
    pub(crate) fn new(name: &str) -> Self {
        Self {
            name: name.to_owned(),
            generic_parameters: Vec::new(),
            associations: HashMap::new(),
            super_type: None,
            identity: 0,
        }
    }

    pub fn with_parent(self, parent: Rc<TypeClass>) -> Self {
        Self {
            super_type: Some(parent),
            ..self
        }
    }

    pub(in crate::types) fn with_identity(self, identity: u64) -> Self {
        Self {
            identity,
            ..self
        }
    }

    pub fn with_generic(mut self, name: &str, bound: Rc<TypeClass>) -> Self {
        self.generic_parameters.push(GenericParam {
            name: name.to_string(),
            erased_type: bound,
        });
        self
    }

    pub fn with_association(mut self, parent_param_idx: usize, ty: Type) -> Self {
        self.associations.insert(parent_param_idx, ty);
        self
    }

    pub fn build(self, ctx: Rc<RefCell<TypeContext>>) -> Result<TypeClass, String> {
        TypeClass::from_builder(self, ctx)
    }
}

impl TypeClass {
    fn from_builder(definition: ClassTypeDefinition, parent_ctx: Rc<RefCell<TypeContext>>) -> Result<Self, String> {
        let parent = definition.super_type;

        let class_context = TypeContext::fork(parent_ctx.clone());

        let associations = parent.clone()
            .map(|p|
                Self::verify_associations(
                    definition.name.clone(),
                    p,
                    &class_context,
                    definition.associations,
                ))
            .transpose()?
            .unwrap_or_default();

        let super_type = if let Some(st) = parent {
            st
        } else {
            parent_ctx.borrow().lookup_defined(any())?
        };

        let def = Self {
            super_type: Some(super_type),
            name: definition.name,
            generic_parameters: definition.generic_parameters,
            identity: definition.identity,
            super_params_associations: associations,
            context: Rc::new(RefCell::new(class_context)),
        };

        Ok(def)
    }

    fn verify_associations(self_name: String,
                           parent: Rc<TypeClass>,
                           class_ctx: &TypeContext,
                           associations: HashMap<usize, Type>) -> Result<Vec<ParamAssociation>, String> {
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

            let association = match association {
                Type::Nothing =>
                    ParamAssociation::Nothing,

                Type::Defined(defined) => {
                    let class = class_ctx.lookup_defined(defined.clone())?;
                    if !class.is_subtype_of(parent_gparam.erased_type.clone()) {
                        return Err(format!(
                            "type {} is not compatible with parent's generic parameter {}",
                            class.name, parent_gparam
                        ))
                    }
                    ParamAssociation::Defined(class)
                },

                Type::Unknown => return Err("unexpected <unknown> type.".to_string())
            };
            validated_associations.push(association)
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
    use std::cell::RefCell;
    use std::ops::Deref;
    use std::rc::Rc;
    use crate::lang_types::{any};
    use crate::types::context::TypeContext;

    use pretty_assertions::assert_eq;
    use crate::types::class::{ClassTypeDefinition, GenericParam, TypeClass};
    use crate::types::types::{Type};

    #[test]
    fn define_iterable() {
        let lang = TypeContext::lang();
        let mut ctx = Rc::new(RefCell::new(TypeContext::fork(lang)));

        let any_cl = ctx.borrow().lookup_defined(any()).expect("Any not found");

        let iterable_cl =
            TypeContext::define_class(&ctx,
                                      ClassTypeDefinition::new("Iterable")
                                          .with_generic("A", any_cl.clone()),
            ).expect("could not define Iterable[A]");

        assert_eq!(iterable_cl.deref(), &TypeClass {
            super_type: Some(any_cl.clone()),
            name: "Iterable".to_string(),
            generic_parameters: vec![GenericParam {
                erased_type: any_cl,
                name: "A".to_string(),
            }],
            super_params_associations: vec![],
            identity: iterable_cl.identity,
            context: Rc::new(RefCell::new(Default::default())),
        })
    }

    #[test]
    fn define_list() -> Result<(), String> {
        let lang = TypeContext::lang();
        let ctx = Rc::new(RefCell::new(TypeContext::fork(lang.clone())));

        let any_cl = lang.borrow().lookup_defined(any())?;

        let iterable_cl = TypeContext::define_class(&ctx,
                                                    ClassTypeDefinition::new("Iterable")
                                                        .with_generic("A", any_cl.clone()),
        )?;

        let list_cl = TypeContext::define_class(&ctx,
                                                ClassTypeDefinition::new("List")
                                                    .with_generic("B", any_cl.clone())
                                                    .with_association(0, Type::cons("B")),
        )?;

        assert_eq!(list_cl.deref(), &TypeClass {
            super_type: Some(list_cl.clone()),
            name: "List".to_string(),
            generic_parameters: vec![GenericParam {
                erased_type: any_cl,
                name: "A".to_string(),
            }],
            super_params_associations: vec![],
            identity: list_cl.identity,
            context: Rc::new(RefCell::new(Default::default()))
            ,
        });
        Ok(())
    }

    #[test]
    fn define_map() -> Result<(), String> {
        let lang = TypeContext::lang();
        let ctx = Rc::new(RefCell::new(TypeContext::fork(lang.clone())));

        let any_cl = lang.borrow().lookup_defined(any())?;

        let iterable_cl =
            TypeContext::define_class(
                &ctx,
                ClassTypeDefinition::new("Iterable")
                    .with_generic("A", any_cl.clone()),
            )?;

        let map_cl =
            TypeContext::define_class(
                &ctx,
                ClassTypeDefinition::new("Map")
                    .with_generic("K", any_cl.clone())
                    .with_generic("V", any_cl.clone())
                    .with_association(0, Type::cons("K"))
                    .with_association(1, Type::cons("V")),
            )?;

        assert_eq!(map_cl.deref(), &TypeClass {
            super_type: Some(map_cl.clone()),
            name: "List".to_string(),
            generic_parameters: vec![GenericParam {
                erased_type: any_cl,
                name: "A".to_string(),
            }],
            super_params_associations: vec![],
            identity: map_cl.identity,
            context: Rc::new(RefCell::new(Default::default())),
        });
        Ok(())
    }
}