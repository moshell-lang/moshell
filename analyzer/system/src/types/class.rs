use crate::lang_types::any;
use crate::name::Name;
use crate::types::context::TypeContext;
use crate::types::types::{ParameterizedType, Type};
use context::display::fmt_comma_separated;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::rc::Rc;

///This structures hosts the definition of a types,
#[derive(Clone, PartialEq)]
pub struct TypeClass {
    ///The super type of this type.
    pub super_type: Option<Rc<TypeClass>>,

    /// The bounds of the generic parameters of the class.
    pub generic_parameters: Vec<GenericParam>,

    pub is_exported: bool,

    /// The associations between the child and parent type parameters.
    ///
    /// This vector must match the length of parent's type parameters.
    /// For instance:
    /// - `class List[A]: Iterable[A]`      The generic param of the child (`List`) is associated with the generic param of the parent (`Iterable`),
    ///                                     Thus, we have `List::A => Iterable::A`
    ///
    /// - `class IndexMap[A]: Map[A, A]` Here, we have `IndexMap::A => Map::K` and `IndexMap::A => Map::V`
    ///
    /// - `class Map[K, V]: Iterable[A]`    This defines a map that is iterable only over its keys, the first map's generic parameter `K`
    ///                                     is then bound on lone iterable's generic parameter, and the generic param `V` has no links with the parent,
    ///                                     Thus we only have `Map::K => Iterable::A` as an association set.
    ///
    /// - `class StrMap[V]: Map[Str, V]`    Here, the associations are `lang::Str => Map::K` and `StrMap::V => Map::V`
    pub(in crate::types) super_params_associations: Vec<TypeParamAssociation>,

    /// The fully qualified class name.
    pub(crate) fqcn: Name,
}

impl Debug for TypeClass {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TypeClass")
            .field("fqcn", &self.fqcn)
            .field("super_type", &self.super_type)
            .field("is_exported", &self.is_exported)
            .field("generic_parameters", &self.generic_parameters)
            .field("super_params_associations", &self.super_params_associations)
            .finish()
    }
}

impl Display for TypeClass {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.fqcn)?;
        fmt_comma_separated('[', ']', &self.generic_parameters, f)
    }
}

///Different kind of associations between a children and its parent generic parameters
#[derive(Debug, Clone, PartialEq)]
pub enum TypeParamAssociation {
    ///The association is a known type class (can be a generic)
    Defined(Rc<TypeClass>),
    ///The association is nothing
    Nothing,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GenericParam {
    pub name: String,
    pub bound: ParameterizedType,
}

impl GenericParam {
    fn new(name: &str, bound: ParameterizedType) -> Self {
        Self {
            name: name.to_string(),
            bound,
        }
    }
}

impl Display for GenericParam {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.bound)
    }
}

pub struct ClassTypeDefinition {
    super_type: Option<Rc<TypeClass>>,
    pub name: Name,
    generic_parameters: Vec<GenericParam>,
    is_exported: bool,
    associations: HashMap<usize, Type>,
}

impl ClassTypeDefinition {
    pub fn new(name: Name) -> Self {
        Self {
            name,
            generic_parameters: Vec::new(),
            associations: HashMap::new(),
            is_exported: true,
            super_type: None,
        }
    }

    pub fn with_exported(self, is_exported: bool) -> Self {
        Self {
            is_exported,
            ..self
        }
    }

    pub fn with_super(self, parent: Rc<TypeClass>) -> Self {
        Self {
            super_type: Some(parent),
            ..self
        }
    }

    ///Returns a builder with a new generic parameter definition appended
    pub fn with_generic(mut self, name: &str, bound: ParameterizedType) -> Self {
        self.generic_parameters.push(GenericParam {
            name: name.to_string(),
            bound,
        });
        self
    }

    ///Returns a builder with the type super type's parameter at given index associated with given type
    pub fn with_association(mut self, parent_param_idx: usize, ty: Type) -> Self {
        self.associations.insert(parent_param_idx, ty);
        self
    }

    pub fn build(self, ctx: &mut TypeContext) -> Result<TypeClass, String> {
        TypeClass::from_builder(self, ctx)
    }
}

impl TypeClass {
    fn from_builder(
        definition: ClassTypeDefinition,
        ctx: &mut TypeContext,
    ) -> Result<Self, String> {
        let super_type = if let Some(st) = definition.super_type {
            st
        } else {
            ctx.use_class(&any().name)?
        };

        Self::contextualize_generics(definition.name.clone(), ctx, &definition.generic_parameters)?;

        let fqcn = ctx.fqn.appended(definition.name);

        let associations = Self::verify_associations(
            fqcn.clone(),
            super_type.clone(),
            ctx,
            definition.associations,
        )?;

        let def = Self {
            super_type: Some(super_type),
            generic_parameters: definition.generic_parameters,
            super_params_associations: associations,
            is_exported: definition.is_exported,
            fqcn,
        };

        Ok(def)
    }

    ///Defines in given context a class type for each given generic parameters
    fn contextualize_generics(
        name_prefix: Name,
        ctx: &mut TypeContext,
        generic_parameters: &Vec<GenericParam>,
    ) -> Result<(), String> {
        for generic in generic_parameters {
            let bound_cl = ctx.use_class(&generic.bound.name)?;
            let mut builder = ClassTypeDefinition::new(name_prefix.child(&generic.name))
                .with_super(bound_cl)
                .with_exported(false);

            for (idx, ty) in generic.bound.params.iter().enumerate() {
                builder = builder.with_association(idx, ty.clone());
            }

            ctx.define_class(builder)?;
        }
        Ok(())
    }

    fn verify_associations(
        self_name: Name,
        parent: Rc<TypeClass>,
        class_ctx: &mut TypeContext,
        associations: HashMap<usize, Type>,
    ) -> Result<Vec<TypeParamAssociation>, String> {
        //Ensure that all generic parameters are compatible with their associated parent's generics
        //This algorithm only look if the generics classes are subtype of parent's generics.

        let parent_gparam_count = parent.generic_parameters.len();

        if associations.len() != parent_gparam_count {
            return Err(format!("Type associations between {} and {} must match {}'s generic parameters count (got {}, expected {}).",
                               self_name, parent.fqcn, parent.fqcn, associations.len(), parent_gparam_count));
        }

        let mut validated_associations = vec![TypeParamAssociation::Nothing; parent_gparam_count];

        for (idx, parent_gparam) in parent.generic_parameters.iter().enumerate() {
            let association = associations.get(&idx).ok_or(format!(
                "No association set for parent generic parameter {}",
                parent.fqcn
            ))?;

            let association = match association {
                Type::Nothing => TypeParamAssociation::Nothing,

                Type::Parametrized(defined) => {
                    let class = class_ctx.use_class(&defined.name)?;
                    let bound_class = class_ctx.use_class(&parent_gparam.bound.name.clone())?;
                    if !class.is_subtype_of(bound_class) {
                        return Err(format!(
                            "type {} is not compatible with parent's generic parameter `{}`",
                            class.fqcn, parent_gparam
                        ));
                    }
                    TypeParamAssociation::Defined(class)
                }

                Type::Unknown => return Err("unexpected <unknown> type.".to_string()),
            };
            validated_associations[idx] = association
        }

        Ok(validated_associations)
    }

    pub fn get_common_parent(self: Rc<Self>, other: Rc<TypeClass>) -> Rc<TypeClass> {
        let mut self_lineage = Some(self.clone());

        //figure if self type is a subtype of other
        while let Some(self_lng) = self_lineage {
            let mut other_lineage = Some(other.clone());

            while let Some(other_lng) = other_lineage {
                if self_lng.fqcn == other_lng.fqcn {
                    return self_lng;
                }
                other_lineage = other_lng.super_type.clone()
            }

            self_lineage = self_lng.super_type.clone()
        }

        panic!(
            "cannot find common parent (does {} or {} extends Any ?)",
            self.fqcn, other.fqcn
        )
    }

    pub fn is_subtype_of(self: &Rc<Self>, other: Rc<TypeClass>) -> bool {
        let mut self_lineage = Some(self.clone());

        //figure if self type is a subtype of other
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
    use crate::lang_types::{any, str};
    use crate::types::context::TypeContext;
    use std::ops::Deref;

    use crate::name::Name;
    use crate::types::class::{ClassTypeDefinition, GenericParam, TypeClass, TypeParamAssociation};
    use crate::types::types::{ParameterizedType, Type};
    use pretty_assertions::assert_eq;

    #[test]
    #[ignore]
    fn define_iterable() {
        let mut std = TypeContext::new(Name::new("std"));

        let any_cl = std.use_class(&any().name).expect("error");

        let iterable_cl = std
            .define_class(ClassTypeDefinition::new(Name::new("Iterable")).with_generic("A", any()))
            .expect("error");

        assert_eq!(
            iterable_cl.deref(),
            &TypeClass {
                super_type: Some(any_cl.clone()),
                fqcn: Name::new("std::Iterable"),
                generic_parameters: vec![GenericParam {
                    bound: any(),
                    name: "A".to_string(),
                }],
                is_exported: true,
                super_params_associations: vec![],
            }
        );
    }

    #[test]
    #[ignore]
    fn define_list() {
        let mut std = TypeContext::new(Name::new("std"));

        let iterable_cl = std
            .define_class(ClassTypeDefinition::new(Name::new("Iterable")).with_generic("A", any()))
            .expect("error");

        let list_cl = std
            .define_class(
                ClassTypeDefinition::new(Name::new("List"))
                    .with_super(iterable_cl.clone())
                    .with_generic("B", any())
                    .with_association(0, Type::cons("List::B")),
            )
            .expect("error");

        assert_eq!(
            list_cl.deref(),
            &TypeClass {
                super_type: Some(iterable_cl.clone()),
                fqcn: Name::new("std::List"),
                generic_parameters: vec![GenericParam {
                    bound: any(),
                    name: "B".to_string(),
                }],
                is_exported: true,
                super_params_associations: vec![TypeParamAssociation::Defined(
                    std.use_class("List::B").expect("error")
                )],
            }
        );
    }

    #[test]
    #[ignore]
    fn define_map() {
        let mut lang = TypeContext::new(Name::new("lang"));
        let mut std = TypeContext::new(Name::new("std"));

        let any_cl = lang.use_class(&any().name).expect("error");

        let iterable_cl = TypeContext::define_class(
            &mut std,
            ClassTypeDefinition::new(Name::new("Iterable")).with_generic("A", any()),
        )
        .expect("error");

        let map_cl = TypeContext::define_class(
            &mut std,
            ClassTypeDefinition::new(Name::new("Map"))
                .with_super(iterable_cl.clone())
                .with_generic("K", any())
                .with_generic("V", any())
                .with_association(0, Type::cons("Map::K")),
        )
        .expect("error");

        let std_clone = std.clone();
        assert_eq!(
            std_clone,
            TypeContext::with_classes(
                [
                    (
                        Name::new("Iterable"),
                        TypeClass {
                            super_type: Some(any_cl.clone()),
                            fqcn: Name::new("std::Iterable"),
                            generic_parameters: vec![GenericParam {
                                name: "A".to_string(),
                                bound: any(),
                            }],
                            is_exported: true,
                            super_params_associations: vec![],
                        }
                    ),
                    (
                        Name::new("Iterable::A"),
                        TypeClass {
                            super_type: Some(any_cl.clone()),
                            fqcn: Name::new("std::Iterable::A"),
                            generic_parameters: vec![],
                            is_exported: false,
                            super_params_associations: vec![],
                        }
                    ),
                    (
                        Name::new("Map"),
                        TypeClass {
                            super_type: Some(iterable_cl.clone()),
                            fqcn: Name::new("std::Map"),
                            generic_parameters: vec![
                                GenericParam {
                                    name: "K".to_string(),
                                    bound: any(),
                                },
                                GenericParam {
                                    name: "V".to_string(),
                                    bound: any(),
                                }
                            ],
                            is_exported: true,
                            super_params_associations: vec![TypeParamAssociation::Defined(
                                std.use_class("Map::K").expect("error")
                            )],
                        }
                    ),
                    (
                        Name::new("Map::K"),
                        TypeClass {
                            super_type: Some(any_cl.clone()),
                            fqcn: map_cl.fqcn.child("K"),
                            generic_parameters: vec![],
                            is_exported: false,
                            super_params_associations: vec![],
                        }
                    ),
                    (
                        Name::new("Map::V"),
                        TypeClass {
                            super_type: Some(any_cl.clone()),
                            fqcn: map_cl.fqcn.child("V"),
                            generic_parameters: vec![],
                            is_exported: false,
                            super_params_associations: vec![],
                        }
                    )
                ],
                Name::new("std")
            ),
        );

        assert_eq!(
            map_cl.deref(),
            &TypeClass {
                fqcn: Name::new("std::Map"),
                super_type: Some(iterable_cl.clone()),
                generic_parameters: vec![
                    GenericParam {
                        bound: any(),
                        name: "K".to_string(),
                    },
                    GenericParam {
                        bound: any(),
                        name: "V".to_string(),
                    },
                ],
                is_exported: true,
                super_params_associations: vec![TypeParamAssociation::Defined(
                    std.use_class("Map::K").expect("error")
                ),],
            }
        );
    }

    #[test]
    #[ignore]
    fn define_str_option() {
        let mut std = TypeContext::new(Name::new("std"));
        let mut lang = TypeContext::new(Name::new("lang"));

        let str_cl = lang.use_class(&str().name).expect("error");
        let any_cl = lang.use_class(&any().name).expect("error");

        let option_cl = TypeContext::define_class(
            &mut std,
            ClassTypeDefinition::new(Name::new("Option")).with_generic("A", str()),
        )
        .expect("error");

        let some_cl = TypeContext::define_class(
            &mut std,
            ClassTypeDefinition::new(Name::new("Some"))
                .with_super(option_cl.clone())
                .with_generic("A", str())
                .with_association(0, Type::cons("Some::A")),
        )
        .expect("error");

        let none_cl = std
            .define_class(
                ClassTypeDefinition::new(Name::new("None"))
                    .with_super(option_cl.clone())
                    .with_association(0, Type::Nothing),
            )
            .expect("error");

        let std_clone = std.clone();

        assert_eq!(
            std_clone,
            TypeContext::with_classes(
                [
                    (
                        Name::new("Option::A"),
                        TypeClass {
                            super_type: Some(str_cl.clone()),
                            generic_parameters: vec![],
                            super_params_associations: vec![],
                            is_exported: false,
                            fqcn: Name::new("std::Option::A"),
                        }
                    ),
                    (
                        Name::new("Option"),
                        TypeClass {
                            super_type: Some(any_cl.clone()),
                            generic_parameters: vec![GenericParam::new("A", str())],
                            super_params_associations: vec![],
                            is_exported: true,
                            fqcn: Name::new("std::Option"),
                        }
                    ),
                    (
                        Name::new("Some::A"),
                        TypeClass {
                            super_type: Some(str_cl.clone()),
                            generic_parameters: vec![],
                            super_params_associations: vec![],
                            is_exported: false,
                            fqcn: Name::new("std::Some::A"),
                        }
                    ),
                    (
                        Name::new("Some"),
                        TypeClass {
                            super_type: Some(option_cl.clone()),
                            generic_parameters: vec![GenericParam::new("A", str())],
                            super_params_associations: vec![TypeParamAssociation::Defined(
                                std.use_class("Some::A").expect("error")
                            )],
                            is_exported: true,
                            fqcn: Name::new("std::Some"),
                        }
                    ),
                    (
                        Name::new("None"),
                        TypeClass {
                            super_type: Some(option_cl.clone()),
                            generic_parameters: vec![],
                            super_params_associations: vec![TypeParamAssociation::Nothing],
                            is_exported: true,
                            fqcn: Name::new("std::None"),
                        }
                    )
                ],
                Name::new("std"),
            ),
        );

        assert_eq!(
            some_cl.deref(),
            &TypeClass {
                super_type: Some(option_cl.clone()),
                generic_parameters: vec![GenericParam {
                    name: "A".to_string(),
                    bound: ParameterizedType::cons("Str"),
                }],
                super_params_associations: vec![TypeParamAssociation::Defined(
                    std.use_class("Some::A").expect("error")
                )],
                is_exported: true,
                fqcn: Name::new("std::Some"),
            }
        );

        assert_eq!(
            none_cl.deref(),
            &TypeClass {
                super_type: Some(option_cl.clone()),
                generic_parameters: vec![],
                super_params_associations: vec![TypeParamAssociation::Nothing],
                is_exported: true,
                fqcn: Name::new("std::None"),
            }
        );
    }

    #[test]
    #[ignore]
    fn define_type_with_inter_referenced_generics() {
        let mut std = TypeContext::new(Name::new("std"));
        let mut lang = TypeContext::new(Name::new("lang"));

        let str_cl = lang.use_class(&str().name).expect("error");
        let any_cl = lang.use_class(&any().name).expect("error");

        let list_cl = std
            .define_class(ClassTypeDefinition::new(Name::new("List")).with_generic("B", any()))
            .expect("error");

        //Equivalent to a `class Map[A: Str, B: List[A]] {}` statement
        let map_list_cl = std
            .define_class(
                ClassTypeDefinition::new(Name::new("Map"))
                    .with_generic("A", ParameterizedType::cons("Str"))
                    .with_generic(
                        "B",
                        ParameterizedType::parametrized("List", &[Type::cons("Map::A")]),
                    ),
            )
            .expect("error");

        let std_clone = std.clone();

        assert_eq!(
            std_clone,
            TypeContext::with_classes(
                [
                    (
                        Name::new("List::B"),
                        TypeClass {
                            super_type: Some(any_cl.clone()),
                            generic_parameters: vec![],
                            super_params_associations: vec![],
                            is_exported: false,
                            fqcn: Name::new("std::List::B"),
                        }
                    ),
                    (
                        Name::new("List"),
                        TypeClass {
                            fqcn: Name::new("std::List"),
                            super_type: Some(any_cl.clone()),
                            generic_parameters: vec![GenericParam {
                                name: "B".to_string(),
                                bound: any(),
                            }],
                            is_exported: true,
                            super_params_associations: vec![],
                        }
                    ),
                    (
                        Name::new("Map::B"),
                        TypeClass {
                            super_type: Some(list_cl),
                            generic_parameters: vec![],
                            super_params_associations: vec![TypeParamAssociation::Defined(
                                std.use_class("Map::A").expect("error")
                            )],
                            is_exported: false,
                            fqcn: Name::new("std::Map::B"),
                        }
                    ),
                    (
                        Name::new("Map::A"),
                        TypeClass {
                            fqcn: Name::new("std::Map::A"),
                            super_type: Some(str_cl),
                            generic_parameters: vec![],
                            is_exported: false,
                            super_params_associations: vec![],
                        }
                    ),
                    (
                        Name::new("Map"),
                        TypeClass {
                            fqcn: Name::new("std::Map"),
                            super_type: Some(any_cl.clone()),
                            generic_parameters: vec![
                                GenericParam {
                                    name: "A".to_string(),
                                    bound: str(),
                                },
                                GenericParam {
                                    name: "B".to_string(),
                                    bound: ParameterizedType::parametrized(
                                        "List",
                                        &[Type::cons("Map::A")]
                                    ),
                                }
                            ],
                            is_exported: true,
                            super_params_associations: vec![],
                        }
                    )
                ],
                Name::new("std"),
            ),
        );

        assert_eq!(
            map_list_cl.deref().clone(),
            TypeClass {
                super_type: Some(any_cl),
                generic_parameters: vec![
                    GenericParam {
                        name: "A".to_string(),
                        bound: ParameterizedType::cons("Str"),
                    },
                    GenericParam {
                        name: "B".to_string(),
                        bound: ParameterizedType::parametrized("List", &[Type::cons("Map::A")]),
                    },
                ],
                is_exported: true,
                super_params_associations: vec![],
                fqcn: Name::new("std::Map"),
            }
        );
    }

    #[test]
    #[ignore]
    fn define_incompatible_subtype() -> Result<(), String> {
        let mut std = TypeContext::new(Name::new("std"));

        let str_iterable_cl = std
            .define_class(
                ClassTypeDefinition::new(Name::new("StrIterable")).with_generic("A", str()),
            )
            .expect("error");

        let int_list_cl_res = std.define_class(
            ClassTypeDefinition::new(Name::new("IntList"))
                .with_super(str_iterable_cl.clone())
                .with_association(0, Type::cons("Int")),
        );

        assert_eq!(
            int_list_cl_res,
            Err(
                "type lang::Int is not compatible with parent's generic parameter `A: Str`"
                    .to_string()
            )
        );

        Ok(())
    }
}
