use crate::lang_types::any;
use crate::types::context::TypeContext;
use crate::types::types::{ParameterizedType, Type};
use context::display::fmt_comma_separated;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::ops::DerefMut;
use std::rc::Rc;
use crate::name::Name;
use crate::visibility::ScopeVisibility;

///This structures hosts the definition of a types,
#[derive(Clone, PartialEq)]
pub struct TypeClass {
    ///The super type of this type.
    pub super_type: Option<Rc<TypeClass>>,

    /// The bounds of the generic parameters of the class.
    pub generic_parameters: Vec<GenericParam>,

    pub visibility: ScopeVisibility,

    /// The associations between the child and parent type parameters.
    ///
    /// This vector must match the length of parent's type parameters.
    /// For instance:
    /// - `class List[A]: Iterable[A]`      The generic param of the child (`List`) is associated with the generic param of the parent (`Iterable`),
    ///                                     Thus, we have `List::A => Iterable::A`
    ///
    /// - `class IdentityMap[A]: Map[A, A]` Here, we have `IdentityMap::A => Map::K` and `IdentityMap::A => Map::V`
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
            .field("visibility", &self.visibility)
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
    visibility: ScopeVisibility,
    associations: HashMap<usize, Type>,
}

impl ClassTypeDefinition {
    pub(crate) fn new(name: Name) -> Self {
        Self {
            name: name.clone(),
            generic_parameters: Vec::new(),
            associations: HashMap::new(),
            visibility: ScopeVisibility::Public,
            super_type: None,
        }
    }

    pub fn with_visibility(self, visibility: ScopeVisibility) -> Self {
        Self {
            visibility,
            ..self
        }
    }

    pub fn with_super(self, parent: Rc<TypeClass>) -> Self {
        Self {
            super_type: Some(parent),
            ..self
        }
    }

    pub fn with_generic(mut self, name: &str, bound: ParameterizedType) -> Self {
        self.generic_parameters.push(GenericParam {
            name: name.to_string(),
            bound,
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
    fn from_builder(
        definition: ClassTypeDefinition,
        ctx: Rc<RefCell<TypeContext>>,
    ) -> Result<Self, String> {
        let super_type = if let Some(st) = definition.super_type {
            st
        } else {
            ctx.borrow_mut().use_class(&any().name)?
        };

        Self::contextualize_generics(definition.name.clone(), ctx.clone(), &definition.generic_parameters)?;

        let fqcn = ctx.borrow().fqn.appended(definition.name);

        let associations = Self::verify_associations(
            fqcn.clone(),
            super_type.clone(),
            ctx.borrow_mut().deref_mut(),
            definition.associations,
        )?;

        let def = Self {
            super_type: Some(super_type),
            generic_parameters: definition.generic_parameters,
            super_params_associations: associations,
            visibility: definition.visibility,
            fqcn,
        };

        Ok(def)
    }

    ///Defines in given context a class type for each given generic parameters
    fn contextualize_generics(
        name_prefix: Name,
        ctx: Rc<RefCell<TypeContext>>,
        generic_parameters: &Vec<GenericParam>,
    ) -> Result<(), String> {
        for generic in generic_parameters {
            let bound_cl = ctx.borrow_mut().use_class(&generic.bound.name)?;
            let mut builder =
                ClassTypeDefinition::new(name_prefix.child(&generic.name))
                    .with_super(bound_cl)
                    .with_visibility(ScopeVisibility::SymbolOnly {symbol: name_prefix.clone()});


            for (idx, ty) in generic.bound.params.iter().enumerate() {
                builder = builder.with_association(idx, ty.clone());
            }

            TypeContext::define_class(ctx.clone(), builder)?;
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
    use crate::types::context::{TypeContext};
    use std::ops::Deref;

    use crate::types::class::{ClassTypeDefinition, GenericParam, TypeParamAssociation, TypeClass};
    use crate::types::types::{ParameterizedType, Type};
    use pretty_assertions::assert_eq;
    use crate::name::Name;
    use crate::import_engine::ImportEngine;
    use crate::layers::ModuleLayers;
    use crate::visibility::ScopeVisibility;
    use crate::visibility::ScopeVisibility::{Public, SymbolOnly};

    #[test]
    fn define_iterable() {
        let layers = ModuleLayers::new();

        let std = ModuleLayers::declare_env(layers.clone(), Name::new("std")).expect("error")
            .borrow()
            .type_context
            .clone();

        let any_cl = std.borrow_mut()
            .use_class(&any().name)
            .expect("error");

        let iterable_cl = TypeContext::define_class(
            std.clone(),
            ClassTypeDefinition::new(Name::new("Iterable"))
                .with_generic("A", any())
                .with_visibility(Public),
        ).expect("error");

        assert_eq!(
            iterable_cl.deref(),
            &TypeClass {
                super_type: Some(any_cl.clone()),
                fqcn: Name::new("std::Iterable"),
                generic_parameters: vec![GenericParam {
                    bound: any(),
                    name: "A".to_string(),
                }],
                visibility: Public,
                super_params_associations: vec![],
            }
        );
    }

    #[test]
    fn define_list() {
        let layers = ModuleLayers::new();

        let std = ModuleLayers::declare_env(layers.clone(), Name::new("std")).expect("error")
            .borrow()
            .type_context
            .clone();

        let iterable_cl = TypeContext::define_class(
            std.clone(),
            ClassTypeDefinition::new(Name::new("Iterable")).with_generic("A", any()),
        ).expect("error");

        let list_cl = TypeContext::define_class(
            std.clone(),
            ClassTypeDefinition::new(Name::new("List"))
                .with_super(iterable_cl.clone())
                .with_generic("B", any())
                .with_association(0, Type::cons("List::B")),
        ).expect("error");

        assert_eq!(
            list_cl.deref(),
            &TypeClass {
                super_type: Some(iterable_cl.clone()),
                fqcn: Name::new("std::List"),
                generic_parameters: vec![GenericParam {
                    bound: any(),
                    name: "B".to_string(),
                }],
                visibility: Public,
                super_params_associations: vec![TypeParamAssociation::Defined(
                    std
                        .borrow_mut()
                        .use_class("List::B").expect("error")
                )],
            }
        );
    }

    #[test]
    fn define_map() {
        let layers = ModuleLayers::new();

        let std = ModuleLayers::declare_env(layers.clone(), Name::new("std")).expect("error")
            .borrow()
            .type_context
            .clone();

        let lang = layers.borrow().get_env(&Name::new("lang")).unwrap();

        let any_cl = lang.borrow().type_context.borrow_mut().use_class(&any().name).expect("error");

        let iterable_cl = TypeContext::define_class(
            std.clone(),
            ClassTypeDefinition::new(Name::new("Iterable"))
                .with_generic("A", any()),
        ).expect("error");

        let map_cl = TypeContext::define_class(
            std.clone(),
            ClassTypeDefinition::new(Name::new("Map"))
                .with_super(iterable_cl.clone())
                .with_generic("K", any())
                .with_generic("V", any())
                .with_association(0, Type::cons("Map::K")),
        ).expect("error");

        let std_clone = std.clone().borrow().clone();
        assert_eq!(
            std_clone,
            TypeContext::with_classes(
                [(
                    Name::new("Iterable"),
                    TypeClass {
                        super_type: Some(any_cl.clone()),
                        fqcn: Name::new("std::Iterable"),
                        generic_parameters: vec![GenericParam {
                            name: "A".to_string(),
                            bound: any(),
                        }],
                        visibility: Public,
                        super_params_associations: vec![],
                    }
                ), (
                    Name::new("Iterable::A"),
                    TypeClass {
                        super_type: Some(any_cl.clone()),
                        fqcn: Name::new("std::Iterable::A"),
                        generic_parameters: vec![],
                        visibility: ScopeVisibility::SymbolOnly {symbol: Name::new("Iterable")},
                        super_params_associations: vec![],
                    }
                ), (
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
                            }],
                        visibility: Public,
                        super_params_associations: vec![
                            TypeParamAssociation::Defined(std.borrow_mut().use_class("Map::K").expect("error"))
                        ],
                    }
                ), (
                    Name::new("Map::K"),
                    TypeClass {
                        super_type: Some(any_cl.clone()),
                        fqcn: map_cl.fqcn.child("K"),
                        generic_parameters: vec![],
                        visibility: ScopeVisibility::SymbolOnly {symbol: Name::new("Map")},
                        super_params_associations: vec![],
                    }
                ), (
                    Name::new("Map::V"),
                    TypeClass {
                        super_type: Some(any_cl.clone()),
                        fqcn: map_cl.fqcn.child("V"),
                        generic_parameters: vec![],
                        visibility: ScopeVisibility::SymbolOnly {symbol: Name::new("Map")},
                        super_params_associations: vec![],
                    }
                )],
                Name::new("std"),
                ImportEngine::new(layers).read_only()),
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
                visibility: Public,
                super_params_associations: vec![TypeParamAssociation::Defined(
                    std
                        .borrow_mut()
                        .use_class("Map::K").expect("error")
                ), ],
            }
        );
    }

    #[test]
    fn define_str_option() {
        let layers = ModuleLayers::new();

        let std = ModuleLayers::declare_env(layers.clone(), Name::new("std")).expect("error")
            .borrow()
            .type_context
            .clone();

        let lang = layers.borrow().get_env(&Name::new("lang")).unwrap();

        let str_cl = lang.borrow().type_context.borrow_mut().use_class(&str().name).expect("error");
        let any_cl = lang.borrow().type_context.borrow_mut().use_class(&any().name).expect("error");

        let option_cl = TypeContext::define_class(
            std.clone(),
            ClassTypeDefinition::new(Name::new("Option"))
                .with_generic("A", str()),
        ).expect("error");

        let some_cl = TypeContext::define_class(
            std.clone(),
            ClassTypeDefinition::new(Name::new("Some"))
                .with_super(option_cl.clone())
                .with_generic("A", str())
                .with_association(0, Type::cons("Some::A")),
        ).expect("error");

        let none_cl = TypeContext::define_class(
            std.clone(),
            ClassTypeDefinition::new(Name::new("None"))
                .with_super(option_cl.clone())
                .with_association(0, Type::Nothing),
        ).expect("error");

        let std_clone = std.clone().borrow().clone();

        assert_eq!(
            std_clone,
            TypeContext::with_classes(
                [(
                    Name::new("Option::A"),
                    TypeClass {
                        super_type: Some(str_cl.clone()),
                        generic_parameters: vec![],
                        super_params_associations: vec![],
                        visibility: SymbolOnly {symbol: Name::new("Option")},
                        fqcn: Name::new("std::Option::A"),
                    }
                ), (
                    Name::new("Option"),
                    TypeClass {
                        super_type: Some(any_cl.clone()),
                        generic_parameters: vec![GenericParam::new("A", str())],
                        super_params_associations: vec![],
                        visibility: Public,
                        fqcn: Name::new("std::Option"),
                    }
                ), (
                    Name::new("Some::A"),
                    TypeClass {
                        super_type: Some(str_cl.clone()),
                        generic_parameters: vec![],
                        super_params_associations: vec![],
                        visibility: SymbolOnly {symbol: Name::new("Some")},
                        fqcn: Name::new("std::Some::A"),
                    }
                ), (
                    Name::new("Some"),
                    TypeClass {
                        super_type: Some(option_cl.clone()),
                        generic_parameters: vec![GenericParam::new("A", str())],
                        super_params_associations: vec![
                            TypeParamAssociation::Defined(std.borrow_mut().use_class("Some::A").expect("error"))
                        ],
                        visibility: Public,
                        fqcn: Name::new("std::Some"),
                    }
                ), (
                    Name::new("None"),
                    TypeClass {
                        super_type: Some(option_cl.clone()),
                        generic_parameters: vec![],
                        super_params_associations: vec![
                            TypeParamAssociation::Nothing
                        ],
                        visibility: Public,
                        fqcn: Name::new("std::None"),
                    }
                )],
                Name::new("std"),
                ImportEngine::new(layers.clone()).read_only(),
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
                    std
                        .borrow_mut()
                        .use_class("Some::A").expect("error")
                )],
                visibility: Public,
                fqcn: Name::new("std::Some"),
            }
        );

        assert_eq!(
            none_cl.deref(),
            &TypeClass {
                super_type: Some(option_cl.clone()),
                generic_parameters: vec![],
                super_params_associations: vec![TypeParamAssociation::Nothing],
                visibility: Public,
                fqcn: Name::new("std::None"),
            }
        );
    }

    #[test]
    fn define_type_with_inter_referenced_generics() {
        let layers = ModuleLayers::new();

        let std = ModuleLayers::declare_env(layers.clone(), Name::new("std")).expect("error")
            .borrow()
            .type_context
            .clone();

        let lang = layers.borrow().get_env(&Name::new("lang")).unwrap();

        let str_cl = lang.borrow().type_context.borrow_mut().use_class(&str().name).expect("error");
        let any_cl = lang.borrow().type_context.borrow_mut().use_class(&any().name).expect("error");


        let list_cl = TypeContext::define_class(
            std.clone(),
            ClassTypeDefinition::new(Name::new("List")).with_generic("B", any()),
        ).expect("error");

        //Equivalent to a `class Map[A: Str, B: List[A]] {}` statement
        let map_list_cl = TypeContext::define_class(
            std.clone(),
            ClassTypeDefinition::new(Name::new("Map"))
                .with_generic("A", ParameterizedType::cons("Str"))
                .with_generic("B", ParameterizedType::parametrized("List", &[Type::cons("Map::A")])),
        ).expect("error");

        let std_clone = std.clone().borrow().clone();

        assert_eq!(
            std_clone,
            TypeContext::with_classes(
                [(
                    Name::new("List::B"),
                    TypeClass {
                        super_type: Some(any_cl.clone()),
                        generic_parameters: vec![],
                        super_params_associations: vec![],
                        visibility: SymbolOnly {symbol: Name::new("List")},
                        fqcn: Name::new("std::List::B"),
                    }
                ), (
                    Name::new("List"),
                    TypeClass {
                        fqcn: Name::new("std::List"),
                        super_type: Some(any_cl.clone()),
                        generic_parameters: vec![GenericParam {
                            name: "B".to_string(),
                            bound: any(),
                        }],
                        visibility: Public,
                        super_params_associations: vec![],
                    }
                ), (
                    Name::new("Map::B"),
                    TypeClass {
                        super_type: Some(list_cl),
                        generic_parameters: vec![],
                        super_params_associations: vec![TypeParamAssociation::Defined(
                            std
                                .borrow_mut()
                                .use_class("Map::A").expect("error")
                        )],
                        visibility: SymbolOnly {symbol: Name::new("Map")},
                        fqcn: Name::new("std::Map::B"),
                    }
                ), (
                    Name::new("Map::A"),
                    TypeClass {
                        fqcn: Name::new("std::Map::A"),
                        super_type: Some(str_cl),
                        generic_parameters: vec![],
                        visibility: SymbolOnly {symbol: Name::new("Map")},
                        super_params_associations: vec![],
                    }
                ), (
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
                                bound: ParameterizedType::parametrized("List", &[Type::cons("Map::A")]),
                            }],
                        visibility: Public,
                        super_params_associations: vec![],
                    }
                )],
                Name::new("std"),
                ImportEngine::new(layers).read_only()),
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
                visibility: Public,
                super_params_associations: vec![],
                fqcn: Name::new("std::Map"),
            }
        );
    }

    #[test]
    fn define_incompatible_subtype() -> Result<(), String> {
        let layers = ModuleLayers::new();

        let std = ModuleLayers::declare_env(layers.clone(), Name::new("std")).expect("error")
            .borrow()
            .type_context
            .clone();

        let str_iterable_cl = TypeContext::define_class(
            std.clone(),
            ClassTypeDefinition::new(Name::new("StrIterable")).with_generic("A", str()),
        ).expect("error");

        let int_list_cl_res = TypeContext::define_class(
            std.clone(),
            ClassTypeDefinition::new(Name::new("IntList"))
                .with_super(str_iterable_cl.clone())
                .with_association(0, Type::cons("Int")),
        );

        assert_eq!(
            int_list_cl_res,
            Err("type lang::Int is not compatible with parent's generic parameter `A: Str`".to_string())
        );

        Ok(())
    }
}
