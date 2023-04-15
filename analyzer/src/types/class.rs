use crate::lang_types::any;
use crate::types::context::TypeContext;
use crate::types::types::{DefinedType, Type};
use context::display::fmt_comma_separated;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::rc::Rc;
use crate::identity::Identity;

///This structures hosts the definition of a types,
#[derive(Clone, PartialEq)]
pub struct TypeClass {
    ///The super type of this type.
    pub super_type: Option<Rc<TypeClass>>,

    /// The bounds of the generic parameters of the class.
    pub generic_parameters: Vec<GenericParam>,

    /// The associations between the child and parent type parameters.
    ///
    /// This vector must match the length of parent's type parameters.
    /// For instance:
    /// - `class List[A]: Iterable[A]`   The generic param of the child (`List`) is associated with the generic param of the parent (`Iterable`),
    ///                                  Thus, we have `List::A => Iterable::A`
    ///
    /// - `class WeirdMap[A]: Map[A, A]` Here, we have `WeirdMap::A => Map::K` and `WeirdMap::A => Map::V`
    ///
    /// - `class Map[K, V]: Iterable[A]` This defines a map that is iterable only over its keys, the first map's generic parameter `K`
    ///                                  is then bound on lone iterable's generic parameter, and the generic param `V` has no links with the parent,
    ///                                  Thus we only have `Map::K => Iterable::A` as an association set.
    ///
    /// - `class StrMap[V]: Map[Str, V]` Here, the associations are `lang::Str => Map::K` and `StrMap::V => Map::V`
    pub(in crate::types) super_params_associations: Vec<ParamAssociation>,

    /// The fully qualified class name.
    pub(crate) fqcn: Identity,

    /// The type context of this type.
    ///
    /// The context is guaranteed to own the definition of the generic parameters.
    pub context: Rc<RefCell<TypeContext>>,
}

impl Debug for TypeClass {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TypeClass")
            .field("identity", &self.fqcn)
            .field("super_type", &self.super_type)
            .field("generic_parameters", &self.generic_parameters)
            .field("super_params_associations", &self.super_params_associations)
            .field("identity", &self.fqcn)
            .field("context", &"<not shown>")
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
pub enum ParamAssociation {
    ///The association is a known type class (can be a generic)
    Defined(Rc<TypeClass>),
    ///The association is nothing
    Nothing,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GenericParam {
    pub name: String,
    pub bound: DefinedType,
}

impl Display for GenericParam {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.bound)
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

    pub fn with_super(self, parent: Rc<TypeClass>) -> Self {
        Self {
            super_type: Some(parent),
            ..self
        }
    }

    pub fn with_generic(mut self, name: &str, bound: DefinedType) -> Self {
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
        parent_ctx: Rc<RefCell<TypeContext>>,
    ) -> Result<Self, String> {
        let super_type = if let Some(st) = definition.super_type {
            st
        } else {
            parent_ctx.borrow().lookup_defined(&any())?
        };

        let fqcn = parent_ctx.borrow().identity.child(&definition.name);
        let class_context = Rc::new(RefCell::new(TypeContext::fork(parent_ctx.clone(), &fqcn.name)));

        Self::contextualize(class_context.clone(), &definition.generic_parameters)?;

        let associations = Self::verify_associations(
            definition.name.clone(),
            super_type.clone(),
            &class_context.borrow(),
            definition.associations,
        )?;


        let def = Self {
            super_type: Some(super_type),
            generic_parameters: definition.generic_parameters,
            super_params_associations: associations,
            fqcn,
            context: class_context,
        };

        Ok(def)
    }

    ///Defines in given context a class type for each given generic parameters
    fn contextualize(
        ctx: Rc<RefCell<TypeContext>>,
        generic_parameters: &Vec<GenericParam>,
    ) -> Result<(), String> {
        for generic in generic_parameters {
            let sup = ctx.borrow().lookup_defined(&generic.bound)?;
            let mut builder = ClassTypeDefinition::new(&generic.name).with_super(sup);

            match &generic.bound {
                DefinedType::Parameterized(p) => {
                    for (idx, ty) in p.params.iter().enumerate() {
                        builder = builder.with_association(idx, ty.clone());
                    }
                }
            }

            TypeContext::define_class(ctx.clone(), builder)?;
        }
        Ok(())
    }

    fn verify_associations(
        self_name: String,
        parent: Rc<TypeClass>,
        class_ctx: &TypeContext,
        associations: HashMap<usize, Type>,
    ) -> Result<Vec<ParamAssociation>, String> {
        //Ensure that all generic parameters are compatible with their associated parent's generics
        //This algorithm only look if the generics classes are subtype of parent's generics.

        let parent_gparam_count = parent.generic_parameters.len();

        if associations.len() != parent_gparam_count {
            return Err(format!("Type associations between {} and {} must match {}'s generic parameters count (got {}, expected {}).",
                               self_name, parent.fqcn, parent.fqcn, associations.len(), parent_gparam_count));
        }

        let mut validated_associations = vec![ParamAssociation::Nothing; parent_gparam_count];

        for (idx, parent_gparam) in parent.generic_parameters.iter().enumerate() {
            let association = associations.get(&idx).ok_or(format!(
                "No association set for parent generic parameter {}",
                parent.fqcn
            ))?;

            let association = match association {
                Type::Nothing => ParamAssociation::Nothing,

                Type::Defined(defined) => {
                    let class = class_ctx.lookup_defined(&defined.clone())?;
                    let bound_class = class_ctx.lookup_defined(&parent_gparam.bound.clone())?;
                    if !class.is_subtype_of(bound_class) {
                        return Err(format!(
                            "type {} is not compatible with parent's generic parameter `{}`",
                            class.fqcn, parent_gparam
                        ));
                    }
                    ParamAssociation::Defined(class)
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
    use std::cell::RefCell;
    use std::ops::Deref;
    use std::rc::Rc;

    use crate::types::class::{ClassTypeDefinition, GenericParam, ParamAssociation, TypeClass};
    use crate::types::types::{DefinedType, Type};
    use pretty_assertions::assert_eq;
    use crate::identity::Identity;
    use crate::import_engine::ImportEngine;
    use crate::imports::ModuleImport;
    use crate::module::ModuleLayers;

    #[test]
    fn define_iterable() -> Result<(), String> {
        let layers = ModuleLayers::new();

        let std = ModuleLayers::declare_env(layers.clone(), Identity::new("std")?)?
            .borrow()
            .type_context
            .clone();
        std.borrow_mut().add_import(ModuleImport::all(Identity::new("lang")?));

        let any_cl = std.borrow().lookup_defined(&any())?;

        let iterable_cl = TypeContext::define_class(
            std.clone(),
            ClassTypeDefinition::new("Iterable").with_generic("A", any()),
        )?;

        assert_eq!(
            iterable_cl.deref(),
            &TypeClass {
                super_type: Some(any_cl.clone()),
                fqcn: Identity::new("std::Iterable")?,
                generic_parameters: vec![GenericParam {
                    bound: any(),
                    name: "A".to_string(),
                }],
                super_params_associations: vec![],
                context: iterable_cl.context.clone(),
            }
        );
        Ok(())
    }

    #[test]
    fn define_list() -> Result<(), String> {
        let layers = ModuleLayers::new();

        let std = ModuleLayers::declare_env(layers.clone(), Identity::new("std")?)?
            .borrow()
            .type_context
            .clone();
        std.borrow_mut().add_import(ModuleImport::all(Identity::new("lang")?));

        let iterable_cl = TypeContext::define_class(
            std.clone(),
            ClassTypeDefinition::new("Iterable").with_generic("A", any()),
        )?;

        let list_cl = TypeContext::define_class(
            std.clone(),
            ClassTypeDefinition::new("List")
                .with_super(iterable_cl.clone())
                .with_generic("B", any())
                .with_association(0, Type::cons("B")),
        )?;

        assert_eq!(
            list_cl.deref(),
            &TypeClass {
                super_type: Some(iterable_cl.clone()),
                fqcn: Identity::new("std::List")?,
                generic_parameters: vec![GenericParam {
                    bound: any(),
                    name: "B".to_string(),
                }],
                super_params_associations: vec![ParamAssociation::Defined(
                    list_cl
                        .context
                        .borrow()
                        .lookup_defined(&DefinedType::cons("B"))?
                )],
                context: list_cl.context.clone(),
            }
        );
        Ok(())
    }

    #[test]
    fn define_map() -> Result<(), String> {
        let layers = ModuleLayers::new();

        let std = ModuleLayers::declare_env(layers.clone(), Identity::new("std")?)?
            .borrow()
            .type_context
            .clone();
        std.borrow_mut().add_import(ModuleImport::all(Identity::new("lang")?));

        let lang = layers.borrow().get_env(Identity::new("lang")?).unwrap();

        let any_cl = lang.borrow().type_context.borrow().lookup_defined(&any())?;

        let iterable_cl = TypeContext::define_class(
            std.clone(),
            ClassTypeDefinition::new("Iterable").with_generic("A", any()),
        )?;

        let map_cl = TypeContext::define_class(
            std.clone(),
            ClassTypeDefinition::new("Map")
                .with_super(iterable_cl.clone())
                .with_generic("K", any())
                .with_generic("V", any())
                .with_association(0, Type::cons("K")),
        )?;

        assert_eq!(
            map_cl.context.borrow().clone(),
            TypeContext::with_classes(
                [(
                    "K".to_string(),
                    TypeClass {
                        super_type: Some(any_cl.clone()),
                        fqcn: map_cl.fqcn.child("K"),
                        generic_parameters: vec![],
                        super_params_associations: vec![],
                        context: Rc::new(RefCell::new(Default::default())),
                    }
                ), (
                    "V".to_string(),
                    TypeClass {
                        super_type: Some(any_cl.clone()),
                        fqcn: map_cl.fqcn.child("V"),
                        generic_parameters: vec![],
                        super_params_associations: vec![],
                        context: Rc::new(RefCell::new(Default::default())),
                    }
                )],
                Identity::new("std::Map")?,
                ImportEngine::new(vec![ModuleImport::all(Identity::new("lang")?)], layers)),
        );

        assert_eq!(
            map_cl.deref(),
            &TypeClass {
                super_type: Some(iterable_cl.clone()),
                generic_parameters: vec![
                    GenericParam {
                        bound: any(),
                        name: "K".to_string(),
                    },
                    GenericParam {
                        bound: any(),
                        name: "V".to_string(),
                    }
                ],
                super_params_associations: vec![ParamAssociation::Defined(
                    map_cl
                        .context
                        .borrow()
                        .lookup_defined(&DefinedType::cons("K"))?
                ), ],
                fqcn: Identity::new("std::Map")?,
                context: map_cl.context.clone(),
            }
        );
        Ok(())
    }

    #[test]
    fn define_str_option() -> Result<(), String> {
        let layers = ModuleLayers::new();

        let std = ModuleLayers::declare_env(layers.clone(), Identity::new("std")?)?
            .borrow()
            .type_context
            .clone();
        std.borrow_mut().add_import(ModuleImport::all(Identity::new("lang")?));

        let lang = layers.borrow().get_env(Identity::new("lang")?).unwrap();

        let str_cl = lang.borrow().type_context.borrow().lookup_defined(&str())?;

        let option_cl = TypeContext::define_class(
            std.clone(),
            ClassTypeDefinition::new("Option").with_generic("A", DefinedType::cons("Str")),
        )?;

        let some_cl = TypeContext::define_class(
            std.clone(),
            ClassTypeDefinition::new("Some")
                .with_super(option_cl.clone())
                .with_generic("A", DefinedType::cons("Str"))
                .with_association(0, Type::cons("A")),
        )?;

        let none_cl = TypeContext::define_class(
            std.clone(),
            ClassTypeDefinition::new("None")
                .with_super(option_cl.clone())
                .with_association(0, Type::Nothing),
        )?;

        assert_eq!(
            some_cl.context.borrow().clone(),
            TypeContext::with_classes(
                [(
                    "A".to_string(),
                    TypeClass {
                        super_type: Some(str_cl.clone()),
                        generic_parameters: vec![],
                        super_params_associations: vec![],
                        fqcn: Identity::new("std::Some::A")?,
                        context: Rc::new(RefCell::new(Default::default())),
                    }
                )],
                Identity::new("std::Some")?,
                ImportEngine::new(vec![ModuleImport::all(Identity::new("lang")?)], layers.clone()),
            ),
        );
        assert_eq!(
            none_cl.context.borrow().clone(),
            TypeContext::new(Identity::new("std::None")?, layers)
        );

        assert_eq!(
            some_cl.deref(),
            &TypeClass {
                super_type: Some(option_cl.clone()),
                generic_parameters: vec![GenericParam {
                    name: "A".to_string(),
                    bound: DefinedType::cons("Str"),
                }],
                super_params_associations: vec![ParamAssociation::Defined(
                    some_cl
                        .context
                        .borrow()
                        .lookup_defined(&DefinedType::cons("A"))?
                ), ],
                fqcn: Identity::new("std::Some")?,
                context: some_cl.context.clone(),
            }
        );

        assert_eq!(
            none_cl.deref(),
            &TypeClass {
                super_type: Some(option_cl.clone()),
                generic_parameters: vec![],
                super_params_associations: vec![ParamAssociation::Nothing],
                fqcn: Identity::new("std::None")?,
                context: none_cl.context.clone(),
            }
        );
        Ok(())
    }

    #[test]
    fn define_type_with_inter_referenced_generics() -> Result<(), String> {
        let layers = ModuleLayers::new();

        let std = ModuleLayers::declare_env(layers.clone(), Identity::new("std")?)?
            .borrow()
            .type_context
            .clone();
        std.borrow_mut().add_import(ModuleImport::all(Identity::new("lang")?));

        let lang = layers.borrow().get_env(Identity::new("lang")?).unwrap();

        let str_cl = lang.borrow().type_context.borrow().lookup_defined(&str())?;
        let any_cl = lang.borrow().type_context.borrow().lookup_defined(&any())?;


        let list_cl = TypeContext::define_class(
            std.clone(),
            ClassTypeDefinition::new("List").with_generic("B", any()),
        )?;

        //Equivalent to a `class Map[A: Str, B: List[A]] {}` statement
        let map_list_cl = TypeContext::define_class(
            std.clone(),
            ClassTypeDefinition::new("Map")
                .with_generic("A", DefinedType::cons("Str"))
                .with_generic("B", DefinedType::parametrized("List", &[Type::cons("A")])),
        )?;

        assert_eq!(
            map_list_cl.context.borrow().clone(),
            TypeContext::with_classes(
                [(
                    "B".to_string(),
                    TypeClass {
                        super_type: Some(list_cl),
                        generic_parameters: vec![],
                        super_params_associations: vec![ParamAssociation::Defined(
                            map_list_cl
                                .context
                                .borrow()
                                .lookup_defined(&DefinedType::cons("A"))?
                        )],
                        fqcn: Identity::new("std::Map::B")?,
                        context: Rc::new(RefCell::new(Default::default())),
                    }
                ), (
                    "A".to_string(),
                    TypeClass {
                        super_type: Some(str_cl),
                        generic_parameters: vec![],
                        super_params_associations: vec![],
                        fqcn: Identity::new("std::Map::A")?,
                        context: Rc::new(RefCell::new(Default::default())),
                    }
                )],
                Identity::new("std::Map")?,
                ImportEngine::new(vec![ModuleImport::all(Identity::new("lang")?)], layers)),
        );

        assert_eq!(
            map_list_cl.deref().clone(),
            TypeClass {
                super_type: Some(any_cl),
                generic_parameters: vec![
                    GenericParam {
                        name: "A".to_string(),
                        bound: DefinedType::cons("Str"),
                    },
                    GenericParam {
                        name: "B".to_string(),
                        bound: DefinedType::parametrized("List", &[Type::cons("A")]),
                    },
                ],
                super_params_associations: vec![],
                fqcn: Identity::new("std::Map")?,
                context: map_list_cl.context.clone(),
            }
        );

        Ok(())
    }

    #[test]
    fn define_incompatible_subtype() -> Result<(), String> {
        let layers = ModuleLayers::new();

        let std = ModuleLayers::declare_env(layers.clone(), Identity::new("std")?)?
            .borrow()
            .type_context
            .clone();
        std.borrow_mut().add_import(ModuleImport::all(Identity::new("lang")?));

        let str_iterable_cl = TypeContext::define_class(
            std.clone(),
            ClassTypeDefinition::new("StrIterable").with_generic("A", str()),
        )?;

        let int_list_cl_res = TypeContext::define_class(
            std.clone(),
            ClassTypeDefinition::new("IntList")
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
