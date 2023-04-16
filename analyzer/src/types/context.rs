use crate::types::class::{ClassTypeDefinition, TypeClass};
use crate::types::types::{Type};
use std::cell::RefCell;
use std::collections::hash_map::{Entry};
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::rc::Rc;
use crate::environment::{Environment, EnvironmentContext};

use crate::identity::Name;
use crate::import_engine::ImportEngine;
use crate::imports::ModuleImport;
use crate::layers::ModuleLayers;


/// A type environment.
///
/// Contexts track substitutions and generate fresh type variables.
#[derive(Clone, Default)]
pub struct TypeContext {
    /// Records the type of each class by their basename.
    classes: HashMap<Name, Rc<TypeClass>>,

    imports: ImportEngine,

    pub identity: Name,
}

impl PartialEq for TypeContext {
    fn eq(&self, other: &Self) -> bool {
        self.classes == other.classes
    }
}

impl EnvironmentContext<Rc<TypeClass>> for TypeContext {
    fn from_env(env: &Environment) -> Rc<RefCell<Self>> {
        env.type_context.clone()
    }

    fn find(&self, name: &Name) -> Option<Rc<TypeClass>> {
        self.classes.get(name).cloned()
    }
}

impl Debug for TypeContext {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut classes: Vec<_> = self.classes.iter().collect();
        classes.sort_by_key(|(k, _)| k.clone());

        f.debug_struct("TypeContext").field("identity", &self.identity)
            .field("imports", &self.imports)
            .field("classes", &classes)
            .finish()
    }
}


impl TypeContext {
    pub(crate) fn with_classes<const T: usize>(classes: [(Name, TypeClass); T],
                                               identity: Name,
                                               imports: ImportEngine) -> Self {
        let classes = classes
            .into_iter()
            .map(|(k, v)| (k, Rc::new(v)))
            .collect();
        Self {
            classes,
            imports,
            identity,
        }
    }

    pub(crate) fn new(identity: Name, layers: Rc<RefCell<ModuleLayers>>) -> Self {
        Self {
            identity,
            imports: ImportEngine::new(layers),
            classes: HashMap::new(),
        }
    }

    ///Definitions of the lang type context.
    pub fn lang(layers: Rc<RefCell<ModuleLayers>>) -> Rc<RefCell<Self>> {
        let ctx = TypeContext::new(
            Name::new("lang"),
            layers,
        );
        let ctx_rc = Rc::new(RefCell::new(ctx));
        let mut ctx = ctx_rc.borrow_mut();

        const MSG: &str = "lang type registration";

        let any_cl = &Rc::new(TypeClass {
            super_type: None,
            generic_parameters: vec![],
            super_params_associations: vec![],
            fqcn: ctx.identity.child("Any"),
        });
        ctx.classes.insert(Name::new(&any_cl.fqcn.name), any_cl.clone());
        drop(ctx);

        let float =
            Self::define_class(ctx_rc.clone(), ClassTypeDefinition::new(Name::new("Float"))).expect(MSG);

        Self::define_class(ctx_rc.clone(), ClassTypeDefinition::new(Name::new("Bool"))).expect(MSG);
        Self::define_class(ctx_rc.clone(), ClassTypeDefinition::new(Name::new("Str"))).expect(MSG);
        Self::define_class(ctx_rc.clone(), ClassTypeDefinition::new(Name::new("Unit"))).expect(MSG);

        let int = Self::define_class(
            ctx_rc.clone(),
            ClassTypeDefinition::new(Name::new("Int")).with_super(float),
        )
            .expect(MSG);

        Self::define_class(
            ctx_rc.clone(),
            ClassTypeDefinition::new(Name::new("Exitcode")).with_super(int),
        )
            .expect(MSG);

        ctx_rc
    }

    pub(crate) fn fork(ctx: Rc<RefCell<Self>>, name: &str) -> Self {
        let mut fork_imports = ctx.borrow().imports.clone();
        fork_imports.add_import(ModuleImport::all(ctx.borrow().identity.clone()));
        Self {
            imports: fork_imports,
            identity: ctx.borrow().identity.child(name),
            classes: HashMap::new(),
        }
    }

    pub fn add_import(&mut self, import: ModuleImport) {
        self.imports.add_import(import)
    }

    /// Creates and registers a new ClassType for given types, the given type must be subtype of given types
    pub fn define_class(
        ctx: Rc<RefCell<Self>>,
        def: ClassTypeDefinition,
    ) -> Result<Rc<TypeClass>, String> {

        let name = def.name.clone();
        let defined = def.build(ctx.clone())?;
        let defined = Rc::new(defined);

        let mut ctx = ctx.borrow_mut();
        match ctx.classes.entry(name.clone()) {
            Entry::Occupied(_) => Err(format!(
                "type {} already contained in context",
                name
            )),
            Entry::Vacant(vacant) => {
                vacant.insert(defined.clone());
                Ok(defined)
            }
        }
    }

    ///perform a class type lookup based on the defined types.
    /// If the type is not directly found in this context, then the context
    /// will lookup in parent's context.
    pub fn lookup_class(&self, name: &str) -> Result<Rc<TypeClass>, String> {
        let name = Name::new(name);
        match self.find(&name) {
            Some(v) => Ok(v.clone()),
            None => self.imports
                .lookup_element::<Rc<TypeClass>, Self>(&name)
                .ok_or(format!("Unknown type {}", &name))
        }
    }

    /// Find nearest type between two class types.
    pub fn unify(&self, t1: &Type, t2: &Type) -> Result<Type, String> {
        match (t1, t2) {
            (any, Type::Nothing) => Ok(any.clone()),
            (Type::Nothing, any) => Ok(any.clone()),

            (Type::Unknown, _) => Ok(Type::Unknown),
            (_, Type::Unknown) => Ok(Type::Unknown),

            (
                Type::Parametrized(p1),
                Type::Parametrized(p2),
            ) => {
                let cl1 = self.lookup_class(&p1.name)?;
                let cl2 = self.lookup_class(&p2.name)?;

                let common = cl1.get_common_parent(cl2);

                let vec: Vec<_> = common
                    .generic_parameters
                    .clone()
                    .into_iter()
                    .map(|_| Type::Unknown)
                    .collect();

                let name = common.fqcn.to_string();
                Ok(Type::parametrized(&name, vec.as_slice()))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lang_types::any;
    use crate::types::class::ClassTypeDefinition;
    use crate::types::context::TypeContext;
    use crate::types::types::Type;
    use pretty_assertions::assert_eq;
    use std::collections::{HashMap, HashSet};
    use crate::identity::Name;
    use crate::imports::ModuleImport;
    use crate::layers::ModuleLayers;

    #[test]
    fn specific_imports() -> Result<(), String> {
        let layers = ModuleLayers::new();

        let foo = ModuleLayers::declare_env(layers.clone(), Name::new("foo"))?
            .borrow()
            .type_context
            .clone();


        let bar = ModuleLayers::declare_env(layers.clone(), Name::new("bar"))?
            .borrow()
            .type_context
            .clone();

        let a = TypeContext::define_class(
            foo.clone(),
            ClassTypeDefinition::new(Name::new("A")),
        )?;

        let b = TypeContext::define_class(
            foo.clone(),
            ClassTypeDefinition::new(Name::new("B")),
        )?;

        bar.borrow_mut().add_import(ModuleImport::specifics(
            Name::new("foo"),
            HashSet::from(["A"]),
            HashMap::from([("AliasedB", "B")]), //Import B and alias it with AliasedB
        ));

        assert_eq!(bar.borrow().lookup_class("A")?, a);
        assert_eq!(bar.borrow().lookup_class("AliasedB")?, b);
        assert_eq!(
            bar.borrow().lookup_class("B"),
            Err("Unknown type B".to_string())
        );

        Ok(())
    }

    #[test]
    fn simple_union() -> Result<(), String> {
        let layers = ModuleLayers::new();

        let ctx = ModuleLayers::declare_env(layers.clone(), Name::new("std"))?
            .borrow()
            .type_context
            .clone();

        //Iterable[A]
        let iterable_cl = TypeContext::define_class(
            ctx.clone(),
            ClassTypeDefinition::new(Name::new("Iterable"))
                .with_generic("A", any()),
        )?;

        //Map[K, V]: Iterable[K]
        TypeContext::define_class(
            ctx.clone(),
            ClassTypeDefinition::new(Name::new("Map"))
                .with_super(iterable_cl.clone())
                .with_generic("K", any())
                .with_generic("V", any())
                .with_association(0, Type::cons("Map::K")),
        )?;

        //List[A]: Iterable[A]
        TypeContext::define_class(
            ctx.clone(),
            ClassTypeDefinition::new(Name::new("List"))
                .with_super(iterable_cl.clone())
                .with_generic("A", any())
                .with_association(0, Type::cons("List::A")),
        )?;

        let res1 = ctx.borrow().unify(
            &Type::parametrized("List", &[Type::cons("Str")]),
            &Type::parametrized("Map", &[Type::cons("Str"), Type::cons("Int")]),
        )?;

        let res2 = ctx.borrow().unify(
            &Type::parametrized("Map", &[Type::cons("Str"), Type::cons("Int")]),
            &Type::parametrized("List", &[Type::cons("Str")]),
        )?;

        assert_eq!(res1, Type::parametrized("std::Iterable", &[Type::Unknown]));
        assert_eq!(res2, Type::parametrized("std::Iterable", &[Type::Unknown]));

        Ok(())
    }
}