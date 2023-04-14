use crate::types::class::{ClassTypeDefinition, TypeClass};
use crate::types::types::{DefinedType, Type};
use std::cell::RefCell;
use std::collections::hash_map::{Entry};
use std::collections::HashMap;
use std::fmt::Debug;
use std::rc::Rc;
use crate::environment::{Environment, EnvironmentContext};

use crate::identity::Identity;
use crate::import_engine::ImportEngine;
use crate::imports::ModuleImport;

/// A type environment.
///
/// Contexts track substitutions and generate fresh type variables.
#[derive(Debug, Clone, Default)]
pub struct TypeContext {
    /// Records the type of each class by their basename.
    classes: HashMap<String, Rc<TypeClass>>,

    imports: ImportEngine,

    pub identity: Identity,
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
    fn find(&self, name: &str) -> Option<Rc<TypeClass>> {
        self.classes.get(name).cloned()
    }
}

impl TypeContext {
    pub(crate) fn with_classes<const T: usize>(classes: [(String, TypeClass); T],
                                               identity: Identity,
                                               imports: ImportEngine) -> Self {
        let classes = classes.into_iter().map(|(k, v)| (k, Rc::new(v))).collect();
        Self {
            classes,
            imports,
            identity,
        }
    }

    pub(crate) fn new(identity: Identity) -> Self {
        Self {
            identity,
            ..Default::default()
        }
    }

    ///Definitions of the lang type context.
    pub fn lang() -> Rc<RefCell<Self>> {
        let ctx = TypeContext::new(Identity::new("lang").expect(""));
        let ctx_rc = Rc::new(RefCell::new(ctx));
        let mut ctx = ctx_rc.borrow_mut();

        const MSG: &str = "lang type registration";

        let any_cl = &Rc::new(TypeClass {
            super_type: None,
            generic_parameters: vec![],
            super_params_associations: vec![],
            fqcn: ctx.identity.child("Any"),
            context: Rc::new(RefCell::default()),
        });
        ctx.classes.insert(any_cl.fqcn.name.clone(), any_cl.clone());
        drop(ctx);

        let float =
            Self::define_class(ctx_rc.clone(), ClassTypeDefinition::new("Float")).expect(MSG);

        Self::define_class(ctx_rc.clone(), ClassTypeDefinition::new("Bool")).expect(MSG);
        Self::define_class(ctx_rc.clone(), ClassTypeDefinition::new("Str")).expect(MSG);
        Self::define_class(ctx_rc.clone(), ClassTypeDefinition::new("Unit")).expect(MSG);

        let int = Self::define_class(
            ctx_rc.clone(),
            ClassTypeDefinition::new("Int").with_super(float),
        )
            .expect(MSG);

        Self::define_class(
            ctx_rc.clone(),
            ClassTypeDefinition::new("Exitcode").with_super(int),
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
        let defined = def.build(ctx.clone())?;
        let defined = Rc::new(defined);

        let mut ctx = ctx.borrow_mut();
        let name = defined.fqcn.name.clone();
        match ctx.classes.entry(name.clone()) {
            Entry::Occupied(_) => Err(format!(
                "type already contained in context {}",
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
    pub fn lookup_name(&self, name: &str) -> Result<Rc<TypeClass>, String> {
        match self.find(name) {
            Some(v) => Ok(v.clone()),
            None => self.imports
                .lookup_element(self, name)
                .ok_or(format!("Unknown type {}", name))
        }
    }

    pub fn lookup_defined(&self, def: &DefinedType) -> Result<Rc<TypeClass>, String> {
        match def {
            DefinedType::Parameterized(p) => self.lookup_name(&p.name),
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
                Type::Defined(DefinedType::Parameterized(p1)),
                Type::Defined(DefinedType::Parameterized(p2)),
            ) => {
                let cl1 = self.lookup_defined(&DefinedType::Parameterized(p1.clone()))?;
                let cl2 = self.lookup_defined(&DefinedType::Parameterized(p2.clone()))?;

                let common = cl1.get_common_parent(cl2);

                let vec: Vec<_> = common
                    .generic_parameters
                    .clone()
                    .into_iter()
                    .map(|_| Type::Unknown)
                    .collect();

                let name = &common.fqcn.name;
                Ok(Type::parametrized(name, vec.as_slice()))
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
    use std::cell::RefCell;
    use std::collections::{HashMap, HashSet};
    use std::rc::Rc;
    use crate::identity::Identity;
    use crate::imports::ModuleImport;

    #[test]
    fn specific_imports() -> Result<(), String> {
        let lang = TypeContext::lang();

        let mut foo = TypeContext::new(Identity::new("foo")?);
        foo.add_import(ModuleImport::all(Identity::new("lang")?));
        let foo = Rc::new(RefCell::new(foo));

        let mut bar = TypeContext::new(Identity::new("bar")?);
        bar.add_import(ModuleImport::all(Identity::new("lang")?));
        let bar = Rc::new(RefCell::new(bar));

        let a = TypeContext::define_class(
            foo.clone(),
            ClassTypeDefinition::new("A"),
        )?;

        let b = TypeContext::define_class(
            foo.clone(),
            ClassTypeDefinition::new("B"),
        )?;

        bar.borrow_mut().add_import(ModuleImport::specifics(
            Identity::new("foo")?,
            HashSet::from(["A"]),
            HashMap::from([("AliasedB", "B")]), //Import B and alias it with AliasedB
        ));

        assert_eq!(bar.borrow().lookup_name("A")?, a);
        assert_eq!(bar.borrow().lookup_name("AliasedB")?, b);
        assert_eq!(
            bar.borrow().lookup_name("B"),
            Err("Unknown type B".to_string())
        );

        Ok(())
    }

    #[test]
    fn simple_union() -> Result<(), String> {
        let lang = TypeContext::lang();
        let ctx = Rc::new(RefCell::new(TypeContext::fork(lang.clone(), "std")));

        //Iterable[A]
        let iterable_cl = TypeContext::define_class(
            ctx.clone(),
            ClassTypeDefinition::new("Iterable").with_generic("A", any()),
        )?;

        //Map[K, V]: Iterable[K]
        TypeContext::define_class(
            ctx.clone(),
            ClassTypeDefinition::new("Map")
                .with_super(iterable_cl.clone())
                .with_generic("K", any())
                .with_generic("V", any())
                .with_association(0, Type::cons("K")),
        )?;

        //List[A]: Iterable[A]
        TypeContext::define_class(
            ctx.clone(),
            ClassTypeDefinition::new("List")
                .with_super(iterable_cl.clone())
                .with_generic("A", any())
                .with_association(0, Type::cons("A")),
        )?;

        let res1 = ctx.borrow().unify(
            &Type::parametrized("List", &[Type::cons("Str")]),
            &Type::parametrized("Map", &[Type::cons("Str"), Type::cons("Int")]),
        )?;

        let res2 = ctx.borrow().unify(
            &Type::parametrized("Map", &[Type::cons("Str"), Type::cons("Int")]),
            &Type::parametrized("List", &[Type::cons("Str")]),
        )?;

        assert_eq!(res1, Type::parametrized("Iterable", &[Type::Unknown]));
        assert_eq!(res2, Type::parametrized("Iterable", &[Type::Unknown]));

        Ok(())
    }
}
