use std::cell::RefCell;
use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::hash::{Hash, Hasher};
use std::rc::Rc;
use crate::types::class::{ClassTypeDefinition, TypeClass};
use crate::types::types::{DefinedType, ParameterizedType, Type};

/// A type environment.
///
/// Contexts track substitutions and generate fresh types variables.
#[derive(Debug, Clone, Eq, Default)]
pub struct TypeContext {
    /// Records the types of each class by their identity.
    pub(crate) classes: HashMap<u64, Rc<TypeClass>>,

    pub(crate) dependencies: Vec<Rc<RefCell<TypeContext>>>,
}

impl PartialEq for TypeContext {
    fn eq(&self, other: &Self) -> bool {
        self.classes == other.classes
    }
}

//temporary function until an identification system is implemented
pub(crate) fn hash_of<H: Hash>(hashable: &H) -> u64 {
    let mut hasher = DefaultHasher::new();
    hashable.hash(&mut hasher);
    hasher.finish()
}

impl TypeContext {
    ///Definitions of the lang types context.
    pub fn lang() -> Rc<RefCell<Self>> {
        let mut ctx_rc = Rc::new(RefCell::new(TypeContext::default()));
        let mut ctx = ctx_rc.borrow_mut();

        const MSG: &str = "lang types registration";

        let any_cl = &Rc::new(TypeClass {
            super_type: None,
            name: "Any".to_owned(),
            generic_parameters: vec![],
            super_params_associations: vec![],
            identity: hash_of(&"Any"),
            context: Rc::new(RefCell::new(Self::fork(ctx_rc.clone()))),
        });
        ctx.classes.insert(any_cl.identity, any_cl.clone());
        drop(ctx);

        let float = Self::define_class(&ctx_rc, ClassTypeDefinition::new("Float")).expect(MSG);

        Self::define_class(&ctx_rc, ClassTypeDefinition::new("Bool")).expect(MSG);
        Self::define_class(&ctx_rc, ClassTypeDefinition::new("Str")).expect(MSG);
        Self::define_class(&ctx_rc, ClassTypeDefinition::new("Unit")).expect(MSG);

        let int = Self::define_class(&ctx_rc,
                                     ClassTypeDefinition::new("Int")
                                         .with_super(float),
        ).expect(MSG);

        Self::define_class(&ctx_rc,
                           ClassTypeDefinition::new("Exitcode")
                               .with_super(int),
        ).expect(MSG);

        ctx_rc.clone()
    }

    /// Creates and registers a new ClassType for given types, the given types must be subtype of given types
    pub fn define_class(mut ctx: &Rc<RefCell<Self>>, def: ClassTypeDefinition) -> Result<Rc<TypeClass>, String> {
        let id = hash_of(&def.name);

        let defined = def.with_identity(id).build(ctx.clone())?;
        let defined = Rc::new(defined);

        if ctx.borrow().classes.contains_key(&defined.identity) {
            return Err(format!("types already contained in context {}", defined.name).to_owned())
        }

        ctx.borrow_mut().classes.insert(
            defined.identity,
            defined.clone(),
        );
        Ok(defined)
    }


    ///perform a class types lookup based on the defined types.
    /// If the types is not directly found in this context, then the context
    /// will lookup in parent's context.
    pub fn lookup_id(&self, id: u64) -> Result<Rc<TypeClass>, String> {
        match self.classes.get(&id) {
            Some(v) => Ok(v.clone()),
            None => {
                let iter = self.dependencies.iter();
                for dep in iter {
                    if let Some(found) = dep.borrow().lookup_id(id).ok() {
                        return Ok(found)
                    }
                }
                Err("Unknown types".to_owned())
            }
        }
    }

    pub fn lookup_defined(&self, def: DefinedType) -> Result<Rc<TypeClass>, String> {
        match def {
            DefinedType::Parameterized(p) => self.lookup_id(hash_of(&p.name)),
        }
    }

    pub fn unify(&self, t1: &Type, t2: &Type) -> Result<Type, String> {
        self.unify_internal(t1, t2)
    }

    pub(crate) fn fork(ctx: Rc<RefCell<Self>>) -> TypeContext {
        TypeContext {
            dependencies: vec!(ctx),
            ..Default::default()
        }
    }

    ///Find largest possible types between two class types
    fn unify_internal(&self, t1: &Type, t2: &Type) -> Result<Type, String> {
        match (t1, t2) {
            (any, Type::Nothing) => Ok(any.clone()),
            (Type::Nothing, any) => Ok(any.clone()),

            (Type::Unknown, _) => Ok(Type::Unknown),
            (_, Type::Unknown) => Ok(Type::Unknown),

            (Type::Defined(DefinedType::Parameterized(p1)),
                Type::Defined(DefinedType::Parameterized(p2))) => {
                self.unify_parameterized(p1, p2)
                    .map(DefinedType::Parameterized)
                    .map(Type::Defined)
            }

        }
    }

    fn unify_parameterized(&self, p1: &ParameterizedType, p2: &ParameterizedType) -> Result<ParameterizedType, String> {
        let cl1 = self.lookup_defined(DefinedType::Parameterized(p1.clone()))?;
        let cl2 = self.lookup_defined(DefinedType::Parameterized(p2.clone()))?;

        let common = cl1.get_common_parent(cl2);

        todo!()
    }

}
