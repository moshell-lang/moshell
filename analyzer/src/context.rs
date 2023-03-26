use crate::builtin_types::{bool_type, float_type, int_type, nothing_type, string_type};
use crate::classes::ClassType;
use crate::types::{Type, TypeScheme, Variable};
use indexmap::IndexMap;

/// A type environment.
///
/// Contexts track substitutions and generate fresh type variables.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Context {
    /// Records the type of each class by name.
    classes: IndexMap<String, Variable>,

    /// Definitions of each class by id.
    definitions: IndexMap<Variable, ClassType>,

    /// A set of constraints mapping from variables to types.
    substitution: IndexMap<Variable, TypeScheme>,

    /// A counter used to generate fresh variables.
    next: usize,
}

impl Context {
    pub fn fill_with_builtins(&mut self) {
        self.define("Bool".to_owned(), bool_type());
        self.define("Int".to_owned(), int_type());
        self.define("Float".to_owned(), float_type());
        self.define("Str".to_owned(), string_type());
        self.define("Nothing".to_owned(), nothing_type());
    }

    /// Creates a new variable from the next unused number.
    pub fn new_variable(&mut self) -> Variable {
        self.next += 1;
        Variable(self.next - 1)
    }

    /// Creates a new substitution for variable number `v` to the given type `t`.
    pub fn extend(&mut self, v: Variable, t: Type) {
        if v.0 >= self.next {
            self.next = v.0 + 1;
        }
        self.substitution.insert(v, TypeScheme::Monotype(t));
    }

    /// Creates a new substitution for variable number `v` to the given type `t`.
    pub fn define(&mut self, name: String, class: ClassType) -> Variable {
        let var = self.new_variable();
        self.definitions.insert(var, class.clone());
        self.substitution.insert(
            var,
            TypeScheme::Monotype(Type::Constructed(name.clone(), vec![])),
        );
        self.classes.insert(name, var);
        var
    }

    pub fn apply(&self, var: Variable) -> &Type {
        match self.substitution.get(&var) {
            Some(TypeScheme::Monotype(t)) => t,
            Some(TypeScheme::Polytype { .. }) => todo!("apply polytype"),
            None => panic!("Unknown variable {var}"),
        }
    }

    pub fn extract(&self, v: Variable) -> Option<TypeScheme> {
        self.substitution.get(&v).cloned()
    }

    pub fn lookup_class_name(&self, name: &str) -> Option<Variable> {
        self.classes.get(name).cloned()
    }

    pub fn lookup_class_name_type(&self, name: &str) -> Option<Type> {
        self.lookup_class_name(name).map(|v| self.apply(v).clone())
    }

    pub fn lookup_definition(&self, var: Variable) -> Option<&ClassType> {
        match self.definitions.get(&var) {
            Some(class) => Some(class),
            None => match self.substitution.get(&var) {
                Some(TypeScheme::Monotype(Type::Variable(var))) => self.lookup_definition(*var),
                _ => None,
            },
        }
    }

    pub fn resolve(&self, declared_type: &TypeScheme) -> Result<Variable, String> {
        match declared_type {
            TypeScheme::Monotype(t) => self.resolve_monotype(t),
            TypeScheme::Polytype { .. } => todo!("resolve polytype"),
        }
    }

    pub fn resolve_monotype(&self, declared_type: &Type) -> Result<Variable, String> {
        match declared_type {
            Type::Variable(v) => self
                .substitution
                .get(v)
                .map(|t| self.resolve(t))
                .unwrap_or(Ok(*v)),
            Type::Constructed(name, args) => {
                let var = self
                    .classes
                    .get(name)
                    .ok_or_else(|| format!("Unknown class {}", name))?;
                let class = self
                    .definitions
                    .get(var)
                    .ok_or_else(|| format!("Unknown class {}", name))?;
                assert_eq!(class.args.len(), args.len());
                assert_eq!(class.args.len(), 0);
                Ok(*var)
            }
        }
    }

    pub fn unify(&mut self, t1: Variable, t2: Variable) -> Result<Variable, String> {
        let var = self.new_variable();
        let t = self.unify_internal(self.apply(t1), self.apply(t2))?;
        self.extend(var, t);
        Ok(var)
    }

    fn unify_internal(&self, t1: &Type, t2: &Type) -> Result<Type, String> {
        let t1 = self.follow(t1);
        let t2 = self.follow(t2);
        match (&t1, &t2) {
            (Type::Variable(v1), Type::Variable(v2)) => {
                if v1 == v2 {
                    Ok(t1.clone())
                } else {
                    Ok(t2.clone())
                }
            }
            (Type::Variable(_), _) => Ok(t2.clone()),
            (_, Type::Variable(_)) => Ok(t1.clone()),
            (Type::Constructed(c1, ts1), Type::Constructed(c2, ts2)) => {
                if c1 == c2 && ts1.len() == ts2.len() {
                    let mut ts = vec![];
                    for (t1, t2) in ts1.iter().zip(ts2.iter()) {
                        ts.push(self.unify_internal(t1, t2)?);
                    }
                    Ok(Type::Constructed(c1.clone(), ts))
                } else {
                    Err(format!("Cannot unify {} and {}", t1, t2))
                }
            }
        }
    }

    fn follow(&self, t: &Type) -> Type {
        match t {
            Type::Variable(v) => self
                .substitution
                .get(v)
                .map(|t| match t {
                    TypeScheme::Monotype(t) => self.follow(t),
                    TypeScheme::Polytype { .. } => todo!("follow polytype"),
                })
                .unwrap_or_else(|| t.clone()),
            _ => t.clone(),
        }
    }
}
