use crate::builtin_types::{bool_type, float_type, int_type, nil_type, nothing_type, string_type};
use crate::types::{Type, Variable};
use indexmap::IndexMap;

/// A type environment.
///
/// Contexts track substitutions and generate fresh type variables.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Context {
    /// Records the type of each class.
    classes: IndexMap<String, Type>,

    /// A set of constraints mapping from variables to types.
    substitution: IndexMap<Variable, Type>,

    /// A counter used to generate fresh variables.
    next: usize,
}

impl Context {
    pub fn fill_with_builtins(&mut self) {
        self.classes.insert("Bool".to_owned(), bool_type());
        self.classes.insert("Int".to_owned(), int_type());
        self.classes.insert("Float".to_owned(), float_type());
        self.classes.insert("Str".to_owned(), string_type());
        self.classes.insert("Nil".to_owned(), nil_type());
        self.classes.insert("Nothing".to_owned(), nothing_type());
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
        self.substitution.insert(v, t);
    }

    pub fn apply(&self, t: &Type) -> Type {
        match t {
            Type::Variable(v) => self
                .substitution
                .get(v)
                .map(|t| self.apply(t))
                .unwrap_or_else(|| t.clone()),
            Type::Constructed(name, args) => {
                let args = args.iter().map(|t| self.apply(t)).collect();
                Type::Constructed(name.clone(), args)
            }
        }
    }

    pub fn lookup(&self, v: Variable) -> Option<Type> {
        self.substitution.get(&v).cloned()
    }

    pub fn lookup_class_name(&self, name: &str) -> Option<&Type> {
        self.classes.get(name)
    }

    pub fn unify(&self, t1: &Type, t2: &Type) -> Result<Type, String> {
        let t1 = self.apply(t1);
        let t2 = self.apply(t2);
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
                        ts.push(self.unify(t1, t2)?);
                    }
                    Ok(Type::Constructed(c1.clone(), ts))
                } else {
                    Err(format!("Cannot unify {} and {}", t1, t2))
                }
            }
        }
    }
}
