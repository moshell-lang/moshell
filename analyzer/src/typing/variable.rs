use crate::hir::NamedExports;
use crate::module::Export;
use crate::symbol::{Symbol, SymbolRegistry, SymbolTable, UndefinedSymbol};
use crate::typing::user::TypeId;
use context::source::Span;
use std::collections::HashMap;
use std::ops::Index;
use std::path::Path;
use std::rc::Rc;

/// A variable identifier.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Var {
    /// The variable is scoped to a [`LocalEnvironment`].
    Local(LocalId),

    /// The variable is a global variable and lives as long as the module.
    Global(Rc<String>),
}

/// A key for a [`LocalEnvironment`].
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct LocalId(pub usize);

/// A table that tracks the symbols and variables properties.
///
/// While the [`SymbolTable`] binds the symbols to their names, this table also tracks
/// variable-specific properties, i.e. if they are globals, locals or captures, and if they can be
/// reassigned.
pub(crate) struct VariableTable<'a> {
    /// The inner symbol table that this table is based on.
    inner: &'a mut SymbolTable,

    /// The environments that are currently in scope.
    environments: Vec<LocalEnvironment>,

    /// The global variables that this table created.
    globals: HashMap<String, GlobalInfo>,

    /// The mapping from symbol indices in the symbol table to local variables.
    ///
    /// If not present, the variable is assumed to be a global.
    symbols_to_locals: HashMap<usize, (usize, LocalId)>,
}

/// A reduced [`SymbolRegistry`] that identify symbols, but not variables.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum SymbolEntry {
    /// A named function.
    Function,

    /// A named type, either a struct or a module.
    Type,
}

impl From<SymbolEntry> for SymbolRegistry {
    fn from(entry: SymbolEntry) -> Self {
        match entry {
            SymbolEntry::Function => SymbolRegistry::Function,
            SymbolEntry::Type => SymbolRegistry::Type,
        }
    }
}

impl<'a> VariableTable<'a> {
    /// Create a new variable table for the given symbol table.
    pub(super) fn new(inner: &'a mut SymbolTable) -> Self {
        Self {
            inner,
            environments: Vec::new(),
            globals: HashMap::new(),
            symbols_to_locals: HashMap::new(),
        }
    }

    /// Insert a new local variable into the table.
    pub(super) fn insert_variable(
        &mut self,
        name: String,
        ty: TypeId,
        declared_at: Span,
        can_reassign: bool,
    ) -> Var {
        let id = if self.inner.current_depth == 0 {
            let name_id = Rc::new(name.clone());
            self.globals.insert(
                name.clone(),
                GlobalInfo {
                    id: name_id.clone(),
                    can_reassign,
                    ty,
                },
            );
            Var::Global(name_id)
        } else {
            let env_id = self.environments.len() - 1;
            let env = self
                .environments
                .last_mut()
                .expect("At least one environment should exist");
            let id = LocalId(env.locals.len());
            self.symbols_to_locals
                .insert(self.inner.len(), (env_id, id));
            env.locals.push(LocalInfo {
                id,
                can_reassign,
                ty,
            });
            Var::Local(id)
        };
        self.inner
            .insert_local(name, ty, declared_at, SymbolRegistry::Variable);
        id
    }

    pub(super) fn insert_local(
        &mut self,
        name: String,
        ty: TypeId,
        declared_at: Span,
        entry: SymbolEntry,
    ) {
        self.inner.insert_local(name, ty, declared_at, entry.into());
    }

    pub(super) fn insert_remote(&mut self, name: String, imported_at: Span, export: &Export) {
        self.inner.insert_remote(name, imported_at, export);
    }

    pub(super) fn get(&self, name: &str, registry: SymbolRegistry) -> Option<&Symbol> {
        self.inner.get(name, registry)
    }

    pub(crate) fn lookup(
        &self,
        name: &str,
        registry: SymbolRegistry,
    ) -> Result<&Symbol, UndefinedSymbol> {
        self.inner.lookup(name, registry)
    }

    pub(super) fn lookup_variable(&self, name: &str) -> Result<VariableInfo, UndefinedSymbol> {
        match self.inner.lookup_position(name, SymbolRegistry::Variable) {
            Ok((idx, symbol)) => Ok(match self.symbols_to_locals.get(&idx) {
                Some((env, local)) => VariableInfo::from(self.environments[*env][*local].clone()),
                None => {
                    if let Some(info) = self.globals.get(name) {
                        // Locally declared global variable
                        VariableInfo::from(info.clone()) // TODO optimize
                    } else {
                        // External global variable
                        VariableInfo {
                            id: Var::Global(Rc::new(name.to_owned())),
                            can_reassign: false,
                            ty: symbol.ty,
                        }
                    }
                }
            }),
            Err(err) => Err(err),
        }
    }

    pub(super) fn enter_scope(&mut self) {
        self.inner.enter_scope();
    }

    pub(super) fn exit_scope(&mut self) {
        self.inner.exit_scope();
    }

    pub(super) fn push_environment(&mut self) {
        self.environments.push(LocalEnvironment::default());
    }

    pub(super) fn pop_environment(&mut self) -> LocalEnvironment {
        let current_env_id = self.current_env_id();
        self.symbols_to_locals
            .retain(|_, (env_id, _)| *env_id != current_env_id);
        self.environments
            .pop()
            .expect("At least one environment should exist")
    }

    pub(super) fn current_env_id(&self) -> usize {
        self.environments.len() - 1
    }

    pub(super) fn path(&self) -> &Path {
        &self.inner.path
    }

    pub(super) fn take_exports(&mut self) -> NamedExports {
        self.globals
            .drain()
            .map(|(_, info)| (info.id, info.ty))
            .collect()
    }
}

#[derive(Default)]
pub struct LocalEnvironment {
    /// All variables that are owned by this environment, independently of their scope.
    pub locals: Vec<LocalInfo>,

    /// Tells which locals in this environment are captures.
    ///
    /// Upvalues mark variables that belong to a parent environment.
    pub upvalues: Vec<LocalId>,
}

#[derive(Clone)]
pub struct AssignableInfo<I> {
    pub id: I,
    pub can_reassign: bool,
    pub ty: TypeId,
}

pub type LocalInfo = AssignableInfo<LocalId>;
pub type GlobalInfo = AssignableInfo<Rc<String>>;
pub type VariableInfo = AssignableInfo<Var>;

impl Index<LocalId> for LocalEnvironment {
    type Output = LocalInfo;

    fn index(&self, index: LocalId) -> &Self::Output {
        &self.locals[index.0]
    }
}

impl From<LocalInfo> for VariableInfo {
    fn from(local: LocalInfo) -> Self {
        Self {
            id: Var::Local(local.id),
            can_reassign: local.can_reassign,
            ty: local.ty,
        }
    }
}

impl From<GlobalInfo> for VariableInfo {
    fn from(global: GlobalInfo) -> Self {
        Self {
            id: Var::Global(global.id),
            can_reassign: global.can_reassign,
            ty: global.ty,
        }
    }
}
