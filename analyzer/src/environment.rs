use crate::name::Name;
use crate::relations::{SourceObjectId, Symbol};
use context::source::{SourceSegment, SourceSegmentHolder};
use indexmap::IndexMap;
use variables::Variables;
use crate::environment::variables::TypeUsage;

pub mod variables;

///! The type environment of the analyzer.
///!
///! An environment maps local variable names to their type and keep tracks of scopes.
///! The same variable name can be accessed in different scopes, and can have different type in
///! different stack frames. For example:
///! ```text
///! {
///!     // The variable `n` doesn't exist yet.
///!     val n = 9; // Create a new variable `n` with type `int`.
///!     // In this frame, the variable `n` of type `int` is in scope.
///!     {
///!         // The variable `n` exists, and refers to the variable in the outer scope.
///!         val n = "9"; // Create a new variable `n` with type `any` that shadows the outer `n`.
///!         echo $n;
///!         // In this frame, the variable `n` of type `any` is in scope.
///!     }
///!     // In this frame, the variable `n` of type `int` is in scope.
///!     echo $n;
///! }
///! ```

/// An environment.
/// The Environment contains the defined types, variables, structure and function definitions of a certain scope.
/// It can have dependencies over other environments.
#[derive(Debug, Clone)]
pub struct Environment {
    /// The source object id of the parent environment, if the environment is nested.
    pub parent: Option<SourceObjectId>,

    ///Fully qualified name of the environment
    pub fqn: Name,

    /// The variables that are declared in the environment.
    pub variables: Variables,

    /// A mapping of expression segments to definitions.
    pub definitions: IndexMap<SourceSegment, Definition>,
}

impl Environment {
    pub fn named(name: Name) -> Self {
        Self {
            parent: None,
            fqn: name,
            variables: Variables::default(),
            definitions: IndexMap::new(),
        }
    }

    pub fn fork(&self, source_id: SourceObjectId, name: &str) -> Environment {
        let env_fqn = self.fqn.child(name);

        Self {
            parent: Some(source_id),
            fqn: env_fqn,
            variables: Variables::default(),
            definitions: IndexMap::new(),
        }
    }

    pub fn begin_scope(&mut self) {
        self.variables.begin_scope();
    }

    pub fn end_scope(&mut self) {
        self.variables.end_scope();
    }

    /// Adds an annotation to any segment.
    ///
    /// This method exposes a low level API to add annotations to segments, preferably use the
    /// wrapper methods defined in traits in the `checker` crate.
    pub fn annotate(&mut self, segment: &impl SourceSegmentHolder, def: Definition) {
        self.definitions.insert(segment.segment(), def);
    }

    pub fn list_definitions(&self) -> impl Iterator<Item = (&SourceSegment, &Definition)> {
        self.definitions.iter()
    }

    /// Gets a symbol from the environment.
    pub fn get_raw_symbol(&self, segment: SourceSegment) -> Option<Symbol> {
        self.definitions.get(&segment).map(|d| d.symbol)
    }

    /// Finds the local segments that references a symbol.
    pub fn find_references(&self, symbol_declaration: Symbol) -> Vec<SourceSegment> {
        let mut references = Vec::new();
        for (segment, def) in &self.definitions {
            if def.symbol == symbol_declaration {
                references.push(segment.clone());
            }
        }
        references
    }
}


#[derive(Debug, Clone)]
pub struct Definition {
    pub symbol: Symbol,
    pub usage: Option<TypeUsage>
}

impl Definition {
    pub fn reference(symbol: Symbol, usage: TypeUsage) -> Self {
        Self {
            symbol,
            usage: Some(usage)
        }
    }

    pub fn declaration(symbol: Symbol) -> Self {
        Self {
            symbol,
            usage: None
        }
    }
}
