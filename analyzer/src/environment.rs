//! The type environment of the analyzer.
//!
//! An environment maps local variable names to their type and keep tracks of scopes.
//! The same variable name can be accessed in different scopes, and can have different type in
//! different stack frames. For example:
//!
//! ```code
//! {
//!     // The variable `n` doesn't exist yet.
//!     val n = 9; // Create a new variable `n` with type `int`.
//!     // In this frame, the variable `n` of type `int` is in scope.
//!     {
//!         // The variable `n` exists, and refers to the variable in the outer scope.
//!         val n = "9"; // Create a new variable `n` with type `any` that shadows the outer `n`.
//!         echo $n;
//!         // In this frame, the variable `n` of type `any` is in scope.
//!     }
//!     // In this frame, the variable `n` of type `int` is in scope.
//!     echo $n;
//! }
//! ```

use std::collections::HashMap;

use context::source::{SourceSegment, SourceSegmentHolder};
use symbols::Symbols;

use crate::name::Name;
use crate::relations::{SourceId, SymbolRef};

pub mod symbols;

/// An environment.
/// The Environment contains the defined types, variables, structure and function definitions of a certain scope.
/// It can have dependencies over other environments.
#[derive(Debug, Clone)]
pub struct Environment {
    /// The source object id of the parent environment, if the environment is nested.
    pub parent: Option<SourceId>,

    /// Whether the environment is directly executable.
    pub is_script: bool,

    ///Fully qualified name of the environment
    pub fqn: Name,

    /// The variables that are declared in the environment.
    pub symbols: Symbols,

    /// A mapping of expression segments to symbols.
    pub definitions: HashMap<SourceSegment, SymbolRef>,

    /// A mapping of expression segments to their declaring environment.
    pub declarations: HashMap<SourceSegment, SourceId>,
}

impl Environment {
    pub fn script(name: Name) -> Self {
        Self {
            parent: None,
            is_script: true,
            fqn: name,
            symbols: Symbols::default(),
            definitions: HashMap::new(),
            declarations: HashMap::new(),
        }
    }

    pub fn fork(&self, source_id: SourceId, name: &str) -> Environment {
        let env_fqn = self.fqn.child(name);

        Self {
            parent: Some(source_id),
            is_script: false,
            fqn: env_fqn,
            symbols: Symbols::default(),
            definitions: HashMap::new(),
            declarations: HashMap::new(),
        }
    }

    pub fn begin_scope(&mut self) {
        self.symbols.begin_scope();
    }

    pub fn end_scope(&mut self) {
        self.symbols.end_scope();
    }

    /// Gets an iterator over the direct inner environment identifiers.
    pub fn iter_direct_inner_environments(&self) -> impl Iterator<Item = SourceId> + '_ {
        self.declarations.values().copied()
    }

    /// Tests if the position of the declaration of a symbol is important.
    ///
    /// If the declaration order is important in the host environment, this requires that symbol
    /// resolution must be done immediately after the child environment is collected. It does
    /// mean that all the symbols referenced in the declaration and in this environment must be
    /// declared before. If not, symbol resolution happens after the whole environment is collected,
    /// and the symbol can be resolved in any order.
    pub fn has_strict_declaration_order(&self) -> bool {
        !self.is_script
    }

    /// Adds an annotation to any segment.
    pub fn annotate(&mut self, segment: &impl SourceSegmentHolder, symbol: SymbolRef) {
        self.definitions.insert(segment.segment(), symbol);
    }

    /// Maps the declaring environment of a segment.
    pub fn bind_source(&mut self, segment: &impl SourceSegmentHolder, source: SourceId) {
        self.declarations.insert(segment.segment(), source);
    }

    /// Iterates over the segments that maps to a symbol.
    pub fn list_definitions(&self) -> impl Iterator<Item = (&SourceSegment, &SymbolRef)> {
        self.definitions.iter()
    }

    /// Gets a symbol from the environment.
    pub fn get_raw_symbol(&self, segment: SourceSegment) -> Option<SymbolRef> {
        self.definitions.get(&segment).copied()
    }

    /// Gets the declaring environment id of a segment.
    pub fn get_raw_env(&self, segment: SourceSegment) -> Option<SourceId> {
        self.declarations.get(&segment).copied()
    }

    /// Finds the local segments that references a symbol.
    pub fn find_references(&self, symbol_declaration: SymbolRef) -> Vec<SourceSegment> {
        let mut references = Vec::new();
        for (segment, symbol_reference) in &self.definitions {
            if symbol_reference == &symbol_declaration {
                references.push(segment.clone());
            }
        }
        references
    }
}
