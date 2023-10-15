use enum_assoc::Assoc;

use crate::reef::ReefId;
use context::source::SourceSegment;

use crate::relations::SourceId;

#[non_exhaustive]
#[derive(PartialEq, Debug, Assoc, Clone, Copy)]
#[func(pub fn code(&self) -> u16)]
#[func(pub fn critical(&self) -> bool { true })]
pub enum DiagnosticID {
    #[assoc(code = 1)]
    UnsupportedFeature,

    /// An import could not be resolved
    #[assoc(code = 2)]
    ImportResolution,

    /// A symbol is unknown as it could not be resolved
    #[assoc(code = 3)]
    UnknownSymbol,

    /// A symbol is invalid as it cannot be accessed in any way
    /// (for example, a symbol into a function, or in a variable)
    #[assoc(code = 4)]
    InvalidSymbol,

    /// A symbol path is invalid by its structure
    /// (for example, the path `reef::foo::reef` is invalid because the last `reef` would targets the current reef)
    #[assoc(code = 5)]
    InvalidSymbolPath,

    /// There is a `use` statement between two expressions,
    /// `use` needs to be declared before any expressions in an environment.
    #[assoc(code = 6)]
    UseBetweenExprs,

    /// A `use` statement is shadowed as the symbol it imports has been imported again below
    #[assoc(code = 7)]
    #[assoc(critical = false)]
    ShadowedImport,

    /// A symbol have the same fully qualified name (its name with its module's name prepended)
    /// as another module
    #[assoc(code = 8)]
    SymbolConflictsWithModule,

    /// A type annotation refers to an unknown type.
    #[assoc(code = 9)]
    UnknownType,

    /// A type annotation is not matching the expected type.
    #[assoc(code = 10)]
    TypeMismatch,

    /// A type annotation is missing, and cannot be inferred.
    #[assoc(code = 11)]
    CannotInfer,

    /// Occurs when a `continue` or `break` directive is declared outside of a loop.
    #[assoc(code = 12)]
    InvalidBreakOrContinue,

    /// A type cannot be casted to another type.
    #[assoc(code = 13)]
    IncompatibleCast,

    /// A named method is unknown or does not match the expected signature.
    #[assoc(code = 14)]
    UnknownMethod,

    /// A variable is being reassigned, but it is not mutable.
    #[assoc(code = 15)]
    CannotReassign,

    /// A function does not have a definition.
    ///
    /// Only internals reefs that are defined natively can omit an implementation.
    #[assoc(code = 16)]
    NoFunctionDefinition,

    /// An incorrect number of type arguments was provided.
    #[assoc(code = 17)]
    InvalidTypeArguments,

    /// An assignment operator was used on a non-place expression.
    #[assoc(code = 18)]
    InvalidAssignment,

    /// A field access was done on a value that is not a structure
    #[assoc(code = 19)]
    InvalidFieldAccess,
}

/// Observations are labels in a code snippet that are used to explain a [`Diagnostic`].
///
/// They can contain a message to explain the role of this specific snippet.
#[derive(Clone, PartialEq, Debug)]
pub struct Observation {
    /// The location where this observation applies
    pub location: SourceLocation,
    /// An optional help string to complete the observation
    pub message: Option<String>,
}

impl Observation {
    /// Creates an observation that underlines an erroneous location.
    ///
    /// Prefer adding a label to explain the observation.
    pub fn new(location: SourceLocation) -> Self {
        Self {
            location,
            message: None,
        }
    }

    /// Creates an observation on an erroneous location.
    pub fn here(
        source: SourceId,
        reef: ReefId,
        segment: SourceSegment,
        message: impl Into<String>,
    ) -> Self {
        Self {
            location: SourceLocation::new(source, reef, segment),
            message: Some(message.into()),
        }
    }

    /// Creates a contextual observation.
    pub fn context(
        source: SourceId,
        reef: ReefId,
        segment: SourceSegment,
        message: impl Into<String>,
    ) -> Self {
        Self {
            location: SourceLocation::new(source, reef, segment),
            message: Some(message.into()),
        }
    }
}

/// A location in a source code.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SourceLocation {
    pub source: SourceId,
    pub reef: ReefId,
    pub segment: SourceSegment,
}

impl SourceLocation {
    /// Creates a new source location.
    pub fn new(source: SourceId, reef: ReefId, segment: SourceSegment) -> Self {
        Self {
            source,
            reef,
            segment,
        }
    }
}

/// The structure of a diagnostic.
#[derive(PartialEq, Debug)]
pub struct Diagnostic {
    /// The diagnostic identifier
    pub identifier: DiagnosticID,
    /// The overall message of this diagnostic
    pub global_message: String,
    /// Some observations to explain the diagnostic
    pub observations: Vec<Observation>,
    /// Any tips to help the user understand and eventually fix the raised issue.
    pub helps: Vec<String>,
}

impl Diagnostic {
    pub fn new(id: DiagnosticID, msg: impl Into<String>) -> Self {
        Self {
            identifier: id,
            global_message: msg.into(),
            observations: Vec::new(),
            helps: Vec::new(),
        }
    }

    pub fn with_observation(mut self, o: Observation) -> Self {
        self.observations.push(o);
        self
    }

    pub fn with_observations<I: IntoIterator<Item = Observation>>(
        mut self,
        observations: I,
    ) -> Self {
        self.observations.extend(observations);
        self
    }

    pub fn with_help(mut self, help: impl Into<String>) -> Self {
        self.helps.push(help.into());
        self
    }
}

impl From<(SourceId, ReefId, SourceSegment)> for Observation {
    fn from((source, reef, segment): (SourceId, ReefId, SourceSegment)) -> Self {
        Self::new(SourceLocation::new(source, reef, segment))
    }
}
