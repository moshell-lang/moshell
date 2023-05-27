use crate::relations::SourceObjectId;
use context::source::SourceSegment;
use enum_assoc::Assoc;

#[derive(PartialEq, Debug, Assoc)]
#[func(pub fn code(&self) -> u16)]
#[func(pub fn critical(&self) -> bool { false })]
pub enum DiagnosticID {
    #[assoc(code = 1)]
    #[assoc(critical = true)]
    UnsupportedFeature,

    /// An import could not be resolved
    #[assoc(code = 2)]
    #[assoc(critical = true)]
    ImportResolution,

    /// A symbol is unknown as it could not be resolved
    #[assoc(code = 3)]
    #[assoc(critical = true)]
    UnknownSymbol,

    /// There is a `use` statement between two expressions,
    /// `use` needs to be declared before any expressions in an environment.
    #[assoc(code = 4)]
    #[assoc(critical = true)]
    UseBetweenExprs,

    /// A `use` statement is shadowed as the symbol it imports has been imported again below
    #[assoc(code = 5)]
    ShadowedImport,

    /// A symbol have the same fully qualified name (its name with its module's name prepended)
    /// as another module
    #[assoc(code = 6)]
    SymbolConflictsWithModule,
}

/// Observations are an area in the source code with an (optional) help message
/// that are contained in a [Diagnostic] to emphasis/further explain the causes of the diagnostic.
#[derive(PartialEq, Debug)]
pub struct Observation {
    /// Observed segment
    pub segment: SourceSegment,
    /// An optional help string to complete the observation
    pub help: Option<String>,
}

impl Observation {
    pub fn new(segment: SourceSegment) -> Self {
        Self {
            segment,
            help: None,
        }
    }

    pub fn with_help(segment: SourceSegment, help: impl Into<String>) -> Self {
        Self {
            segment,
            help: Some(help.into()),
        }
    }
}

/// The structure of a diagnostic.
#[derive(PartialEq, Debug)]
pub struct Diagnostic {
    /// The source where this diagnostic applies
    pub source: SourceObjectId,
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
    pub fn new(id: DiagnosticID, module: SourceObjectId, msg: impl Into<String>) -> Self {
        Self {
            source: module,
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

    pub fn with_help(mut self, help: impl Into<String>) -> Self {
        self.helps.push(help.into());
        self
    }
}
