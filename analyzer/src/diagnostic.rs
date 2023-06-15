use crate::relations::SourceId;
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

    /// A symbol is invalid as it cannot be accessed in any way
    /// (for example, a symbol into a function, or in a variable)
    #[assoc(code = 4)]
    #[assoc(critical = true)]
    InvalidSymbol,

    /// There is a `use` statement between two expressions,
    /// `use` needs to be declared before any expressions in an environment.
    #[assoc(code = 5)]
    #[assoc(critical = true)]
    UseBetweenExprs,

    /// A `use` statement is shadowed as the symbol it imports has been imported again below
    #[assoc(code = 6)]
    ShadowedImport,

    /// A symbol have the same fully qualified name (its name with its module's name prepended)
    /// as another module
    #[assoc(code = 7)]
    #[assoc(critical = true)]
    SymbolConflictsWithModule,

    #[assoc(code = 8)]
    #[assoc(critical = true)]
    UnknownType,

    #[assoc(code = 9)]
    #[assoc(critical = true)]
    TypeMismatch,

    #[assoc(code = 10)]
    #[assoc(critical = true)]
    CannotInfer,

    /// Occurs when a `continue` or `break` directive is declared outside of a loop
    #[assoc(code = 11)]
    #[assoc(critical = true)]
    InvalidBreakOrContinue,
}

/// Observations are an area in the source code with an (optional) help message
/// that are contained in a [Diagnostic] to emphasis/further explain the causes of the diagnostic.
#[derive(Clone, PartialEq, Debug)]
pub struct Observation {
    /// Optional environment if the observation occurs on a different environment than the diagnostic's source
    /// if none, this observation will target its diagnostic.
    pub foreign_env: Option<SourceId>,
    /// Observed segment
    pub segment: SourceSegment,
    /// An optional help string to complete the observation
    pub help: Option<String>,
    /// An optional tag to group observations
    pub tag: Option<ObservationTag>,
}

impl Observation {
    pub fn new(segment: SourceSegment) -> Self {
        Self {
            segment,
            foreign_env: None,
            help: None,
            tag: None,
        }
    }

    pub fn within(mut self, env: SourceId) -> Self {
        self.foreign_env = Some(env);
        self
    }

    pub fn with_help(mut self, help: impl Into<String>) -> Self {
        self.help = Some(help.into());
        self
    }

    pub fn with_tag(mut self, tag: ObservationTag) -> Self {
        self.tag = Some(tag);
        self
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum ObservationTag {
    InFault,
    Declaration,
    Other(u8),
}

/// The structure of a diagnostic.
#[derive(PartialEq, Debug)]
pub struct Diagnostic {
    /// The source where this diagnostic applies
    pub source: SourceId,
    /// The diagnostic identifier
    pub identifier: DiagnosticID,
    /// The overall message of this diagnostic
    pub global_message: String,
    /// Some observations to explain the diagnostic
    pub observations: Vec<Observation>,
    /// Any tips to help the user understand and eventually fix the raised issue.
    pub helps: Vec<String>,
    /// Any tips to help the user understand and eventually fix the raised issue.
    pub notes: Vec<String>,
}

impl Diagnostic {
    pub fn new(id: DiagnosticID, source: SourceId, msg: impl Into<String>) -> Self {
        Self {
            source,
            identifier: id,
            global_message: msg.into(),
            observations: Vec::new(),
            helps: Vec::new(),
            notes: Vec::new(),
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

    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.notes.push(note.into());
        self
    }
}
