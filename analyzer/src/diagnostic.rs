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

    #[assoc(code = 2)]
    #[assoc(critical = true)]
    CannotImport,

    #[assoc(code = 3)]
    #[assoc(critical = true)]
    ImportResolution,

    #[assoc(code = 4)]
    #[assoc(critical = true)]
    UnknownSymbol,

    #[assoc(code = 5)]
    #[assoc(critical = true)]
    UseBetweenExprs,

    #[assoc(code = 6)]
    ShadowedImport,

    #[assoc(code = 7)]
    #[assoc(critical = true)]
    UnknownType,

    #[assoc(code = 8)]
    #[assoc(critical = true)]
    TypeMismatch,
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
    pub tips: Vec<String>,
}

impl Diagnostic {
    pub fn new(id: DiagnosticID, module: SourceObjectId, msg: impl Into<String>) -> Self {
        Self {
            source: module,
            identifier: id,
            global_message: msg.into(),
            observations: Vec::new(),
            tips: Vec::new(),
        }
    }

    pub fn with_observation(mut self, o: Observation) -> Self {
        self.observations.push(o);
        self
    }

    pub fn with_tip(mut self, tip: &str) -> Self {
        self.tips.push(tip.to_string());
        self
    }
}
