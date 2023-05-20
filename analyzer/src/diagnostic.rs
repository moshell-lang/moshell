use enum_assoc::Assoc;
use crate::relations::{SourceObjectId};
use context::source::{SourceSegment, SourceSegmentHolder};
use crate::diagnostic::DiagnosticType::{Error, Warn};

#[derive(PartialEq, Debug, Assoc)]
#[func(pub fn code(&self) -> &'static str)]
pub enum ErrorID {
    #[assoc(code = "E001")]
    UnsupportedFeature,

    //No message is intentional as
    #[assoc(code = "E002")]
    CannotImport,

    #[assoc(code = "E003")]
    ImportResolution,

    #[assoc(code = "E004")]
    UnknownSymbol,

    #[assoc(code = "E005")]
    UseBetweenExprs,
}

#[derive(PartialEq, Debug, Assoc)]
#[func(pub fn code(&self) -> &'static str)]
pub enum WarnID {
    #[assoc(code = "W001")]
    ShadowedImport
}

#[derive(PartialEq, Debug)]
pub enum DiagnosticType {
    Error(ErrorID),
    Warn(WarnID),
}

impl DiagnosticType {
    pub fn code(&self) -> &'static str {
        match self {
            Error(e) => e.code(),
            Warn(w) => w.code(),
        }
    }
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
    pub fn new(segment_holder: &impl SourceSegmentHolder) -> Self {
        Self {
            segment: segment_holder.segment(),
            help: None,
        }
    }

    pub fn with_help(segment_holder: &impl SourceSegmentHolder, help: &str) -> Self {
        Self {
            segment: segment_holder.segment(),
            help: Some(help.to_string()),
        }
    }
}

/// The structure of a diagnostic.
#[derive(PartialEq, Debug)]
pub struct Diagnostic {
    /// The source where this diagnostic applies
    pub source: SourceObjectId,
    /// The type of diagnostic, see [DiagnosticType] for further details
    pub ty: DiagnosticType,
    /// The overall message of this diagnostic
    pub global_message: String,
    /// Some observations to explain the diagnostic
    pub observations: Vec<Observation>,
    /// Any tips to help the user understand and eventually fix the raised issue.
    pub tips: Vec<String>,
}

impl Diagnostic {
    pub fn warn(id: WarnID, module: SourceObjectId, msg: &str) -> Self {
        Self {
            source: module,
            ty: Warn(id),
            global_message: msg.to_string(),
            observations: Vec::new(),
            tips: Vec::new(),
        }
    }

    pub fn error(id: ErrorID, module: SourceObjectId, msg: &str) -> Self {
        Self {
            source: module,
            ty: Error(id),
            global_message: msg.to_string(),
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
