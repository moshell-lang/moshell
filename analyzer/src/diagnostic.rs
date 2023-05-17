use enum_assoc::Assoc;
use crate::relations::{SourceObjectId};
use context::source::SourceSegment;
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

#[derive(PartialEq, Debug)]
pub enum WarnID {

}

#[derive(PartialEq, Debug)]
pub enum DiagnosticType {
    Error(ErrorID),
    Warn(WarnID),
}

#[derive(PartialEq, Debug)]
pub struct Observation {
    pub segment: SourceSegment,
    pub help: Option<String>,
}

impl Observation {
    pub fn new(segment: SourceSegment) -> Self {
        Self {
            segment,
            help: None,
        }
    }

    pub fn with_help<'a>(segment: SourceSegment, help: &str) -> Self {
        Self {
            segment,
            help: Some(help.to_string()),
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct Diagnostic {
    pub module: SourceObjectId,
    pub ty: DiagnosticType,
    pub global_message: String,
    pub observations: Vec<Observation>,
    pub tips: Vec<String>,
}

impl Diagnostic {
    pub fn warn(id: WarnID, module: SourceObjectId, msg: &str) -> Self {
        Self {
            module,
            ty: Warn(id),
            global_message: msg.to_string(),
            observations: Vec::new(),
            tips: Vec::new(),
        }
    }

    pub fn error(id: ErrorID, module: SourceObjectId, msg: &str) -> Self {
        Self {
            module,
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
