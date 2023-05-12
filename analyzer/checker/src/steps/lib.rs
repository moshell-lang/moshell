use crate::import::ImportError;
use parser::err::ParseError;

#[derive(Debug, PartialEq)]
pub enum GatherError {
    Import(ImportError),
    Parse(Vec<ParseError>),
    Other(String),
}

impl From<ImportError> for GatherError {
    fn from(err: ImportError) -> Self {
        GatherError::Import(err)
    }
}
