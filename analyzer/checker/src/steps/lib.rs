use parser::err::ParseError;
use crate::import::ImportError;

#[derive(Debug)]
pub enum GatherError {
    Import(ImportError),
    Parse(Vec<ParseError>),
    Internal(String),
}

impl From<ImportError> for GatherError {
    fn from(err: ImportError) -> Self {
        GatherError::Import(err)
    }
}