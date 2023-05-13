use parser::err::ParseError;
use crate::importer::ImportError;

pub mod collect;
mod resolve;


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
