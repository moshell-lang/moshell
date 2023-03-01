use miette::{Diagnostic, NamedSource, Result, SourceSpan};
use parser::parse;
use parser::source::Source;
use std::fmt::Display;
use std::fs;
use thiserror::Error;

#[derive(Error, Debug, Diagnostic)]
struct FormattedError {
    #[source_code]
    src: NamedSource,
    #[label("Here")]
    cursor: SourceSpan,
    message: String,
}

impl Display for FormattedError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

fn this_fails() -> Result<()> {
    let src = fs::read_to_string("lexer/tests/sample.msh").unwrap();
    let source = Source {
        source: &src,
        name: "sample.msh".to_string(),
    };
    parse(source).map_err(|err| FormattedError {
        src: NamedSource::new("sample.msh", src.clone()),
        cursor: err.position.into(),
        message: err.message,
    })?;

    Ok(())
}

fn main() -> Result<()> {
    this_fails()?;
    Ok(())
}
