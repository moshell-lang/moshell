use miette::{Diagnostic, NamedSource, Result, SourceSpan};
use parser::parse_source;
use parser::source::SourceCode;
use std::fmt::Display;
use std::fs;
use thiserror::Error;

#[derive(Error, Debug, Diagnostic)]
struct FormattedError {
    #[source_code]
    src: NamedSource,
    #[label("Here")]
    cursor: Option<SourceSpan>,
    message: String,
}

impl Display for FormattedError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

fn this_fails() -> Result<()> {
    let src = fs::read_to_string("lexer/tests/sample.msh")
        .unwrap();
    let source = SourceCode {
        source: &src,
        name: "sample.msh".to_string(),
    };
    parse_source(source).map_err(|err| {
        let position = err.position.map(|p| (p.start, p.length).into());
        FormattedError {
            src: NamedSource::new("sample.msh", src.clone()),
            cursor: position,
            message: err.message,
        }
    })?;

    Ok(())
}

fn main() -> Result<()> {
    this_fails()?;
    Ok(())
}
