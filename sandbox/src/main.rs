use context::source::Source;
use miette::{Diagnostic, NamedSource, Result, SourceSpan};
use parser::err::ParseErrorKind;
use parser::parse;
use std::fmt::Display;
use std::fs;
use thiserror::Error;

#[derive(Error, Debug, Diagnostic)]
#[error("Parse error")]
struct Errors {
    #[related]
    src: Vec<FormattedError>,
}

#[derive(Error, Debug, Diagnostic)]
struct FormattedError {
    #[source_code]
    src: NamedSource,
    #[label("Here")]
    cursor: SourceSpan,
    #[label("Start")]
    related: Option<SourceSpan>,
    message: String,
    #[help]
    help: Option<String>,
}

impl Display for FormattedError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

fn this_fails() -> Result<()> {
    let src = fs::read_to_string("lexer/tests/sample.msh").unwrap();
    let source = Source::new(&src, "sample.msh");
    let parsed = parse(source);
    let errors = parsed
        .errors
        .into_iter()
        .map(|err| FormattedError {
            src: NamedSource::new("sample.msh", src.clone()),
            cursor: err.position.into(),
            related: match &err.kind {
                ParseErrorKind::Unpaired(pos) => Some(pos.clone().into()),
                _ => None,
            },
            message: err.message,
            help: match &err.kind {
                ParseErrorKind::Expected(excepted) => Some(format!("Expected: {:?}", excepted)),
                _ => None,
            },
        })
        .collect::<Vec<FormattedError>>();
    if !errors.is_empty() {
        Err(Errors { src: errors }.into())
    } else {
        Ok(())
    }
}

fn main() -> Result<()> {
    this_fails()?;
    Ok(())
}
