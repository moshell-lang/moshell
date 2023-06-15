use std::io;

use ariadne::{Color, Config, Label, Report, ReportKind};

use crate::fix_ariadne_report;
use context::source::Source;
use parser::err::{ParseError, ParseErrorKind};

pub fn render_parse_error(source: Source, error: ParseError) -> io::Result<String> {
    let source_name = source.name;
    let mut builder = Report::build(ReportKind::Error, source_name, 0)
        .with_config(Config::default().with_underlines(false))
        .with_message(error.message)
        .with_label(
            Label::new((source_name, error.position))
                .with_color(Color::Red)
                .with_message("Here"),
        );

    builder = match error.kind {
        ParseErrorKind::Expected(e) => builder.with_help(format!("expected: {e}")),
        ParseErrorKind::UnexpectedInContext(e) => builder.with_help(e),
        ParseErrorKind::Unpaired(pos) => builder.with_label(
            Label::new((source_name, pos))
                .with_message("Start")
                .with_color(Color::Green)
                .with_order(1),
        ),
        _ => builder,
    };

    let mut buf = Vec::new();

    builder.finish().write_for_stdout(
        (source_name, ariadne::Source::from(source.source)),
        &mut buf,
    )?;

    let str = fix_ariadne_report(String::from_utf8(buf).unwrap());
    Ok(str)
}
