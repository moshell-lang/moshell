use std::io;

use ariadne::{Color, ColorGenerator, Config, Label, Report, ReportKind};

use context::source::Source;

use crate::fix_ariadne_report;

pub fn render_diagnostic(
    source_code: Source,
    diagnostic: analyzer::diagnostic::Diagnostic,
) -> io::Result<String> {
    let mut colors = ColorGenerator::new();

    let source_name = source_code.name;


    let (code, color) = if diagnostic.identifier.critical() {
        (format!("error[E{:04}]", diagnostic.identifier.code()), Color::Red)
    } else {
        (format!("warn[W{:04}]", diagnostic.identifier.code()), Color::Yellow)
    };

    let labels = diagnostic
        .observations
        .into_iter()
        .map(|o| {
            let mut label = Label::new((source_name, o.segment))
                .with_color(colors.next());
            if let Some(help) = o.help {
                label = label.with_message(help)
            }
            label
        });

    let mut help = None;
    if let Some((head, tail)) = diagnostic.helps.split_first() {
        help = if tail.is_empty() {
            Some(head.clone())
        } else {
            let helps_agg = tail.iter().fold(format!("\n- {head}"), |acc, help| {
                format!("{acc}\n- {help}")
            });
            Some(helps_agg)
        }
    }


    let mut builder = Report::build(ReportKind::Custom(&code, color), source_name, 0)
        .with_config(
            Config::default()
                .with_compact(true)
                .with_underlines(true)
        )
        .with_message(diagnostic.global_message)
        .with_labels(labels);

    if let Some(help) = help {
        builder = builder.with_help(help)
    }

    let mut buf = Vec::new();
    builder
        .finish()
        .write_for_stdout((source_name, ariadne::Source::from(source_code.source)), &mut buf)?;

    let str = String::from_utf8(buf).unwrap();
    Ok(fix_ariadne_report(str))
}
