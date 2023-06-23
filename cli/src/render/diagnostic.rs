use std::fmt::Write as FmtWrite;
use std::io;
use std::io::Write;

use ariadne::{Cache, Color, ColorGenerator, Label, Report, ReportKind};
use concolor::Stream;
use yansi::Paint;

use analyzer::diagnostic::ObservationTag;
use analyzer::relations::{ObjectId, SourceId};
use context::source::Source;

use crate::render::SourcesCache;

pub fn render_diagnostic<'a, S>(
    diagnostic: analyzer::diagnostic::Diagnostic,
    cache: &mut SourcesCache<S>,
    w: &mut impl Write,
) -> io::Result<()>
where
    S: Fn(SourceId) -> Source<'a>,
{
    let mut colors = ColorGenerator::new();

    fn get_color(colors: &mut ColorGenerator, tag: Option<ObservationTag>) -> Color {
        match tag {
            Some(ObservationTag::InFault) => Color::Red,
            Some(ObservationTag::Declaration) => Color::Yellow,
            Some(ObservationTag::Expected) => Color::Green,
            None => colors.next(),
        }
    }

    let (code, color) = if diagnostic.identifier.critical() {
        (
            format!("error[E{:04}]", diagnostic.identifier.code()),
            Color::Red,
        )
    } else {
        (
            format!("warn[W{:04}]", diagnostic.identifier.code()),
            Color::Yellow,
        )
    };

    let labels = diagnostic.observations.into_iter().map(|o| {
        let mut label = Label::new((o.source, o.segment));
        cache.fetch(&o.source).expect("Could not fetch source");

        if let Some(ObservationTag::Expected) = o.tag {
            label = label.with_order(100)
        }

        let color = get_color(&mut colors, o.tag);
        label = label.with_color(color);

        if let Some(help) = o.help {
            label = label.with_message(colorize_message(help, color))
        }
        label
    });

    let help = agg_strings(diagnostic.helps);
    let note = agg_strings(diagnostic.notes);

    let mut builder = Report::build(
        ReportKind::Custom(&code, color),
        SourceId(ObjectId::MAX), /*this id is never used*/
        0,
    )
    .with_message(colorize_message(diagnostic.global_message, Color::Blue))
    .with_labels(labels);

    if let Some(help) = help {
        builder = builder.with_help(colorize_message(help, Color::Green))
    }

    if let Some(note) = note {
        builder = builder.with_note(colorize_message(note, Color::Cyan))
    }

    builder.finish().write(cache, w)
}

fn agg_strings(mut strings: Vec<String>) -> Option<String> {
    if let Some(last) = strings.pop() {
        if strings.is_empty() {
            return Some(last);
        }

        let mut builder = String::new();
        for note in strings {
            writeln!(builder, "- {note}").unwrap();
        }
        write!(builder, "- {last}").unwrap();
        return Some(builder);
    }
    None
}

/// parses given string and colorizes segments between anti quotes (`) with given color only if ansi colors
/// are supported by stderr
fn colorize_message(msg: String, color: Color) -> String {
    if !concolor::get(Stream::Stderr).ansi_color() {
        return msg;
    }
    let parts = msg.split('`');

    let mut color_msg = String::new();

    let mut colorize = false;

    for part in parts {
        if !colorize {
            colorize = true;
            color_msg.push_str(part);
            continue;
        }

        let colorized = Paint::new(part).fg(color).to_string();
        write!(color_msg, "`{colorized}`").unwrap();
        colorize = false;
    }
    color_msg
}
