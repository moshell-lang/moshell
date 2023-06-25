use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::io;
use std::io::Write;

use analyzer::diagnostic::ObservationTag;
use ariadne::{Cache, Label, Report, ReportKind};
use concolor::Stream;
use yansi::{Color, Paint};

use analyzer::relations::{ObjectId, SourceId};
use context::source::Source;
use parser::err::{ParseError, ParseErrorKind};
use std::fmt::Write as FmtWrite;

/// Associates a [`SourceId`] to the appropriate [`ariadne::Source`] type.
///
/// This cache is used to integrate with the ariadne crate, and is not used for
/// any other purpose.
pub struct SourcesCache<'a, S>
where
    S: Fn(SourceId) -> Source<'a>,
{
    /// supplier used to fetch a source bound to given [SourceId]
    supplier: S,
    names: HashMap<SourceId, String>,
    sources: HashMap<String, ariadne::Source>,
}

impl<'a, S> SourcesCache<'a, S>
where
    S: Fn(SourceId) -> Source<'a>,
{
    pub fn new(supplier: S) -> Self {
        Self {
            supplier,
            names: HashMap::new(),
            sources: HashMap::new(),
        }
    }
}

impl<'a, S> Cache<SourceId> for SourcesCache<'a, S>
where
    S: Fn(SourceId) -> Source<'a>,
{
    fn fetch(&mut self, id: &SourceId) -> Result<&ariadne::Source, Box<dyn Debug + '_>> {
        let pos = self.names.get(id);
        match pos {
            Some(name) => Ok(self.sources.get(name).expect("unable to find source")),
            None => {
                let source = (self.supplier)(*id);
                self.names.insert(*id, source.name.to_string());
                let src = self
                    .sources
                    .entry(source.name.to_string())
                    .or_insert_with(|| ariadne::Source::from(source.source));
                Ok(src)
            }
        }
    }

    fn display<'b>(&self, id: &'b SourceId) -> Option<Box<dyn Display + 'b>> {
        let source_name = self.names.get(id).expect("unable to find source").clone();
        Some(Box::new(source_name))
    }
}

pub fn render_parse_error(source: Source, error: ParseError, w: &mut impl Write) -> io::Result<()> {
    let source_name = source.name;
    let mut builder = Report::build(ReportKind::Error, source_name, 0)
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

    builder
        .finish()
        .write((source_name, ariadne::Source::from(source.source)), w)
}

pub fn render_diagnostic<'a, S>(
    diagnostic: analyzer::diagnostic::Diagnostic,
    cache: &mut SourcesCache<'a, S>,
    w: &mut impl Write,
) -> io::Result<()>
where
    S: Fn(SourceId) -> Source<'a>,
{
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

        if o.tag == ObservationTag::Expected {
            label = label.with_order(100)
        }

        let color = get_color(o.tag);
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

fn get_color(tag: ObservationTag) -> Color {
    match tag {
        ObservationTag::InFault => Color::Red,
        ObservationTag::Declaration => Color::Yellow,
        ObservationTag::Expected => Color::Green,
    }
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
