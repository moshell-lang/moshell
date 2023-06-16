use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::io;
use std::ops::Add;

use analyzer::diagnostic::ObservationTag;
use analyzer::relations::SourceId;
use ariadne::{Cache, Color, ColorGenerator, Config, Label, Report, ReportKind};
use regex::Regex;
use yansi::Paint;

use context::source::Source;

use crate::fix_ariadne_report;

pub fn render_diagnostic<'a>(
    diagnostic: analyzer::diagnostic::Diagnostic,
    source_supplier: impl Fn(SourceId) -> Source<'a>,
) -> io::Result<String> {
    let mut colors = ColorGenerator::new();
    let mut colormap = HashMap::new();
    let mut sources = HashMap::new();

    fn get_color(
        colors: &mut ColorGenerator,
        colormap: &mut HashMap<u8, Color>,
        tag: Option<ObservationTag>,
    ) -> Color {
        match tag {
            Some(ObservationTag::InFault) => Color::Red,
            Some(ObservationTag::Declaration) => Color::Yellow,
            Some(ObservationTag::Expected) => Color::Green,
            Some(ObservationTag::Other(tag)) => {
                *colormap.entry(tag).or_insert_with(|| colors.next())
            }
            None => colors.next(),
        }
    }

    let main_source = source_supplier(diagnostic.source);
    sources.insert(diagnostic.source, main_source);

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
        let source = o.foreign_env.unwrap_or(diagnostic.source);
        let source = *sources
            .entry(source)
            .or_insert_with(|| source_supplier(source));

        let mut label = Label::new((source.name, o.segment));

        if let Some(ObservationTag::Expected) = o.tag {
            label = label.with_order(100)
        }

        let color = get_color(&mut colors, &mut colormap, o.tag);
        label = label.with_color(color);

        if let Some(help) = o.help {
            label = label.with_message(colorize_message(help, color))
        }
        label
    });

    let help = agg_strings(diagnostic.helps);
    let note = agg_strings(diagnostic.notes);

    let mut builder = Report::build(ReportKind::Custom(&code, color), main_source.name, 0)
        .with_config(Config::default().with_underlines(false))
        .with_message(colorize_message(diagnostic.global_message, Color::Blue))
        .with_labels(labels);

    if let Some(help) = help {
        builder = builder.with_help(colorize_message(help, Color::Green))
    }

    if let Some(note) = note {
        builder = builder.with_note(colorize_message(note, Color::Cyan))
    }

    let cache = sources
        .into_values()
        .map(|s| (s.name, ariadne::Source::from(s.source)))
        .collect::<HashMap<_, _>>();

    let cache = CacheMap { cache };

    let mut buf = Vec::new();
    builder.finish().write_for_stdout(cache, &mut buf)?;

    let str = String::from_utf8(buf).unwrap();
    Ok(fix_ariadne_report(str))
}

fn agg_strings(strings: Vec<String>) -> Option<String> {
    let mut agg = None;
    if let Some((head, tail)) = strings.split_first() {
        agg = if tail.is_empty() {
            Some(head.clone())
        } else {
            let notes_agg = tail
                .iter()
                .fold(format!("\n- {head}"), |acc, str| format!("{acc}\n- {str}"));
            Some(notes_agg)
        }
    }
    agg
}

fn colorize_message(msg: String, color: Color) -> String {
    let regex = Regex::new(r"`([^`]*)`").unwrap();

    let mut last = 0;

    let mut color_msg = String::new();

    for cap in regex.captures_iter(&msg) {
        if let Some(mach) = cap.get(1) {
            let start = mach.start();
            let end = mach.end();

            color_msg = color_msg.add(&msg[last..start]);
            let colorized = Paint::new(&msg[start..end]).fg(color).to_string();
            color_msg = color_msg.add(&colorized);

            last = end;
        }
    }
    color_msg.add(&msg[last..])
}

struct CacheMap<'a> {
    cache: HashMap<&'a str, ariadne::Source>,
}

impl<'a> Cache<&'a str> for CacheMap<'a> {
    fn fetch(&mut self, id: &&'a str) -> Result<&ariadne::Source, Box<dyn Debug + '_>> {
        Ok(self.cache.get(id).unwrap())
    }

    fn display<'b>(&self, id: &'b &'a str) -> Option<Box<dyn Display + 'b>> {
        Some(Box::new(id))
    }
}
