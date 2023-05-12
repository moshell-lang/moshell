use crate::report::{print_flush, FormattedError};
use context::source::Source;
use dbg_pls::color;
use miette::GraphicalReportHandler;
use parser::parse;
use std::io;
use std::io::BufRead;
use std::io::Write;

pub fn prompt(handler: GraphicalReportHandler) -> io::Result<()> {
    let stdin = io::stdin();
    let lines = stdin.lock().lines();

    print_flush!("=> ");
    let mut content = String::new();
    for line in lines {
        let line = line?;
        content.push_str(&line);
        if line.ends_with('\\') {
            content.push('\n');
            print_flush!(".. ");
            continue;
        }

        let source = Source::new(&content, "stdin");
        let report = parse(source);
        if !report.stack_ended {
            content.push('\n');
            print_flush!(".. ");
            continue; // Silently ignore incomplete input
        }

        let errors = report
            .errors
            .into_iter()
            .map(|err| FormattedError::from(err, &source))
            .collect::<Vec<_>>();

        if errors.is_empty() {
            print_flush!("{}\n=> ", color(&report.expr));
            content.clear();
            continue;
        }

        let mut msg = String::new();
        for err in &errors {
            if let Err(fmt_err) = handler.render_report(&mut msg, err) {
                eprintln!("{fmt_err}");
            } else {
                eprintln!("{msg}");
            }
            msg.clear();
        }
        content.clear();
        print_flush!("=> ");
    }

    Ok(())
}
