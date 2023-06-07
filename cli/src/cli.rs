use std::path::PathBuf;

use clap::Parser;
use colored::Colorize;
use dbg_pls::color;
use miette::SourceSpan;

use ast::Expr;
use context::source::SourceSegment;
use lexer::token::Token;

#[derive(Parser, Debug, Clone)]
#[command(author, version, about, long_about = None)]
pub struct Cli {
    /// Defines the source file to evaluate
    #[arg(short, long, value_name = "FILE")]
    pub(crate) source: Option<PathBuf>,

    /// Defines the working directory of the cli.
    #[arg(short, long, value_name = "FILE")]
    pub(crate) working_dir: Option<PathBuf>,

    /// Activate lexer output visualisation
    #[arg(short = 'L', long)]
    pub(crate) lexer: bool,

    /// Activate parser AST visualisation
    #[arg(short = 'P', long)]
    pub(crate) parser: bool,

    /// Activate analyzer relations visualisation
    #[arg(short = 'A', long, default_value = "true")]
    pub(crate) analyzer: bool,
}

pub struct Configuration {
    pub lexer_visualisation: bool,
    pub parser_visualization: bool,
    pub analyzer_visualisation: bool,
}

impl Configuration {
    pub fn needs_visualisation(&self) -> bool {
        self.lexer_visualisation || self.parser_visualization || self.analyzer_visualisation
    }
}

impl From<Cli> for Configuration {
    fn from(value: Cli) -> Self {
        Self {
            lexer_visualisation: value.lexer,
            parser_visualization: value.parser,
            analyzer_visualisation: value.parser,
        }
    }
}

pub(crate) fn display_tokens(tokens: Vec<Token>) {
    println!("{}", "Lexer tokens: ".bright_black());
    println!("\t- {} tokens lexed from input", tokens.len());

    let mut count = 0;
    for token in tokens {
        print!(
            "{:10}: '{}', ",
            format!("{:?}", token.token_type).bright_blue(),
            token.value.white()
        );
        count += 1;
        if count % 5 == 0 {
            println!()
        }
    }
    println!()
}

pub(crate) fn display_exprs(exprs: &Vec<Expr>) {
    for expr in exprs {
        println!("{}", color(expr));
    }
}

pub fn offset_empty_span(span: SourceSegment) -> SourceSpan {
    if span.start == span.end {
        (span.start - 1..span.end).into()
    } else {
        span.into()
    }
}
