use std::path::PathBuf;

use clap::Parser;
use dbg_pls::color;

use analyzer::relations::SourceId;
use analyzer::types::engine::TypedEngine;
use ast::Expr;
use compiler::compile;
use lexer::token::Token;
use owo_colors::OwoColorize;

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
    #[arg(short = 'A', long)]
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

#[link(name = "vm", kind = "static")]
extern "C" {
    fn exec(bytes: *const u8, byte_count: usize);
}


fn execute(types: TypedEngine) {
    let mut bytes: Vec<u8> = Vec::new();
    compile(&types.get(SourceId(0)).unwrap().expression, &mut bytes).expect("write failed");

    let len = bytes.len();

    unsafe {
        exec(bytes.as_ptr(), len);
    }
}
