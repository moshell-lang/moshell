use std::path::PathBuf;

use clap::Parser;
use dbg_pls::color;

use ast::Expr;
use lexer::token::Token;

#[derive(Parser, Debug, Clone)]
#[command(author, version, about, long_about = None)]
pub struct Cli {
    /// Defines the source file to evaluate
    #[arg(short, long, value_name = "FILE")]
    pub(crate) source: Option<PathBuf>,

    /// Activate lexer output visualisation
    #[arg(short = 'L', long)]
    pub(crate) lexer: bool,

    /// Activate parser AST visualisation
    #[arg(short = 'P', long)]
    pub(crate) parser: bool,

    /// Activate compiler output visualisation
    #[arg(short = 'B', long)]
    pub(crate) bytecode: bool,

    /// Disable code execution
    #[arg(long = "execute", default_value = "true")]
    pub(crate) execute: bool,
}

pub struct CliConfiguration {
    pub lexer_visualisation: bool,
    pub parser_visualization: bool,
    pub bytecode_visualisation: bool,
    pub execute: bool,
}

impl CliConfiguration {
    pub fn needs_visualisation(&self) -> bool {
        self.lexer_visualisation || self.parser_visualization || self.bytecode_visualisation
    }
}

impl<'a> From<&'a Cli> for CliConfiguration {
    fn from(value: &'a Cli) -> Self {
        Self {
            lexer_visualisation: value.lexer,
            parser_visualization: value.parser,
            bytecode_visualisation: value.bytecode,
            execute: value.execute,
        }
    }
}

pub(crate) fn display_tokens(tokens: Vec<Token>) {
    println!("Lexed tokens:");
    println!("\t- {} tokens lexed from input", tokens.len());

    for (count, token) in tokens.iter().enumerate() {
        print!(
            "{:10}: '{}', ",
            format!("{:?}", token.token_type),
            token.value
        );
        if count % 5 == 0 {
            println!()
        }
    }
    println!()
}

pub(crate) fn display_exprs(exprs: &[Expr]) {
    println!("AST:");
    for expr in exprs {
        println!("{}", color(expr));
    }
}

#[link(name = "vm", kind = "static")]
extern "C" {
    fn exec(bytes: *const u8, byte_count: usize);
}

pub(crate) fn execute(bytes: &[u8]) {
    let len = bytes.len();
    unsafe {
        exec(bytes.as_ptr(), len);
    }
}
