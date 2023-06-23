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
    #[arg(long = "no-exec")]
    pub(crate) no_exec: bool,
}

pub struct Configuration {
    pub lexer_visualisation: bool,
    pub parser_visualization: bool,
    pub bytecode_visualisation: bool,
}

impl Configuration {
    pub fn needs_visualisation(&self) -> bool {
        self.lexer_visualisation || self.parser_visualization || self.bytecode_visualisation
    }
}

impl From<Cli> for Configuration {
    fn from(value: Cli) -> Self {
        Self {
            lexer_visualisation: value.lexer,
            parser_visualization: value.parser,
            bytecode_visualisation: value.bytecode,
        }
    }
}

pub(crate) fn display_tokens(tokens: Vec<Token>) {
    println!("Lexer tokens:");
    println!("\t- {} tokens lexed from input", tokens.len());

    let mut count = 0;
    for token in tokens {
        print!(
            "{:10}: '{}', ",
            format!("{:?}", token.token_type),
            token.value
        );
        count += 1;
        if count % 5 == 0 {
            println!()
        }
    }
    println!()
}

pub(crate) fn display_exprs(exprs: &Vec<Expr>) {
    println!("AST:");
    for expr in exprs {
        println!("{}", color(expr));
    }
}

#[link(name = "vm", kind = "static")]
extern "C" {
    fn exec(bytes: *const u8, byte_count: usize);
}

pub(crate) fn execute(bytes: Vec<u8>) {
    let len = bytes.len();
    unsafe {
        exec(bytes.as_ptr(), len);
    }
}
