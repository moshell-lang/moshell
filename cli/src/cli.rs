use std::path::PathBuf;

use clap::Parser;
use dbg_pls::color;
use owo_colors::OwoColorize;

use ast::Expr;
use compiler::bytecode::Opcode;
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
    #[arg(short = 'A', long)]
    pub(crate) analyzer: bool,

    /// Activate compiler output visualisation
    #[arg(short = 'B', long)]
    pub(crate) bytecode: bool,
}

pub struct Configuration {
    pub lexer_visualisation: bool,
    pub parser_visualization: bool,
    pub analyzer_visualisation: bool,
    pub bytecode_visualisation: bool,
}

impl Configuration {
    pub fn needs_visualisation(&self) -> bool {
        self.lexer_visualisation
            || self.parser_visualization
            || self.analyzer_visualisation
            || self.bytecode_visualisation
    }
}

impl From<Cli> for Configuration {
    fn from(value: Cli) -> Self {
        Self {
            lexer_visualisation: value.lexer,
            parser_visualization: value.parser,
            analyzer_visualisation: value.parser,
            bytecode_visualisation: value.bytecode,
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
    println!("{}", "AST: ".cyan());
    for expr in exprs {
        println!("{}", color(expr));
    }
}

pub(crate) fn display_bytecode(bytes: &[u8]) {
    println!("{}", "Bytecode: ".green());
    let (ip, constants) = display_constant_pool(bytes);
    println!();
    display_byte_instructions(constants, &bytes[ip..])
}

fn display_byte_instructions(constants: Vec<String>, bytes: &[u8]) {
    println!("\t{}", "Instructions".green());
    let mut ip = 0;
    let bytes_len = bytes.len();

    let byte_padding = bytes_len.to_string().len();

    while ip < bytes_len {
        let opcode: Opcode = bytes[ip].try_into().expect("not an opcode");
        macro_rules! get_usize {
            () => {
                usize::from_be_bytes((&bytes[ip..ip + 8]).try_into().unwrap())
            };
        }

        print!(
            "#{ip:<padding$}: {:7} ",
            opcode.mnemonic(),
            padding = byte_padding
        );

        ip += 1;
        match opcode {
            Opcode::PushInt => {
                print!("<value {}>", get_usize!());
                ip += 8;
            }
            Opcode::PushFloat => {
                print!(
                    "<value {}>",
                    f64::from_be_bytes((&bytes[ip..ip + 8]).try_into().unwrap())
                );
                ip += 8;
            }
            Opcode::PushString => {
                let constant_pos = get_usize!();
                let padding = (byte_padding - constant_pos.to_string().len()) + 10;
                print!(
                    "<constant #{}> {:padding$}// \"{}\"",
                    constant_pos,
                    "",
                    constants[constant_pos],
                    padding = padding
                );
                ip += 8;
            }
            Opcode::GetLocal => {
                print!("<local @{}>", bytes[ip]);
                ip += 1;
            }
            Opcode::SetLocal => {
                print!("<constant @{}>", bytes[ip]);
                ip += 1;
            }
            Opcode::Spawn => {
                print!("<arguments stack size {}>", bytes[ip]);
                ip += 1;
            }
            Opcode::IfJump => {
                print!("<instruction #{}>", get_usize!());
                ip += 8;
            }
            Opcode::IfNotJump => {
                print!("<instruction #{}>", get_usize!());
                ip += 8;
            }
            Opcode::Jump => {
                print!("<instruction #{}>", get_usize!());
                ip += 8;
            }
            _ => {}
        }

        println!()
    }
}

fn display_constant_pool(bytes: &[u8]) -> (usize, Vec<String>) {
    println!("\t{}", "Constant pool".green());

    let count = bytes[0];
    let mut current_byte = 1;

    let mut constants = Vec::new();

    let padding = count.to_string().len();
    for constant_id in 0..count {
        let str_len =
            usize::from_be_bytes((&bytes[current_byte..current_byte + 8]).try_into().unwrap());
        current_byte += 8;
        let str = String::from_utf8(bytes[current_byte..current_byte + str_len].to_vec())
            .expect("not utf8");
        current_byte += str_len;

        println!("#{constant_id:<padding$} <string utf-8> // \"{str}\"");
        constants.push(str);
    }

    (current_byte, constants)
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
