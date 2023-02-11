#[cfg(test)]
mod tests {
    use std::fs;
    use crate::linter::lex;
    use crate::token::{Token, TokenType};

    #[test]
    fn run_sample() {
        let contents = fs::read_to_string("sample.msh")
            .expect("Cannot open file sample.msh");

        let tokens = lex(&contents);
        println!("{:?}", tokens);
    }
}