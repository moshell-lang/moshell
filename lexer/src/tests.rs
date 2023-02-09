#[cfg(test)]
mod tests {
    use std::fs;
    use logos::Logos;
    use crate::Token;

    #[test]
    fn run_sample() {
        let contents = fs::read_to_string("sample.msh")
            .expect("Cannot open file sample.msh");

        let mut lexer = Token::lexer(&contents);

        while let Some(token) = lexer.next() {
            println!("{:?} {:?}", token, lexer.slice());
        }
    }
}