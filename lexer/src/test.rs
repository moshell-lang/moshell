#[cfg(test)]
mod tests {
    use std::fs;
    use crate::lexer::lex;


    #[test]
    fn test_lexer() {
        let contents = fs::read_to_string("sample.msh")
            .expect("Cannot open file sample.msh");

        let tokens = lex(&contents);
        for token in tokens {
            println!("{:?}", token)
        }
    }
}