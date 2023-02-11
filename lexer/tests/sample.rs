use lexer::lexer::lex;

#[test]
fn run_sample() {
    let content = include_str!("sample.msh");

    let tokens = lex(content);
    println!("{:?}", tokens);
}
