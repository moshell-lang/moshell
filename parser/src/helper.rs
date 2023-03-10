use std::str::Lines;
use context::poller::StringPoller;
use lexer::reader::BufferedTokenReader;

pub fn reader_from_str<'a>(str: String) -> BufferedTokenReader<'a, Lines<'a>> {
    BufferedTokenReader::new(StringPoller::new(str.lines()))
}