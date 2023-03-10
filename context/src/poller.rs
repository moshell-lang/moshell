use std::convert::Infallible;
use std::str::Lines;

pub trait Poller<'a, I> {
    type Error;
    fn next(&mut self) -> Result<Option<I>, Self::Error>;
    fn empty(&self) -> bool;
}

pub struct StringPoller<'a> {
    lines: Lines<'a>,
    end_of_lines: bool,
}

impl<'a> StringPoller<'a> {
    pub fn new(lines: Lines<'a>) -> Self {
        Self {
            lines,
            end_of_lines: false
        }
    }
}

impl<'a> Poller<'a, String> for StringPoller<'a> {
    type Error = Infallible;

    fn next(&mut self) -> Result<Option<String>, Infallible> {
        if self.end_of_lines {
            return Ok(None)
        }
        if let Some(ln) = self.lines.next() {
            return  Ok(Some(ln.to_string()))
        }
        self.end_of_lines = true;
        Ok(None)
    }
    fn empty(&self) -> bool {
        self.end_of_lines
    }
}