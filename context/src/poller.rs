use std::str::Lines;

pub trait Poller<'a, I> {
    fn next(&mut self) -> Option<I>;
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

impl<'a> Poller<'a, &'a str> for StringPoller<'a> {

    fn next(&mut self) -> Option<&'a str> {
        if self.end_of_lines {
            return None
        }
        if let Some(ln) = self.lines.next() {
            return  Some(ln)
        }
        self.end_of_lines = true;
        None
    }
    fn empty(&self) -> bool {
        self.end_of_lines
    }
}