use std::fmt::{self, Display, Write};

pub fn fmt_comma_separated<T: Display, W: Write>(
    start: char,
    end: char,
    types: &[T],
    w: &mut W,
) -> fmt::Result {
    write!(w, "{start}")?;
    for (i, t) in types.iter().enumerate() {
        if i > 0 {
            write!(w, ", ")?;
        }
        write!(w, "{t}")?;
    }
    write!(w, "{end}")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fmt_comma_separated_str() {
        let mut buf = String::new();
        fmt_comma_separated('[', ']', &["a", "b", "c"], &mut buf).unwrap();
        assert_eq!(buf, "[a, b, c]");
    }
}
