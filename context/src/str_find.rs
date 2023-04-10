use crate::source::SourceSegment;

/// Finds the byte segment of the first match of needle in input source.
///
/// # Panics
/// Panics if the needle is not found.
pub fn find_in<'a>(source: &'a str, needle: &'a str) -> SourceSegment {
    let start = source.find(needle).expect("String not found.");
    start..start + needle.len()
}

/// Finds the byte segment of the nth match of needle in input source.
///
/// # Panics
/// Panics if the needle is not found.
pub fn find_in_nth<'a>(source: &'a str, needle: &'a str, nth: usize) -> SourceSegment {
    let start = source
        .match_indices(needle)
        .nth(nth)
        .expect("String not found.")
        .0;
    start..start + needle.len()
}

/// Finds the byte segment of the first match between start and end in input source.
///
/// The start and end are included in the segment.
///
/// # Panics
/// Panics if the start or end is not found, or if the end is before the start.
pub fn find_between<'a>(source: &'a str, start: &'a str, end: &'a str) -> SourceSegment {
    let start = source.find(start).expect("Start not found.");
    let end = source[start + 1..].find(end).expect("End not found.") + 1 + end.len();
    start..start + end
}

/// Finds the byte segment of the last match between start and end in input source.
pub fn rfind_between<'a>(source: &'a str, start: &'a str, end: &'a str) -> SourceSegment {
    let end = source.rfind(end).expect("End not found.");
    let start = source[..end].rfind(start).expect("Start not found.");
    start..end + 1
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn find_in_ascii() {
        let source = "Hello, world!";
        let segment = find_in(source, "world");
        assert_eq!(segment, 7..12);
    }

    #[test]
    fn find_in_nth_ascii() {
        let source = "Hello, world! Hello, world!";
        let segment = find_in_nth(source, "world", 1);
        assert_eq!(segment, 21..26);
    }

    #[test]
    fn find_in_utf8() {
        let source = "Hello, 🦀!";
        let segment = find_in(source, "🦀");
        assert_eq!(segment, 7..11);
    }

    #[test]
    fn find_in_nth_utf8() {
        let source = "🦀🦀";
        let segment = find_in_nth(source, "🦀", 1);
        assert_eq!(segment, 4..8);
    }

    #[test]
    fn find_between_ascii() {
        let source = "if (a) { b }";
        let segment = find_between(source, "(", ")");
        assert_eq!(segment, 3..6);
    }

    #[test]
    fn find_between_utf8() {
        let source = "if (🦀) { b }";
        let segment = find_between(source, "(", ")");
        assert_eq!(segment, 3..9);
    }

    #[test]
    fn rfind_between_ascii() {
        let source = "{x}{y}{z}";
        let segment = rfind_between(source, "{", "}");
        assert_eq!(segment, 6..9);
    }
}
