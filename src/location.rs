use regex::Regex;
use std::{error::Error, fmt::Display, ops::Range, sync::LazyLock};

#[allow(
    clippy::unwrap_used,
    reason = "Regular expressions compiled from literals"
)]
static LINE_TERMINATOR: LazyLock<Regex> = LazyLock::new(|| Regex::new("(?:\r\n|\n)").unwrap());

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Location(Range<usize>);

impl Location {
    pub fn start(&self) -> usize {
        self.0.start
    }

    pub fn end(&self) -> usize {
        self.0.end
    }

    pub fn line(&self, code: &str) -> usize {
        LINE_TERMINATOR.find_iter(&code[..self.start()]).count() + 1
    }

    pub fn column(&self, code: &str) -> usize {
        let line_start = LINE_TERMINATOR
            .find_iter(&code[..self.start()])
            .last()
            .map(|mat| mat.end())
            .unwrap_or(0);
        self.start() - line_start + 1
    }
}

impl From<Range<usize>> for Location {
    fn from(value: Range<usize>) -> Self {
        Self(value)
    }
}

impl Default for Location {
    fn default() -> Self {
        (0..0).into()
    }
}

#[derive(Debug)]
pub struct ErrorLocation<E: Error + 'static> {
    line: usize,
    column: usize,
    error: E,
}

impl<E: Error> Display for ErrorLocation<E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "At line {}, column {}", self.line, self.column)
    }
}

impl<E: Error> Error for ErrorLocation<E> {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        Some(&self.error)
    }
}

#[expect(dead_code, reason = "This is complicated to implement. See todo.md")]
impl<E: Error> ErrorLocation<E> {
    pub fn new(location: &Location, code: &str, error: E) -> Self {
        Self {
            line: location.line(code),
            column: location.column(code),
            error,
        }
    }
}
