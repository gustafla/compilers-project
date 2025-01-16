mod exercises;
mod tests;

use crate::Config;
use regex::Regex;
use std::{
    ops::{Index, Range},
    sync::LazyLock,
};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Tokenizer could not match input \"{0}...\"")]
    NoMatch(String),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Kind {
    Integer,
    Operator,
    Punctuation,
    Identifier,
    StrLiteral,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Location(Range<usize>);

impl Location {
    pub fn line(&self, code: &str) -> usize {
        count_line_terminators(&code[..self.0.start]) + 1
    }

    pub fn start(&self) -> usize {
        self.0.start
    }

    pub fn end(&self) -> usize {
        self.0.end
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Token {
    kind: Kind,
    location: Location,
}

impl Token {
    pub fn as_str<'a>(&self, code: &'a str) -> &'a str {
        &code[self.location.start()..self.location.end()]
    }

    pub fn line(&self, code: &str) -> usize {
        self.location.line(code)
    }
}

#[derive(Debug)]
pub struct Tokens<'a> {
    tokens: Vec<Token>,
    code: &'a str,
}

impl Tokens<'_> {
    pub fn tokens(&self) -> &[Token] {
        &self.tokens
    }

    pub fn get(&self, index: usize) -> Option<&Token> {
        self.tokens.get(index)
    }

    pub fn code(&self) -> &str {
        self.code
    }
}

impl Index<usize> for Tokens<'_> {
    type Output = Token;

    fn index(&self, index: usize) -> &Self::Output {
        &self.tokens[index]
    }
}

#[allow(
    clippy::unwrap_used,
    reason = "Regular expressions compiled from literals"
)]
static LINE_TERMINATOR: LazyLock<Regex> = LazyLock::new(|| Regex::new("(?:\r\n|\n)").unwrap());

fn count_line_terminators(fragment: &str) -> usize {
    LINE_TERMINATOR.find_iter(fragment).count()
}

macro_rules! regex_array{
    [$(($pat: literal, $kind: expr)),*$(,)?] => {
        [
            $(
                (::regex::Regex::new($pat).unwrap(), $kind),
            )*
        ]
    };
}

#[allow(
    clippy::unwrap_used,
    reason = "Regular expressions compiled from literals"
)]
static REGULAR_EXPRESSIONS: LazyLock<[(Regex, Option<Kind>); 8]> = LazyLock::new(|| {
    regex_array![
        // Whitespace
        (r#"^\s+"#, None),
        // Comment
        (r#"(?m)^(?://|#).*$"#, None),
        // Block comment
        (r#"(?s)^/\*.*?\*/"#, None),
        // Integer
        (r#"^[[:digit:]]+"#, Some(Kind::Integer)),
        // Operator
        (r#"^(?:==|!=|<=|>=|\+|-|\*|/|=|<|>)"#, Some(Kind::Operator)),
        // Punctuation
        (r#"^[(){},;]"#, Some(Kind::Punctuation)),
        // Identifier
        (
            r#"^[[:alpha:]_][[:alpha:]_[:digit:]]*"#,
            Some(Kind::Identifier)
        ),
        // String literal
        (r#"^".*?[^\\]""#, Some(Kind::StrLiteral)),
    ]
});

pub fn tokenize<'a>(code: &'a str, config: &Config) -> Result<Tokens<'a>, Error> {
    let mut tokens = Vec::new();
    let mut at: usize = 0;

    if config.verbose {
        eprintln!("Tokenizer: ");
    }

    let result = 'outer: loop {
        let rest = &code[at..];

        if rest.is_empty() {
            break Ok(Tokens { tokens, code });
        }

        for (re, kind) in REGULAR_EXPRESSIONS.iter() {
            // Try current regex
            // Don't use Regex::find_at(), it considers the context around the haystack, so the ^-anchor won't match
            let Some(mat) = re.find(rest) else {
                continue;
            };

            // This is invariant with our regular expressions
            #[cfg(debug_assertions)]
            {
                assert!(!mat.is_empty());
                assert_eq!(mat.start(), 0);
            }

            // Debug print
            if config.verbose {
                eprint!("{}", mat.as_str());
            }

            // Store the match
            if let Some(kind) = *kind {
                tokens.push(Token {
                    kind,
                    location: Location(at..at + mat.end()),
                });
            }

            // Advance the tokenizer state
            at += mat.end();

            continue 'outer;
        }

        // All regex were tried without a match, this is an error
        break Err(Error::NoMatch(rest[..rest.len().min(12)].to_owned()));
    };

    if config.verbose {
        eprintln!();
    }

    result
}
