mod exercises;
mod tests;

use crate::Config;
use regex::Regex;
use std::ops::{Index, Range};
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

fn count_line_terminators(fragment: &str) -> usize {
    #[allow(
        clippy::unwrap_used,
        reason = "Regular expressions compiled from literals"
    )]
    let line_terminator = Regex::new("(?:\r\n|\n)").unwrap();
    line_terminator.find_iter(fragment).count()
}

pub fn tokenize<'a>(code: &'a str, config: &Config) -> Result<Tokens<'a>, Error> {
    let patterns = [
        // Whitespace
        (r#"^\s+"#, None),
        // Comment
        (r#"(?m)^//.*$"#, None),
        // Block comment
        (r#"(?s)^/\*.*?\*/"#, None),
        // Identifier and keyword
        (
            r#"^[[:alpha:]_][[:alpha:]_[:digit:]]*"#,
            Some(Kind::Identifier),
        ),
        // Arithmetic operator
        (r#"^[-+*/]"#, Some(Kind::Operator)),
        // Integer
        (r#"^[[:digit:]]+"#, Some(Kind::Integer)),
    ];

    #[allow(
        clippy::unwrap_used,
        reason = "Regular expressions compiled from literals"
    )]
    let res: Vec<(Regex, Option<Kind>)> = patterns
        .iter()
        .map(|(pat, kind)| (Regex::new(pat).unwrap(), *kind))
        .collect();

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

        for (re, kind) in res.iter() {
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
