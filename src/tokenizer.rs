mod exercises;
mod tests;

use crate::{
    Location,
    trace::{end_trace, start_trace, trace},
};
use regex::Regex;
use std::{
    ops::{Deref, Index},
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
    End,
    Integer,
    Operator,
    Punctuation,
    Identifier,
    StrLiteral,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token {
    kind: Kind,
    location: Location,
}

impl Token {
    pub fn end(location: Location) -> Self {
        Self {
            kind: Kind::End,
            location,
        }
    }

    pub fn as_str<'a>(&self, code: &'a str) -> &'a str {
        &code[self.location.start()..self.location.end()]
    }

    pub fn len(&self) -> usize {
        self.location.end() - self.location.start()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn kind(&self) -> Kind {
        self.kind
    }

    pub fn location(&self) -> &Location {
        &self.location
    }
}

#[derive(Debug)]
pub struct Tokens<'a> {
    tokens: Vec<Token>,
    pub code: &'a str,
}

impl Index<usize> for Tokens<'_> {
    type Output = Token;

    fn index(&self, index: usize) -> &Self::Output {
        &self.tokens[index]
    }
}

impl Deref for Tokens<'_> {
    type Target = [Token];

    fn deref(&self) -> &Self::Target {
        self.tokens.deref()
    }
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
        (
            r#"^(?:==|!=|<=|>=|\+|-|\*|/|%|=|<|>)"#,
            Some(Kind::Operator)
        ),
        // Punctuation
        (r#"^[(){},:;]"#, Some(Kind::Punctuation)),
        // Identifier
        (
            r#"^[[:alpha:]_][[:alpha:]_[:digit:]]*"#,
            Some(Kind::Identifier)
        ),
        // String literal
        (r#"^".*?[^\\]""#, Some(Kind::StrLiteral)),
    ]
});

pub fn tokenize(code: &str) -> Result<Tokens<'_>, Error> {
    let mut tokens = Vec::new();
    let mut at: usize = 0;

    start_trace!("Tokenizer");

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

            trace!("{}", mat.as_str());

            // Store the match
            if let Some(kind) = *kind {
                tokens.push(Token {
                    kind,
                    location: (at..at + mat.end()).into(),
                });
            }

            // Advance the tokenizer state
            at += mat.end();

            continue 'outer;
        }

        // All regex were tried without a match, this is an error
        break Err(Error::NoMatch(rest[..rest.len().min(12)].to_owned()));
    };

    end_trace!();

    result
}
