#![cfg(test)]

use super::*;

impl PartialEq<&[(&str, Kind)]> for Tokens<'_> {
    fn eq(&self, other: &&[(&str, Kind)]) -> bool {
        dbg!((self.tokens.len(), other.len()));
        if self.tokens.len() != other.len() {
            return false;
        }

        for (tok, (string, kind)) in self.tokens.iter().zip(*other) {
            dbg!((tok.kind, *kind));
            if tok.kind != *kind {
                return false;
            }
            dbg!((tok.as_str(self.code), string));
            if tok.as_str(self.code) != *string {
                return false;
            }
        }

        true
    }
}

#[test]
fn tokenizer_basics() {
    let code = r#"while 1 + 11*123"#;
    let tokens = tokenize(code, &Default::default()).unwrap();
    assert_eq!(
        tokens,
        &[
            ("while", Kind::Identifier),
            ("1", Kind::Integer),
            ("+", Kind::Operator),
            ("11", Kind::Integer),
            ("*", Kind::Operator),
            ("123", Kind::Integer),
        ]
    );
    let code = r#"0012-0x"#;
    let tokens = tokenize(code, &Default::default()).unwrap();
    assert_eq!(
        tokens,
        &[
            ("0012", Kind::Integer),
            ("-", Kind::Operator),
            ("0", Kind::Integer),
            ("x", Kind::Identifier),
        ]
    );
}

#[test]
fn tokenizer_unrecognized() {
    let code = "$";
    let result = tokenize(code, &Default::default());
    assert!(matches!(result, Err(Error::NoMatch(_))));
}

#[test]
fn tokenizer_operators() {
    let code = "+-*/===!=<<=>>=";
    let tokens = tokenize(code, &Default::default()).unwrap();
    assert_eq!(
        tokens,
        &[
            ("+", Kind::Operator),
            ("-", Kind::Operator),
            ("*", Kind::Operator),
            ("/", Kind::Operator),
            ("==", Kind::Operator),
            ("=", Kind::Operator),
            ("!=", Kind::Operator),
            ("<", Kind::Operator),
            ("<=", Kind::Operator),
            (">", Kind::Operator),
            (">=", Kind::Operator),
        ]
    );
}

#[test]
fn tokenizer_punctuation() {
    let code = r#"int main(int argc, char **argv) {
    printf("Hello World!");
    return 0;
}"#;
    let tokens = tokenize(code, &Default::default()).unwrap();
    assert_eq!(
        tokens,
        &[
            ("int", Kind::Identifier),
            ("main", Kind::Identifier),
            ("(", Kind::Punctuation),
            ("int", Kind::Identifier),
            ("argc", Kind::Identifier),
            (",", Kind::Punctuation),
            ("char", Kind::Identifier),
            ("*", Kind::Operator),
            ("*", Kind::Operator),
            ("argv", Kind::Identifier),
            (")", Kind::Punctuation),
            ("{", Kind::Punctuation),
            ("printf", Kind::Identifier),
            ("(", Kind::Punctuation),
            ("\"Hello World!\"", Kind::StrLiteral),
            (")", Kind::Punctuation),
            (";", Kind::Punctuation),
            ("return", Kind::Identifier),
            ("0", Kind::Integer),
            (";", Kind::Punctuation),
            ("}", Kind::Punctuation),
        ]
    );
}

#[test]
fn tokenizer_comment() {
    let code = r#"while 1 do // This is a comment
    thing"#;
    let tokens = tokenize(code, &Default::default()).unwrap();
    assert_eq!(
        tokens,
        &[
            ("while", Kind::Identifier),
            ("1", Kind::Integer),
            ("do", Kind::Identifier),
            ("thing", Kind::Identifier)
        ]
    );

    let code = r#"while 1 do /* This is a comment */ rust"#;
    let tokens = tokenize(code, &Default::default()).unwrap();
    assert_eq!(
        tokens,
        &[
            ("while", Kind::Identifier),
            ("1", Kind::Integer),
            ("do", Kind::Identifier),
            ("rust", Kind::Identifier)
        ]
    );

    let code = r#"while 1 do /* This is a comment
    also this*/ rust"#;
    let tokens = tokenize(code, &Default::default()).unwrap();
    assert_eq!(
        tokens,
        &[
            ("while", Kind::Identifier),
            ("1", Kind::Integer),
            ("do", Kind::Identifier),
            ("rust", Kind::Identifier)
        ]
    );

    let code = r#"/**/while 1 do/*This is a comment
also this*/rust#wow"#;
    let tokens = tokenize(code, &Default::default()).unwrap();
    assert_eq!(
        tokens,
        &[
            ("while", Kind::Identifier),
            ("1", Kind::Integer),
            ("do", Kind::Identifier),
            ("rust", Kind::Identifier),
        ]
    );

    let code = r#"//
rust//wow"#;
    let tokens = tokenize(code, &Default::default()).unwrap();
    assert_eq!(tokens, &[("rust", Kind::Identifier)]);
}

#[test]
fn tokenizer_line_numbers() {
    let code = r#"while 1 do // This is a comment
    thing
    thing2
    thing3
    thing4/*
    thing5
    thing6
*/last"#;
    let tokens = tokenize(code, &Default::default()).unwrap();
    assert_eq!(
        tokens,
        &[
            ("while", Kind::Identifier),
            ("1", Kind::Integer),
            ("do", Kind::Identifier),
            ("thing", Kind::Identifier),
            ("thing2", Kind::Identifier),
            ("thing3", Kind::Identifier),
            ("thing4", Kind::Identifier),
            ("last", Kind::Identifier)
        ]
    );

    assert_eq!(tokens[0].line(code), 1);
    assert_eq!(tokens[1].line(code), 1);
    assert_eq!(tokens[2].line(code), 1);
    assert_eq!(tokens[3].line(code), 2);
    assert_eq!(tokens[4].line(code), 3);
    assert_eq!(tokens[5].line(code), 4);
    assert_eq!(tokens[6].line(code), 5);
    assert_eq!(tokens[7].line(code), 8);
}
