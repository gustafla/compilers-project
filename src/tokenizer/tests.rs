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

    assert_eq!(tokens[0].location().line(code), 1);
    assert_eq!(tokens[1].location().line(code), 1);
    assert_eq!(tokens[2].location().line(code), 1);
    assert_eq!(tokens[3].location().line(code), 2);
    assert_eq!(tokens[4].location().line(code), 3);
    assert_eq!(tokens[5].location().line(code), 4);
    assert_eq!(tokens[6].location().line(code), 5);
    assert_eq!(tokens[7].location().line(code), 8);
}

#[test]
fn tokenizer_column_numbers() {
    let code = r#"int main(int argc, char **argv) {
    printf("Hello World!");
    return 0;
}"#;
    let tokens = tokenize(code, &Default::default()).unwrap();

    let columns: Vec<usize> = tokens.iter().map(|t| t.location().column(code)).collect();

    assert_eq!(
        columns,
        &[
            1,  // int
            5,  // main
            9,  // (
            10, // int
            14, // argc
            18, // ,
            20, // char
            25, // *
            26, // *
            27, // argv
            31, // )
            33, // {
            5,  // printf
            11, // (
            12, // "Hello World!"
            26, // )
            27, // ;
            5,  // return
            12, // 0
            13, // ;
            1,  // }
        ]
    );

    let lines: Vec<usize> = tokens.iter().map(|t| t.location().line(code)).collect();

    assert_eq!(
        lines,
        &[
            1, // int
            1, // main
            1, // (
            1, // int
            1, // argc
            1, // ,
            1, // char
            1, // *
            1, // *
            1, // argv
            1, // )
            1, // {
            2, // printf
            2, // (
            2, // "Hello World!"
            2, // )
            2, // ;
            3, // return
            3, // 0
            3, // ;
            4, // }
        ]
    );
}
