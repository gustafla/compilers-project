#![cfg(test)]

use super::*;

impl PartialEq<&[&str]> for Tokens<'_> {
    fn eq(&self, other: &&[&str]) -> bool {
        if self.tokens.len() != other.len() {
            return false;
        }

        for (tok, &string) in self.tokens.iter().zip(*other) {
            if tok.as_str(self.code) != string {
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
    assert_eq!(tokens, &["while", "1", "+", "11", "*", "123"]);
    let code = r#"0012-0x"#;
    let tokens = tokenize(code, &Default::default()).unwrap();
    assert_eq!(tokens, &["0012", "-", "0", "x"]);
}

#[test]
fn tokenizer_unrecognized() {
    let code = "$";
    let result = tokenize(code, &Default::default());
    assert!(matches!(result, Err(Error::NoMatch(_))));
}

#[test]
fn tokenizer_comment() {
    let code = r#"while 1 do // This is a comment
    thing"#;
    let tokens = tokenize(code, &Default::default()).unwrap();
    assert_eq!(tokens, &["while", "1", "do", "thing"]);

    let code = r#"while 1 do /* This is a comment */ rust"#;
    let tokens = tokenize(code, &Default::default()).unwrap();
    assert_eq!(tokens, &["while", "1", "do", "rust"]);

    let code = r#"while 1 do /* This is a comment
    also this*/ rust"#;
    let tokens = tokenize(code, &Default::default()).unwrap();
    assert_eq!(tokens, &["while", "1", "do", "rust"]);

    let code = r#"/**/while 1 do/*This is a comment
also this*/rust//wow"#;
    let tokens = tokenize(code, &Default::default()).unwrap();
    assert_eq!(tokens, &["while", "1", "do", "rust"]);

    let code = r#"//
rust//wow"#;
    let tokens = tokenize(code, &Default::default()).unwrap();
    assert_eq!(tokens, &["rust"]);
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
        &["while", "1", "do", "thing", "thing2", "thing3", "thing4", "last"]
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
