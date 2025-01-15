#![cfg(test)]

use regex::Match;

macro_rules! re {
    ($expr: literal) => {{
        ::regex::Regex::new($expr).unwrap()
    }};
}

#[test]
fn regex_hello() {
    let re = re!(r"^hello!*$");
    assert!(re.is_match("hello"));
    assert!(re.is_match("hello!"));
    assert!(re.is_match("hello!!"));
    assert!(re.is_match("hello!!!"));
    assert!(!re.is_match("Hello!"));
}

#[test]
fn regex_hello_there() {
    let re = re!(r"^hello( +there)?!*$");
    assert!(re.is_match("hello"));
    assert!(re.is_match("hello!!"));
    assert!(re.is_match("hello there!!!"));
    assert!(re.is_match("hello    there"));
    assert!(!re.is_match("hellothere"));
    assert!(!re.is_match("hello !"));
}

#[test]
fn regex_arithmetic_operator() {
    let re = re!(r"[+\-*/]");
    assert!(re.is_match("hello+there"));
    assert!(re.is_match("hello-there"));
    assert!(re.is_match("hello*there"));
    assert!(re.is_match("hello/there"));

    let mat = re.find("6+ 32").unwrap();
    assert_eq!(mat.range(), 1..2);

    let mat: Vec<Match> = re.find_iter("c++").collect();
    assert_eq!(mat.len(), 2);
    assert_eq!(mat[0].range(), 1..2);
    assert_eq!(mat[1].range(), 2..3);

    let mat: Vec<Match> = re.find_iter("pos - target").collect();
    assert_eq!(mat.len(), 1);
    assert_eq!(mat[0].range(), 4..5);
    assert_eq!(mat[0].as_str(), "-");
}

#[test]
fn regex_integer() {
    let re = re!(r"-?[0-9]+");
    assert!(re.is_match("7"));
    assert!(re.is_match("123"));
    assert!(re.is_match("-95"));
    let mat = re.find("The year 1998").unwrap();
    assert_eq!(mat.range(), 9..13);
    let mat = re.find("1000000th visitor!").unwrap();
    assert_eq!(mat.as_str(), "1000000");
}

#[test]
fn regex_quoted_string_literal() {
    let re = re!(r#""(.*)""#);
    assert!(re.is_match(r#""Hello world!""#));
    assert!(re.is_match(r#""""#));
    assert!(!re.is_match(r#"""#));
    let cap = re.captures(r#""Rust or Bust".to_owned()"#).unwrap();
    assert_eq!(cap.get(1).unwrap().as_str(), "Rust or Bust");
}

#[test]
fn regex_quoted_string_literal_with_escaped_quotes() {
    let re = re!(r#""(.*)""#);
    let cap = re.captures(r#""\"Rust or \"Bust\"\"".to_owned()"#).unwrap();
    assert_eq!(cap.get(1).unwrap().as_str(), r#"\"Rust or \"Bust\"\""#);
}

#[test]
fn regex_identifier() {
    let re = re!(r"[[:alpha:]_][[:alpha:]_[:digit:]]*");
    assert!(!re.is_match("7"));
    assert!(re.is_match("i7"));
    let mat: Vec<Match> = dbg!(re.find_iter("pos - target").collect());
    assert_eq!(mat.len(), 2);
    assert_eq!(mat[0].as_str(), "pos");
    assert_eq!(mat[1].as_str(), "target");
    let mat: Vec<Match> = dbg!(re.find_iter("f64::atan2(y, x)").collect());
    assert_eq!(mat.len(), 4);
    assert_eq!(mat[0].as_str(), "f64");
    assert_eq!(mat[1].as_str(), "atan2");
    assert_eq!(mat[2].as_str(), "y");
    assert_eq!(mat[3].as_str(), "x");
    assert_eq!(re.find("_core_i7").unwrap().as_str(), "_core_i7");
}
