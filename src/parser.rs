// Stringly-Typed JSON Library for Rust
// Written in 2015 by
//   Andrew Poelstra <apoelstra@wpsoftware.net>
//
// To the extent possible under law, the author(s) have dedicated all
// copyright and related and neighboring rights to this software to
// the public domain worldwide. This software is distributed without
// any warranty.
//
// You should have received a copy of the CC0 Public Domain Dedication
// along with this software.
// If not, see <http://creativecommons.org/publicdomain/zero/1.0/>.
//

//! # Parsing support
//!

use encoding::{Encoding, DecoderTrap};
use encoding::all::UTF_16BE;
use std::{error, fmt, io, num};
use std::borrow::Cow;
use std::collections::HashMap;
use serde::iter::LineColIterator;

use {Json, JsonInner, UnsafeStringRef};

/// The type of a Json parsing error
#[derive(Debug)]
pub enum ErrorType {
    /// Expected a string, got something else
    ExpectedString,
    /// end-of-file reached before json was complete
    UnexpectedEOF,
    /// bad character encountered when parsing some data
    UnexpectedCharacter(char),
    /// a number contained a bad or misplaced character
    MalformedNumber,
    /// an escape sequence was invalid
    MalformedEscape,
    /// an identifier was given that has no meaning
    UnknownIdent,
    /// a unicode codepoint constant was malformed
    Unicode(num::ParseIntError),
    /// a series of codepoints could not be parsed as utf16
    Utf16(Cow<'static, str>),
    /// some sort of IO error
    Io(io::Error)
}

impl From<num::ParseIntError> for ErrorType {
    fn from(e: num::ParseIntError) -> ErrorType { ErrorType::Unicode(e) }
}

impl From<Cow<'static, str>> for ErrorType {
    fn from(e: Cow<'static, str>) -> ErrorType { ErrorType::Utf16(e) }
}

impl From<io::Error> for ErrorType {
    fn from(e: io::Error) -> ErrorType { ErrorType::Io(e) }
}

/// A Json parsing error
#[derive(Debug)]
pub struct Error {
    line: usize,
    col: usize,
    error: ErrorType
}

impl Error {
    fn at<I: Iterator<Item=io::Result<u8>>>(iter: &LineColIterator<I>, ty: ErrorType) -> Error {
        Error {
            line: iter.line(),
            col: iter.col(),
            error: ty
        }
    }
}

/// A macro which acts like try! but attaches line/column info to the error
macro_rules! try_at(
    ($s:expr, $e:expr) => (
        match $e {
            Ok(x) => x,
            Err(e) => {
                return Err(Error::at(&$s.iter, e));
            }
        }
    )
);

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.error {
            ErrorType::UnexpectedCharacter(c) => write!(f, "{}:{}: unexpected character {}", self.line, self.col, c),
            ErrorType::Io(ref e) => write!(f, "{}:{}: {}", self.line, self.col, e),
            ErrorType::Unicode(ref e) => write!(f, "{}:{}: {}", self.line, self.col, e),
            ErrorType::Utf16(ref e) => write!(f, "{}:{}: {}", self.line, self.col, e),
            _ => write!(f, "{}:{}: {}", self.line, self.col, error::Error::description(self))
        }
    }
}

impl error::Error for Error {
    fn cause(&self) -> Option<&error::Error> {
        if let ErrorType::Io(ref e) = self.error {
            Some(e)
        } else {
            None
        }
    }

    fn description(&self) -> &str {
        match self.error {
            ErrorType::ExpectedString => "expected string",
            ErrorType::UnexpectedEOF => "unexpected eof",
            ErrorType::UnexpectedCharacter(_) => "bad character",
            ErrorType::MalformedEscape => "bad escape",
            ErrorType::MalformedNumber => "malformed number",
            ErrorType::UnknownIdent => "unknown ident",
            ErrorType::Unicode(ref e) => error::Error::description(e),
            ErrorType::Utf16(ref e) => e,
            ErrorType::Io(ref e) => error::Error::description(e)
        }
    }
}

/// A structure capable of parsing binary ASCII data into a "JSON object",
/// which is simply a tree of strings. Further parsing should be done by
/// other layers.
pub struct Parser<I: Iterator<Item=io::Result<u8>>> {
    iter: LineColIterator<I>,
    ch: Option<u8>
}

impl<I: Iterator<Item=io::Result<u8>>> Parser<I> {
    /// Construct a new parser, given a byte iterator as input
    pub fn new(iter: I) -> Parser<I> {
        Parser {
            iter: LineColIterator::new(iter),
            ch: None
        }
    }

    fn peek(&mut self) -> Result<Option<u8>, ErrorType> {
        match self.ch {
            Some(c) => Ok(Some(c)),
            None => {
                match self.iter.next() {
                    Some(Ok(ch)) => {
                        self.ch = Some(ch);
                        Ok(self.ch)
                    }
                    Some(Err(e)) => {
                        Err(ErrorType::Io(e))
                    }
                    None => Ok(None)
                }
            }
        }
    }

    fn peek_noeof(&mut self) -> Result<u8, ErrorType> {
        match self.peek() {
            Ok(Some(c)) => Ok(c),
            Ok(None) => Err(ErrorType::UnexpectedEOF),
            Err(e) => Err(e)
        }
    }

    fn eat(&mut self) { self.ch = None; }

    fn eat_whitespace(&mut self) -> Result<(), ErrorType> {
        loop {
            match try!(self.peek()) {
                Some(b' ') | Some(b'\n') | Some(b'\r') => {
                    self.eat();
                }
                _ => { return Ok(()); }
            }
        }
    }

    fn eat_ident(&mut self, ident: &'static str) -> Result<(), ErrorType> {
        for c in ident.bytes() {
            if try!(self.peek()) == Some(c) {
                self.eat();
            } else {
                return Err(ErrorType::UnknownIdent);
            }
        }
        Ok(())
    }

    fn parse_number(&mut self) -> Result<String, ErrorType> {
        #[derive(PartialEq)]
        enum State { Start, ZeroStart, PreDecimal, PostDecimal, InExp, PastExp }

        let mut ret = String::new();
        let mut state = State::Start;
        while let Some(c) = try!(self.peek()) {
            match c {
                b'+' => {
                    if state == State::InExp {
                        state = State::PastExp;
                    } else {
                        return Err(ErrorType::UnexpectedCharacter('+'));
                    }
                }
                b'-' => {
                    if state == State::InExp {
                        state = State::PastExp;
                    } else if state != State::Start {
                        return Err(ErrorType::UnexpectedCharacter('-'));
                    }
                }
                b'0' ... b'9' => {
                    if state == State::Start {
                        if c == b'0' {
                            state = State::ZeroStart
                        } else {
                            state = State::PreDecimal;
                        }
                    // Can't start a number with 0, except 0 itself and 0.xyz
                    } else if state == State::ZeroStart {
                        return Err(ErrorType::MalformedNumber);
                    }
                }
                b'.' => {
                    if state == State::PreDecimal || state == State::ZeroStart {
                        state = State::PostDecimal;
                    } else {
                        return Err(ErrorType::MalformedNumber);
                    }
                }
                b' ' | b'\r' | b'\n' | b']' | b',' | b':' => {
                    break;
                }
                b'e' | b'E' => {
                    // e, E, e+, E+, e-, E- may appear at the end of a number. never at the start
                    if state == State::ZeroStart ||
                       state == State::PreDecimal ||
                       state == State::PostDecimal {
                        state = State::InExp;
                    } else {
                        return Err(ErrorType::MalformedNumber);
                    }
                }
                x => {
                    return Err(ErrorType::UnexpectedCharacter(x as char));
                }
            }
            ret.push(c as char);
            self.eat();
        }
        if state == State::Start {
            return Err(ErrorType::MalformedNumber);
        } else {
            Ok(ret)
        }
    }

    /// Consume a string, assuming the first character has been vetted to be '"'.
    fn parse_string(&mut self) -> Result<String, ErrorType> {
        #[derive(PartialEq)]
        enum State { Start, Scanning, Escaping, Done }

        let mut ret = String::new();
        let mut state = State::Start;
        while let Some(mut c) = try!(self.peek()) {
            match c {
                b'"' => {
                    match state {
                        State::Start => { state = State::Scanning; self.eat(); continue; }
                        State::Scanning => { self.eat(); state = State::Done; break; }
                        State::Escaping => { state = State::Scanning; }
                        State::Done => unreachable!()
                    }
                }
                b'\\' => {
                    match state {
                        State::Start => { return Err(ErrorType::ExpectedString); }
                        State::Scanning => { state = State::Escaping; self.eat(); continue; }
                        State::Escaping => { state = State::Scanning; }
                        State::Done => unreachable!()
                    }
                }
                _ => {
                    match state {
                        State::Start => {
                            return Err(ErrorType::ExpectedString);
                        }
                        State::Scanning => {
                            // Do nothing -- after the match we will push this character onto the buffer
                        }
                        State::Escaping => {
                            c = match c {
                                b'b' => 7,
                                b'f' => 12,
                                b'n' => b'\n',
                                b'r' => b'\r',
                                b't' => b'\t',
                                b'/' => b'/',
                                b'\\' => unreachable!(),  // covered above in the main b'\\' branch
                                b'u' => {
                                    // Read as many \uXXXX's in a row as we can, then parse them all as
                                    // UTF16, according to ECMA 404 p10
                                    let mut utf16_be: Vec<u8> = vec![];
                                    loop {
                                        // Parse codepoint
                                        self.eat();
                                        let mut num_str = String::new();
                                        num_str.push(try!(self.peek_noeof()) as char); self.eat();
                                        num_str.push(try!(self.peek_noeof()) as char); self.eat();
                                        utf16_be.push(try!(u8::from_str_radix(&num_str[..], 16)));
                                        num_str = String::new();
                                        num_str.push(try!(self.peek_noeof()) as char); self.eat();
                                        num_str.push(try!(self.peek_noeof()) as char); self.eat();
                                        utf16_be.push(try!(u8::from_str_radix(&num_str[..], 16)));
                                        // Check if another codepoint follows
                                        if try!(self.peek()) == Some(b'\\') {
                                            self.eat();
                                            if try!(self.peek()) != Some(b'u') {
                                                state = State::Escaping;
                                                break;
                                            }
                                        } else {
                                            state = State::Scanning;
                                            break;
                                        }
                                    }

                                    let s = try!(UTF_16BE.decode(&utf16_be[..], DecoderTrap::Strict));
                                    ret.push_str(&s[..]);
                                    continue;
                                }
                                _ => { return Err(ErrorType::MalformedEscape); }
                            };
                            state = State::Scanning;
                        }
                        State::Done => unreachable!()
                    }
                }
            }
            ret.push(c as char);
            self.eat();
        }
        if state == State::Done {
            Ok(ret)
        } else {
            Err(ErrorType::UnexpectedEOF)
        }
    }

    /// Consume the internal iterator and produce a Json object
    pub fn parse(&mut self) -> Result<Json, Error> {
        try_at!(self, self.eat_whitespace());

        let first_ch = match self.peek() {
            Ok(Some(c)) => c,
            Ok(None) => {
                return Err(Error::at(&self.iter, ErrorType::UnexpectedEOF));
            },
            Err(e) => {
                return Err(Error::at(&self.iter, e));
            }
        };

        match first_ch {
            // keywords
            b'n' => {
                try_at!(self, self.eat_ident("null"));
                Ok(Json(JsonInner::Null))
            }
            b't' => {
                try_at!(self, self.eat_ident("true"));
                Ok(Json(JsonInner::Bool(true)))
            }
            b'f' => {
                try_at!(self, self.eat_ident("false"));
                Ok(Json(JsonInner::Bool(false)))
            }
            // numbers
            b'-' | b'0' ... b'9' => {
                Ok(Json(JsonInner::Number(try_at!(self, self.parse_number()))))
            }
            // strings
            b'"' | b'\'' => {
                Ok(Json(JsonInner::String(try_at!(self, self.parse_string()))))
            }
            // arrays
            b'[' => {
                self.eat();
                let mut ret = vec![];
                loop {
                    try_at!(self, self.eat_whitespace());
                    if !(ret.is_empty() && try_at!(self, self.peek_noeof()) == b']') {
                        ret.push(try!(self.parse()));
                        try_at!(self, self.eat_whitespace());
                    }
                    match try_at!(self, self.peek_noeof()) {
                        b',' => { self.eat(); }
                        b']' => { self.eat(); break; }
                        _ => { return Err(Error::at(&self.iter, ErrorType::UnknownIdent)); }
                    }
                }
                Ok(Json(JsonInner::Array(ret)))
            }
            // objects TODO
            b'{' => {
                self.eat();
                let mut ret = HashMap::new();  // actual map of strings to objects
                let mut refs = vec![];         // vector of references to keys, used to maintain ordering
                loop {
                    try_at!(self, self.eat_whitespace());
                    // special-case {}
                    if ret.is_empty() && try_at!(self, self.peek_noeof()) == b'}' {
                        self.eat();
                        break;
                    }
                    // parse key
                    let mut key = try_at!(self, self.parse_string());
                    try_at!(self, self.eat_whitespace());
                    // parse : separator
                    let sep_ch = try_at!(self, self.peek_noeof());
                    if sep_ch == b':' {
                        self.eat();
                        try_at!(self, self.eat_whitespace());
                    } else {
                        return Err(Error::at(&self.iter, ErrorType::UnexpectedCharacter(sep_ch as char)));
                    }
                    // parse value
                    let val = try!(self.parse());
                    // Unsafety as we are taking an unsafe reference to the hashmap keys.
                    // This is safe because the API of this object does not allow the
                    // hashmap to be destroyed without also destroying the references.
                    // Further, we promise not to mutate the keys once they're in the
                    // map.
                    unsafe { refs.push(UnsafeStringRef::from_string(&mut key)); }
                    ret.insert(key, val);
                    try_at!(self, self.eat_whitespace());
                    // parse , separator
                    match try_at!(self, self.peek_noeof()) {
                        b',' => { self.eat(); },
                        b'}' /* { */ => { self.eat(); break; }
                        x => { return Err(Error::at(&self.iter, ErrorType::UnexpectedCharacter(x as char))); }
                    }
                }
                Ok(Json(JsonInner::Object(ret, refs)))
            }
            _ => Err(Error::at(&self.iter, ErrorType::UnknownIdent))
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use {Json, JsonInner, UnsafeStringRef};

    macro_rules! jnull( () => (Json(JsonInner::Null)) );
    macro_rules! jbool( ($e:expr) => (Json(JsonInner::Bool($e))) );
    macro_rules! jnum( ($e:expr) => (Json(JsonInner::Number($e.to_owned()))) );
    macro_rules! jstr( ($e:expr) => (Json(JsonInner::String($e.to_owned()))) );
    macro_rules! jarr( ($($e:expr),*) => (Json(JsonInner::Array(vec![$($e),*]))) );
    macro_rules! jobj( ($($k:expr => $v:expr),*) => ({
        let mut vec = vec![];
        let mut map = HashMap::new();
        &mut map;  /* dummy "use as mut" to avoid errors in case of no inserts */
        &mut vec;
        $(
            let mut s = $k.to_owned();
            // Unsafety ok because unit test ;)
            unsafe { vec.push(UnsafeStringRef::from_string(&mut s)); }
            map.insert(s, $v);
        )*
        Json(JsonInner::Object(map, vec))
    }) );

    #[test]
    fn test_primitives() {
        assert_eq!(Json::from_str("null").unwrap(), jnull!());
        assert_eq!(Json::from_str("  true  ").unwrap(), jbool!(true));
        assert_eq!(Json::from_str(" false ").unwrap(), jbool!(false));

        assert_eq!(Json::from_str("\"\\n\\r\\t\\b\\f \\\\ \\/\"").unwrap(), jstr!("\n\r\t\u{7}\u{c} \\ /"));
        assert_eq!(Json::from_str("\"\\\"\"").unwrap(), jstr!("\""));
        assert_eq!(Json::from_str(" \"string\"").unwrap(), jstr!("string"));
        assert_eq!(Json::from_str("\"i've \\\"ed this\"").unwrap(), jstr!("i've \"ed this"));
        assert_eq!(Json::from_str(" \"\\u0020\"").unwrap(), jstr!(" "));
        assert_eq!(Json::from_str(" \"\\uffff\\t\"").unwrap(), jstr!("\u{ffff}\t"));
        assert_eq!(Json::from_str(" \"\\ud834\\uDD1E\"").unwrap(), jstr!("\u{1d11e}"));

        assert_eq!(Json::from_str(" 0").unwrap(), jnum!("0"));
        assert_eq!(Json::from_str("-0").unwrap(), jnum!("-0"));
        assert_eq!(Json::from_str("  101").unwrap(), jnum!("101"));
        assert_eq!(Json::from_str("101.99").unwrap(), jnum!("101.99"));
        assert_eq!(Json::from_str("-101.99  ").unwrap(), jnum!("-101.99"));
        assert_eq!(Json::from_str("-10e55").unwrap(), jnum!("-10e55"));
        assert_eq!(Json::from_str("-10.1e55").unwrap(), jnum!("-10.1e55"));
        assert_eq!(Json::from_str("-10e+55").unwrap(), jnum!("-10e+55"));
        assert_eq!(Json::from_str("-10e-55").unwrap(), jnum!("-10e-55"));
        assert_eq!(Json::from_str("-1E+5").unwrap(), jnum!("-1E+5"));
        assert_eq!(Json::from_str("-1E-5").unwrap(), jnum!("-1E-5"));

        assert!(Json::from_str("").is_err());
        assert!(Json::from_str("gibberish").is_err());
        assert!(Json::from_str("\"\\c\"").is_err());
        assert!(Json::from_str("\"\\u\"").is_err());
        assert!(Json::from_str("\"\\u123\"").is_err());
        assert!(Json::from_str("\"\\ud800\"").is_err());
        assert!(Json::from_str("\"\\udd1e\\ud834\"").is_err());
        assert!(Json::from_str("\"\\uf+ff\"").is_err());
        assert!(Json::from_str("\"").is_err());
        assert!(Json::from_str("\"\\").is_err());
        assert!(Json::from_str(".5").is_err());
        assert!(Json::from_str("9.5.5").is_err());
        assert!(Json::from_str("-").is_err());
        assert!(Json::from_str("+").is_err());
        assert!(Json::from_str("+1").is_err());
        assert!(Json::from_str("1e2.5").is_err());
        assert!(Json::from_str("1e2e5").is_err());
        assert!(Json::from_str("0123").is_err());
        assert!(Json::from_str("3f").is_err());
        assert!(Json::from_str("00").is_err());
        assert!(Json::from_str("2-3").is_err());
        assert!(Json::from_str("2+3").is_err());
    }

    #[test]
    fn test_array() {
        assert_eq!(Json::from_str("[]").unwrap(), jarr![]);
        assert_eq!(Json::from_str("[\"1\"]").unwrap(), jarr![jstr!("1")]);
        assert_eq!(Json::from_str("[\"1\", 2]").unwrap(), jarr![jstr!("1"), jnum!("2")]);

        assert_eq!(Json::from_str("[true, [false, 2], 3]").unwrap(),
                   jarr![jbool!(true), jarr![jbool!(false), jnum!("2")], jnum!("3")]);
        assert_eq!(Json::from_str("[[[[[]]]]]").unwrap(), jarr![jarr![jarr![jarr![jarr![]]]]]);

        assert!(Json::from_str("[").is_err());
        assert!(Json::from_str("]").is_err());
        assert!(Json::from_str("[1 2]").is_err());
        assert!(Json::from_str("[,1]").is_err());
        assert!(Json::from_str("[1,]").is_err());
        assert!(Json::from_str("[,1,2]").is_err());
        assert!(Json::from_str("[1,,2]").is_err());
        assert!(Json::from_str("[1,2,]").is_err());
    }

    #[test]
    fn test_object() {
        assert_eq!(Json::from_str("{}").unwrap(), jobj![]);
        assert_eq!(Json::from_str("{\"key\": \"val\"}").unwrap(), jobj!["key" => jstr!("val")]);
        assert_eq!(Json::from_str("{\"key\": false}").unwrap(), jobj!["key" => jbool!(false)]);
        assert_eq!(Json::from_str("{\"key\": []}").unwrap(), jobj!["key" => jarr![]]);

        assert_eq!(Json::from_str("{\"key1\": \"val\", \"key2\": \"val\"}").unwrap(), jobj!["key1" => jstr!("val"), "key2" => jstr!("val")]);
        // Unsure what the best behaviour is here, but adding a test
        // so we'll notice if the current behaviour changes.
        assert_eq!(Json::from_str("{\"key\": \"val\", \"key\": \"val2\"}").unwrap(), jobj!["key" => jstr!("val2")]);

        assert!(Json::from_str("{{}}").is_err());
        assert!(Json::from_str("{,}").is_err());
        assert!(Json::from_str("{:}").is_err());
        assert!(Json::from_str("{\\\"}").is_err());
        assert!(Json::from_str("{\"key\" \"val\"}").is_err());
        assert!(Json::from_str("{\"key\": \"val\" \"val2\"}").is_err());
        assert!(Json::from_str("{{\"key\": \"val\"}}").is_err());
        assert!(Json::from_str("{null: \"val\"}").is_err());
        assert!(Json::from_str("{true: \"val\"}").is_err());
        assert!(Json::from_str("{false: \"val\"}").is_err());
        assert!(Json::from_str("{[]: \"val\"}").is_err());
        assert!(Json::from_str("{10: \"val\"}").is_err());
        assert!(Json::from_str("{: \"val\"}").is_err());
        assert!(Json::from_str("{\"key\": }").is_err());
        assert!(Json::from_str("{\"key1\": , \"key2\": \"val\"}").is_err());
        assert!(Json::from_str("{\"key1\": \"val\", \"key2\":}").is_err());
        assert!(Json::from_str("{\"key1\": \"val\",, \"key2\":\"val\"}").is_err());
        assert!(Json::from_str("{,\"key1\": \"val\", \"key2\":\"val\"}").is_err());
        assert!(Json::from_str("{\"key1\": \"val\", \"key2\":\"val\",}").is_err());
    }

    #[test]
    fn test_error() {
        let e = Json::from_str("10+5").unwrap_err();
        assert_eq!(e.line, 1);
        assert_eq!(e.col, 3);
        assert_eq!(e.to_string(), "1:3: unexpected character +");
    }
}


