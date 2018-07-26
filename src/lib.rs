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

//! # Strason -- Stringly-Typed JSON parser and serializer for Rust
//!

#![crate_type = "lib"]
#![crate_type = "rlib"]
#![crate_type = "dylib"]
#![crate_name = "strason"]

// Coding conventions
#![deny(non_upper_case_globals)]
#![deny(non_camel_case_types)]
#![deny(non_snake_case)]
#![deny(unused_mut)]
#![warn(missing_docs)]

// Clippy whitelist
#![cfg_attr(feature = "clippy", allow(match_same_arms))]  // many false positives

extern crate serde;
#[cfg(test)] extern crate serde_json;

use serde::ser;
use std::{fmt, io, ops};

pub mod parser;
pub mod serializer;
pub mod object;

pub use parser::Error;
pub use object::from_serialize;

#[derive(Clone, PartialEq, Eq, Debug)]
enum JsonInner {
    /// A literal "null"
    Null,
    /// A boolean
    Bool(bool),
    /// A number
    Number(String),
    /// A string
    String(String),
    /// An array of other Json objects
    Array(Vec<Json>),
    /// An ordered map of Strings to Json objects
    Object(Vec<(String, Json)>)
}

/// A "stringly-typed" Json object. That is, either a value (represented
/// as a String), or an object (represented as a map from Strings to Jsons).
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Json(JsonInner);

impl Json {
    /// Construct a Json object by parsing a byte iterator, e.g. from a Reader
    pub fn from_iter<I: Iterator<Item=io::Result<u8>>>(it: I) -> Result<Json, parser::Error>  {
        parser::Parser::new(it).parse()
    }

    /// Construct a Json object by parsing a string
    pub fn from_str(s: &str) -> Result<Json, parser::Error> {
        Json::from_iter(s.bytes().map(Ok))
    }

    /// Construct a Json object from a reader
    pub fn from_reader<R: io::Read>(r: R) -> Result<Json, parser::Error> {
        Json::from_iter(r.bytes())
    }

    /// Construct a Json object from a Serialize type
    pub fn from_serialize<T: ser::Serialize>(t: &T) -> Result<Json, parser::Error> {
        object::from_serialize(t)
    }

    /// Returns a null, if this is a null
    pub fn null(&self) -> Option<()> { if let JsonInner::Null = self.0 { Some(()) } else { None } }
    /// Returns the value, if this is a boolean
    pub fn bool(&self) -> Option<bool> { if let JsonInner::Bool(x) = self.0 { Some(x) } else { None } }
    /// Returns the value, if this is a number
    pub fn num(&self) -> Option<&str> { if let JsonInner::Number(ref x) = self.0 { Some(&x[..]) } else { None } }
    /// Returns the value, if this is a string
    pub fn string(&self) -> Option<&str> { if let JsonInner::String(ref x) = self.0 { Some(&x[..]) } else { None } }
    /// Returns the value, if this is an array
    pub fn array(&self) -> Option<&[Json]> { if let JsonInner::Array(ref x) = self.0 { Some(&x[..]) } else { None } }
    /// Returns the value, if this is an object
    pub fn object(&self) -> Option<&[(String, Json)]> { if let JsonInner::Object(ref x) = self.0 { Some(&x[..]) } else { None } }

    /// Obtain a reference to a specified member, if this is an object
    pub fn get(&self, index: &str) -> Option<&Json> {
        if let JsonInner::Object(ref v) = self.0 {
            for &(ref key, ref obj) in v {
                if key == index {
                    return Some(obj);
                }
            }
        }
        None
    }

    /// Return the number of subobjects this object represents
    /// (so a count for Arrays and Objects). NOT a string length.
    pub fn len(&self) -> usize {
        match self.0 {
            JsonInner::Null => 0,
            JsonInner::Bool(_) => 1,
            JsonInner::String(_) => 1,
            JsonInner::Number(_) => 1,
            JsonInner::Array(ref v) => v.len(),
            JsonInner::Object(ref v) => v.len()
        }
    }

    /// Return whether the object is empty, if it is a collection
    pub fn is_empty(&self) -> bool {
        match self.0 {
            JsonInner::Null => true,
            JsonInner::Bool(_) => false,
            JsonInner::String(_) => false,
            JsonInner::Number(_) => false,
            JsonInner::Array(ref v) => v.is_empty(),
            JsonInner::Object(ref v) => v.is_empty()
        }
    }

    /// Reserialize the object into a writer
    pub fn write_to<W: io::Write>(&self, mut w: W) -> io::Result<()> {
        serializer::serialize(self, &mut w)
    }

    /// Reserialize the object to byte array
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut ret = vec![];
        self.write_to(&mut ret).unwrap();
        ret
    }

    /// Convert the Json object to something deserializable
    pub fn into_deserialize<T: serde::de::Deserialize>(self) -> Result<T, Error> {
        object::into_deserialize(self)
    }
}

impl From<()> for Json {
    fn from(_: ()) -> Json {
        Json(JsonInner::Null)
    }
}

impl From<bool> for Json {
    fn from(t: bool) -> Json {
        Json(JsonInner::Bool(t))
    }
}

macro_rules! format_from_impl(
    ($t:ty) => {
        impl From<$t> for Json {
            fn from(n: $t) -> Json {
                Json(JsonInner::Number(format!("{}", n)))
            }
        }
    }
);

format_from_impl!(usize);
format_from_impl!(u64);
format_from_impl!(u32);
format_from_impl!(u16);
format_from_impl!(u8);
format_from_impl!(isize);
format_from_impl!(i64);
format_from_impl!(i32);
format_from_impl!(i16);
format_from_impl!(i8);
format_from_impl!(f64);
format_from_impl!(f32);

impl From<String> for Json {
    fn from(s: String) -> Json {
        Json(JsonInner::String(s))
    }
}

impl<'a> From<&'a str> for Json {
    fn from(s: &'a str) -> Json {
        Json(JsonInner::String(s.to_owned()))
    }
}

impl From<Vec<Json>> for Json {
    fn from(v: Vec<Json>) -> Json {
        Json(JsonInner::Array(v))
    }
}

impl From<Vec<(String, Json)>> for Json {
    fn from(v: Vec<(String, Json)>) -> Json {
        Json(JsonInner::Object(v))
    }
}

impl fmt::Display for Json {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use std::fmt::Write;

        match self.0 {
            JsonInner::Null => f.write_str("null"),
            JsonInner::Bool(false) => f.write_str("false"),
            JsonInner::Bool(true) => f.write_str("true"),
            JsonInner::Number(ref nstr) => f.write_str(nstr),
            JsonInner::String(ref sstr) => write!(f, "\"{}\"", sstr),
            JsonInner::Array(ref v) => {
                try!(f.write_char('['));
                if !v.is_empty() {
                    try!(write!(f, " {}", v[0]));
                }
                for elem in v.iter().skip(1) {
                    try!(write!(f, ", {}", elem));
                }
                f.write_str(" ]")
            }
            JsonInner::Object(ref v) => {
                try!(f.write_char('{'));
                if !v.is_empty() {
                    try!(write!(f, " \"{}\": {}", v[0].0, v[0].1));
                }
                for elem in v.iter().skip(1) {
                    try!(write!(f, ", \"{}\": {}", elem.0, elem.1));
                }
                f.write_str(" }")
            }
        }
    }
}

impl<'a> ops::Index<&'a str> for Json {
    type Output = Json;
    #[inline]
    fn index(&self, index: &'a str) -> &Json {
        if let JsonInner::Object(ref v) = self.0 {
            for &(ref key, ref obj) in v {
                if key == index {
                    return obj;
                }
            }
            panic!("Json object accessed with non-existent index!");
        } else {
            panic!("Tried to index a non-object Json object as an object!");
        }
    }
}

impl ops::Index<usize> for Json {
    type Output = Json;
    #[inline]
    fn index(&self, index: usize) -> &Json {
        if let JsonInner::Array(ref v) = self.0 {
            &v[index]
        } else {
            panic!("Tried to index a non-array Json object as an array!");
        }
    }
}

impl ops::Index<ops::Range<usize>> for Json {
    type Output = [Json];
    #[inline]
    fn index(&self, index: ops::Range<usize>) -> &[Json] {
        if let JsonInner::Array(ref v) = self.0 {
            &v[index]
        } else {
            panic!("Tried to index a non-array Json object as an array!");
        }
    }
}

impl ops::Index<ops::RangeTo<usize>> for Json {
    type Output = [Json];
    #[inline]
    fn index(&self, index: ops::RangeTo<usize>) -> &[Json] {
        if let JsonInner::Array(ref v) = self.0 {
            &v[index]
        } else {
            panic!("Tried to index a non-array Json object as an array!");
        }
    }
}

impl ops::Index<ops::RangeFrom<usize>> for Json {
    type Output = [Json];
    #[inline]
    fn index(&self, index: ops::RangeFrom<usize>) -> &[Json] {
        if let JsonInner::Array(ref v) = self.0 {
            &v[index]
        } else {
            panic!("Tried to index a non-array Json object as an array!");
        }
    }
}

impl ops::Index<ops::RangeFull> for Json {
    type Output = [Json];
    #[inline]
    fn index(&self, index: ops::RangeFull) -> &[Json] {
        if let JsonInner::Array(ref v) = self.0 {
            &v[index]
        } else {
            panic!("Tried to index a non-array Json object as an array!");
        }
    }
}

#[cfg(test)]
mod tests {
    use {Json, JsonInner};

    fn ck_null(obj: &Json) {
        assert!(obj.null().is_some());
        assert!(obj.bool().is_none());
        assert!(obj.num().is_none());
        assert!(obj.string().is_none());
        assert!(obj.array().is_none());
        assert!(obj.object().is_none());
    }

    fn ck_bool(obj: &Json) {
        assert!(obj.null().is_none());
        assert!(obj.bool().is_some());
        assert!(obj.num().is_none());
        assert!(obj.string().is_none());
        assert!(obj.array().is_none());
        assert!(obj.object().is_none());
    }

    fn ck_num(obj: &Json) {
        assert!(obj.null().is_none());
        assert!(obj.bool().is_none());
        assert!(obj.num().is_some());
        assert!(obj.string().is_none());
        assert!(obj.array().is_none());
        assert!(obj.object().is_none());
    }

    fn ck_string(obj: &Json) {
        assert!(obj.null().is_none());
        assert!(obj.bool().is_none());
        assert!(obj.num().is_none());
        assert!(obj.string().is_some());
        assert!(obj.array().is_none());
        assert!(obj.object().is_none());
    }

    fn ck_array(obj: &Json) {
        assert!(obj.null().is_none());
        assert!(obj.bool().is_none());
        assert!(obj.num().is_none());
        assert!(obj.string().is_none());
        assert!(obj.array().is_some());
        assert!(obj.object().is_none());
    }

    fn ck_object(obj: &Json) {
        assert!(obj.null().is_none());
        assert!(obj.bool().is_none());
        assert!(obj.num().is_none());
        assert!(obj.string().is_none());
        assert!(obj.array().is_none());
        assert!(obj.object().is_some());
    }

    #[test]
    fn test_is() {
        ck_null(&Json::from_str("null").unwrap());
        ck_bool(&Json::from_str("true").unwrap());
        ck_bool(&Json::from_str("false").unwrap());
        ck_num(&Json::from_str("-10").unwrap());
        ck_num(&Json::from_str("10").unwrap());
        ck_num(&Json::from_str("0.100e9").unwrap());
        ck_string(&Json::from_str("\"str\"").unwrap());

        ck_array(&Json::from_str("[]").unwrap());
        ck_array(&Json::from_str("[\"t\"]").unwrap());
        ck_array(&Json::from_str("[\"t\", true, false]").unwrap());

        ck_object(&Json::from_str("{}").unwrap());
        ck_object(&Json::from_str("{\"key\": true}").unwrap());
    }

    #[test]
    fn test_index() {
        let arr = Json::from_str("[1, 2, 3]").unwrap();
        assert_eq!(arr[0].0, JsonInner::Number("1".to_owned()));
        assert_eq!(arr[1].0, JsonInner::Number("2".to_owned()));
        assert_eq!(arr[2].0, JsonInner::Number("3".to_owned()));
        assert_eq!(arr.len(), 3);
        assert!(!arr.is_empty());

        let obj = Json::from_str("{\"vrai\": true, \"faux\": false}").unwrap();
        assert_eq!(obj["vrai"].0, JsonInner::Bool(true));
        assert_eq!(obj["faux"].0, JsonInner::Bool(false));
        assert_eq!(obj.len(), 2);
        assert!(!obj.is_empty());

        let val = Json::from_str("null").unwrap();
        assert_eq!(val.len(), 0);
        assert!(val.is_empty());

        let mt_arr = Json::from_str("[]").unwrap();
        assert!(mt_arr.is_empty());
        let mt_obj = Json::from_str("{}").unwrap();
        assert!(mt_obj.is_empty());
    }

    #[test]
    fn from() {
        assert_eq!(Json::from_str("123").unwrap(), From::from(123u8));
        assert_eq!(Json::from_str("123").unwrap(), From::from(123u16));
        assert_eq!(Json::from_str("123").unwrap(), From::from(123u32));
        assert_eq!(Json::from_str("123").unwrap(), From::from(123u64));
        assert_eq!(Json::from_str("-123").unwrap(), From::from(-123i8));
        assert_eq!(Json::from_str("-123").unwrap(), From::from(-123i16));
        assert_eq!(Json::from_str("-123").unwrap(), From::from(-123i32));
        assert_eq!(Json::from_str("-123").unwrap(), From::from(-123i64));
        assert_eq!(Json::from_str("-103.375").unwrap(), From::from(-103.375f32));
        assert_eq!(Json::from_str("-103.375").unwrap(), From::from(-103.375f64));
        assert_eq!(Json::from_str("true").unwrap(), From::from(true));
        assert_eq!(Json::from_str("false").unwrap(), From::from(false));
        assert_eq!(Json::from_str("null").unwrap(), From::from(()));

        assert_eq!(Json::from_str("[]").unwrap(), From::from(Json::from_str("[]").unwrap()));
    }

    #[test]
    fn format() {
        macro_rules! format_roundtrip (
            ($s:expr) => (
                assert_eq!(format!("{}", Json::from_str($s).unwrap()), $s);
            )
        );
        format_roundtrip!("null");
        format_roundtrip!("true");
        format_roundtrip!("false");
        format_roundtrip!("[ ]");
        format_roundtrip!("{ }");
        format_roundtrip!("[ true, false, true, true ]");
        format_roundtrip!("0");
        format_roundtrip!("1000");
        format_roundtrip!("\"Andrew\"");
        format_roundtrip!("{ \"Andrew\": 10, \"Jonas\": 100 }");
    }
}

