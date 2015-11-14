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

extern crate encoding;
extern crate serde;

use std::collections::HashMap;
use std::{io, ops};

pub mod parser;
pub mod serializer;

/// This is roughly a "*mut str"
#[allow(raw_pointer_derive)]
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
struct UnsafeStringRef {
    ptr: *mut u8,
    len: usize,
    cap: usize
}

impl UnsafeStringRef {
    /// Extract the raw pointer data from a String. This object will be invalidated
    /// if the String is ever reallocated or dropped!
    unsafe fn from_string(s: &mut String) -> UnsafeStringRef {
        let v = s.as_mut_vec();
        UnsafeStringRef {
            ptr: v.as_mut_ptr(),
            len: v.len(),
            cap: v.capacity()
        }
    }

    /// Obtain a &str from a UnsafeStringRef. Note that as lifetimes are not tracked,
    /// the compiler will not prevent this pointing to invalidated memory!
    unsafe fn borrow(&self) -> &str {
        use std::mem;
        let owned_s = String::from_raw_parts(self.ptr, self.len, self.cap);
        let forever_ref = mem::transmute::<_, &'static str>(&owned_s[..]);
        mem::forget(owned_s);
        forever_ref
    }
}

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
    /// A map of Strings to Json objects
    Object(HashMap<String, Json>, Vec<UnsafeStringRef>)
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
        Json::from_iter(s.bytes().map(|u| Ok(u)))
    }

    /// Construct a Json object from a reader
    pub fn from_reader<R: io::Read>(r: R) -> Result<Json, parser::Error> {
        Json::from_iter(r.bytes())
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
    pub fn object(&self) -> Option<&HashMap<String, Json>> { if let JsonInner::Object(ref x, _) = self.0 { Some(&x) } else { None } }

    /// Return the number of subobjects this object represents
    /// (so a count for Arrays and Objects). NOT a string length.
    pub fn len(&self) -> usize {
        match self.0 {
            JsonInner::Null => 0,
            JsonInner::Bool(_) => 1,
            JsonInner::String(_) => 1,
            JsonInner::Number(_) => 1,
            JsonInner::Array(ref v) => v.len(),
            JsonInner::Object(ref m, _) => m.len()
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
            JsonInner::Object(ref m, _) => m.is_empty()
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
}

impl<'a> ops::Index<&'a str> for Json {
    type Output = Json;
    #[inline]
    fn index(&self, index: &'a str) -> &Json {
        if let JsonInner::Object(ref m, _) = self.0 {
            &m[index]
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
}

