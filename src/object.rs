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

//! # Serde de/serialization support to/from Json objects
//!

use std::{fmt, vec};

use serde;
use serde::{de, ser};
use super::Error;
use sentinel::{self, IsSentinel};
use {Json, JsonInner};

impl<'de> de::Deserialize<'de> for Json {
    fn deserialize<D: de::Deserializer<'de>>(d: D) -> Result<Json, D::Error> {
        struct BadVisitor;
        struct GoodVisitor;

        impl<'v> de::Visitor<'v> for BadVisitor {
            type Value = Json;

            fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.write_str(sentinel::SENTINEL_STR)
            }

            fn visit_bytes<E: de::Error>(self, buf: &[u8]) -> Result<Json, E> {
                unsafe {
                    let ptr = buf.as_ptr() as *const Json;
                    Ok((*ptr).clone())
                }
            }
        }

        impl<'v> de::Visitor<'v> for GoodVisitor {
            type Value = Json;

            fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.write_str("a JSON object")
            }

            // copied list of functions to implement from serde_json's `Value`
            // Notably missing are visit_bytes_* which do not make sense for JSON.
            fn visit_bool<E: de::Error>(self, val: bool) -> Result<Json, E> {
                Ok(Json(JsonInner::Bool(val)))
            }

            fn visit_i64<E: de::Error>(self, val: i64) -> Result<Json, E> {
                Ok(Json(JsonInner::Number(format!("{}", val))))
            }

            fn visit_u64<E: de::Error>(self, val: u64) -> Result<Json, E> {
                Ok(Json(JsonInner::Number(format!("{}", val))))
            }

            fn visit_f64<E: de::Error>(self, val: f64) -> Result<Json, E> {
                Ok(Json(JsonInner::Number(format!("{}", val))))
            }

            fn visit_str<E: de::Error>(self, val: &str) -> Result<Json, E> {
                Ok(Json(JsonInner::String(val.to_owned())))
            }

            fn visit_string<E: de::Error>(self, val: String) -> Result<Json, E> {
                Ok(Json(JsonInner::String(val)))
            }

            fn visit_none<E: de::Error>(self) -> Result<Json, E> {
                Ok(Json(JsonInner::Null))
            }

            fn visit_some<D: de::Deserializer<'v>>(self, d: D) -> Result<Json, D::Error> {
                de::Deserialize::deserialize(d)
            }

            fn visit_unit<E: >(self) -> Result<Json, E> {
                Ok(Json(JsonInner::Null))
            }

            fn visit_seq<V: de::SeqAccess<'v>>(self, mut v: V) -> Result<Json, V::Error> {
                let mut arr = vec![];
                while let Some(elem) = v.next_element()? {
                    arr.push(elem);
                }
                Ok(Json(JsonInner::Array(arr)))
            }

            fn visit_map<V: de::MapAccess<'v>>(self, mut v: V) -> Result<Json, V::Error> {
                let mut ret = vec![];
                while let Some(keyval) = v.next_entry()? {
                    ret.push(keyval);
                }
                Ok(Json(JsonInner::Object(ret)))
            }
        }

        if sentinel::detect_our_error_type_de::<D::Error>() {
            d.deserialize_any(BadVisitor)
        } else {
            d.deserialize_any(GoodVisitor)
        }
    }
}

impl ser::Serialize for Json {
    fn serialize<S: ser::Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        // If we are dealing with our own Serializer, pass through a pointer to ourself
        // so that it can simply clone us. The motivation for this hack is as above in
        // the `Deserialize`; the mechanism is slightly different.
        if sentinel::detect_our_error_type_ser::<S::Error>() {
            return s.serialize_newtype_struct(sentinel::SENTINEL_STR, self);
        }

        // Otherwise just serialize honestly. This will serialize numbers as strings
        match self.0 {
            JsonInner::Null => s.serialize_unit(),
            JsonInner::Bool(b) => s.serialize_bool(b),
            JsonInner::Number(ref st) => s.serialize_str(st),
            JsonInner::String(ref st) => s.serialize_str(st),
            JsonInner::Array(ref arr) => ser::Serialize::serialize(arr, s),
            JsonInner::Object(ref arr) => {
                use ser::SerializeMap;
                let mut map = s.serialize_map(Some(arr.len()))?;
                for (ref k, ref v) in arr {
                    map.serialize_key(k)?;
                    map.serialize_value(v)?;
                }
                map.end()
            }
        }
    }
}

/// A "Json to whatever" deserializer
pub struct Deserializer(Json);

impl Deserializer {
    /// Creates a new deserializer from a Json value
    pub fn new<T>(val: T) -> Deserializer
        where Json: From<T>
    {
        Deserializer(From::from(val))
    }
}

macro_rules! deserialize_num (
    ($fn_name:ident, $visit_name:ident, $ty:ident) => {
        fn $fn_name<V: de::Visitor<'de>>(self, v: V) -> Result<V::Value, Error> {
            let Json(val) = self.0;
            match val {
                JsonInner::Number(s) => {
                    use std::str::FromStr;
                    if let Ok(num) = $ty::from_str(&s) {
                        v.$visit_name(num)
                    } else {
                        Err(de::Error::invalid_type(de::Unexpected::Other("could not parse number"), &v))
                    }
                },
                JsonInner::Bool(val) => Err(de::Error::invalid_type(de::Unexpected::Bool(val), &v)),
                JsonInner::Null => Err(de::Error::invalid_type(de::Unexpected::Unit, &v)),
                JsonInner::String(s) => Err(de::Error::invalid_type(de::Unexpected::Str(&s), &v)),
                JsonInner::Array(_) => Err(de::Error::invalid_type(de::Unexpected::Seq, &v)),
                JsonInner::Object(_) => Err(de::Error::invalid_type(de::Unexpected::Seq, &v))
            }
        }
    }
);

impl<'de> de::Deserializer<'de> for Deserializer {
    type Error = Error;

    fn deserialize_any<V: de::Visitor<'de>>(self, v: V) -> Result<V::Value, Error> {
        // Check whether we are deserializing a Json type; if so, smuggle a pointer
        // to the Deserializer's internal state through the `expecting` method
        struct Dummy<'a, B: 'a>(&'a B);
        impl<'a, 'b, B: de::Visitor<'b> + 'a> fmt::Display for Dummy<'a, B> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                self.0.expecting(f)
            }
        }

        // Pass-through object, smuggling a self-pointer through `visit_bytes`, if
        // we are deserializing one of our own Json objects
        if Dummy(&v).is_sentinel() {
            unsafe {
                use std::slice;
                let internal_ptr = &self.0 as *const _ as *const u8;
                return v.visit_bytes(slice::from_raw_parts(internal_ptr, 0));
            }
        }

        // Otherwise deserialize normally
        let Json(current) = self.0;
        match current {
            JsonInner::Null => v.visit_unit(),
            JsonInner::Bool(b) => v.visit_bool(b),
            JsonInner::Number(s) => v.visit_string(s),
            JsonInner::String(s) => v.visit_string(s),
            JsonInner::Array(arr) => {
                let arr_len = arr.len();
                let mut sd = SeqDeserializer {
                    iter: arr.into_iter(),
                    next_map_val: None,
                };
                let seq = v.visit_seq(&mut sd)?;
                if sd.iter.len() == 0 {
                    Ok(seq)
                } else {
                    Err(de::Error::invalid_length(arr_len, &"array too large"))
                }
            }
            JsonInner::Object(map) => {
                let map_len = map.len();
                let mut sd = SeqDeserializer {
                    iter: map.into_iter(),
                    next_map_val: None,
                };
                let map = v.visit_map(&mut sd)?;
                if sd.iter.len() == 0 {
                    Ok(map)
                } else {
                    Err(de::Error::invalid_length(map_len, &"map too large"))
                }
            }
        }
    }

    fn deserialize_bool<V: de::Visitor<'de>>(self, v: V) -> Result<V::Value, Error> {
        let Json(val) = self.0;
        match val {
            JsonInner::Bool(val) => v.visit_bool(val),
            JsonInner::Null => Err(de::Error::invalid_type(de::Unexpected::Unit, &v)),
            JsonInner::Number(s) => Err(de::Error::invalid_type(de::Unexpected::Str(&s), &v)),
            JsonInner::String(s) => Err(de::Error::invalid_type(de::Unexpected::Str(&s), &v)),
            JsonInner::Array(_) => Err(de::Error::invalid_type(de::Unexpected::Seq, &v)),
            JsonInner::Object(_) => Err(de::Error::invalid_type(de::Unexpected::Seq, &v)),
        }
    }

    // Given a number hint, parse out a number
    deserialize_num!(deserialize_i8, visit_i8, i8);
    deserialize_num!(deserialize_i16, visit_i16, i16);
    deserialize_num!(deserialize_i32, visit_i32, i32);
    deserialize_num!(deserialize_i64, visit_i64, i64);
    deserialize_num!(deserialize_u8, visit_u8, u8);
    deserialize_num!(deserialize_u16, visit_u16, u16);
    deserialize_num!(deserialize_u32, visit_u32, u32);
    deserialize_num!(deserialize_u64, visit_u64, u64);
    deserialize_num!(deserialize_f32, visit_f32, f32);
    deserialize_num!(deserialize_f64, visit_f64, f64);

    // Special-case Option to allow absenteeism
    fn deserialize_option<V: de::Visitor<'de>>(self, v: V) -> Result<V::Value, Error> {
       match self.0 {
           Json(JsonInner::Null) => v.visit_none(),
           _ => v.visit_some(self),
       }
    }

    // Everything else should just fall back to deserialize_any
    fn deserialize_char<V: de::Visitor<'de>>(self, v: V) -> Result<V::Value, Error> {
        self.deserialize_any(v)
    }

    fn deserialize_str<V: de::Visitor<'de>>(self, v: V) -> Result<V::Value, Error> {
        self.deserialize_any(v)
    }

    fn deserialize_string<V: de::Visitor<'de>>(self, v: V) -> Result<V::Value, Error> {
        self.deserialize_any(v)
    }

    fn deserialize_bytes<V: de::Visitor<'de>>(self, v: V) -> Result<V::Value, Error> {
        self.deserialize_any(v)
    }

    fn deserialize_byte_buf<V: de::Visitor<'de>>(self, v: V) -> Result<V::Value, Error> {
        self.deserialize_any(v)
    }

    fn deserialize_unit<V: de::Visitor<'de>>(self, v: V) -> Result<V::Value, Error> {
        self.deserialize_any(v)
    }

    fn deserialize_unit_struct<V: de::Visitor<'de>>(self, _: &'static str, v: V) -> Result<V::Value, Error> {
        self.deserialize_any(v)
    }

    fn deserialize_newtype_struct<V: de::Visitor<'de>>(self, _: &'static str, v: V) -> Result<V::Value, Error> {
        v.visit_newtype_struct(self)
    }

    fn deserialize_seq<V: de::Visitor<'de>>(self, v: V) -> Result<V::Value, Error> {
        self.deserialize_any(v)
    }

    fn deserialize_map<V: de::Visitor<'de>>(self, v: V) -> Result<V::Value, Error> {
        self.deserialize_any(v)
    }

    fn deserialize_tuple<V: de::Visitor<'de>>(self, _: usize, v: V) -> Result<V::Value, Error> {
        self.deserialize_any(v)
    }

    fn deserialize_tuple_struct<V: de::Visitor<'de>>(self, _: &'static str, _: usize, v: V) -> Result<V::Value, Error> {
        self.deserialize_any(v)
    }

    fn deserialize_struct<V: de::Visitor<'de>>(self, _: &'static str, _: &'static [&'static str], v: V) -> Result<V::Value, Error> {
        self.deserialize_any(v)
    }

    fn deserialize_enum<V: de::Visitor<'de>>(self, _: &'static str, _: &'static [&'static str], v: V) -> Result<V::Value, Error> {
        self.deserialize_any(v)
    }

    fn deserialize_identifier<V: de::Visitor<'de>>(self, v: V) -> Result<V::Value, Error> {
        self.deserialize_any(v)
    }

    fn deserialize_ignored_any<V: de::Visitor<'de>>(self, v: V) -> Result<V::Value, Error> {
        self.deserialize_any(v)
    }

    fn is_human_readable(&self) -> bool { true }
}

struct SeqDeserializer<T> {
    iter: vec::IntoIter<T>,
    next_map_val: Option<Json>,
}

impl<'de> de::SeqAccess<'de> for SeqDeserializer<Json> {
    type Error = Error;

    fn next_element_seed<T: de::DeserializeSeed<'de>>(&mut self, seed: T) -> Result<Option<T::Value>, Error> {
        match self.iter.next() {
            Some(val) => seed.deserialize(Deserializer(val)).map(Some),
            None => Ok(None)
        }
    }

    fn size_hint(&self) -> Option<usize> {
        let (_, x) = self.iter.size_hint();
        x
    }
}

impl<'de> de::MapAccess<'de> for SeqDeserializer<(String, Json)> {
    type Error = Error;

    fn next_key_seed<T: de::DeserializeSeed<'de>>(&mut self, seed: T) -> Result<Option<T::Value>, Error> {
        match self.iter.next() {
            Some((key, val)) => {
                self.next_map_val = Some(val);
                seed.deserialize(Deserializer(Json(JsonInner::String(key)))).map(Some)
            }
            None => Ok(None)
        }
    }

    fn next_value_seed<T: de::DeserializeSeed<'de>>(&mut self, seed: T) -> Result<T::Value, Error> {
        use de::Error;
        match self.next_map_val.take() {
            Some(val) => {
                seed.deserialize(Deserializer(val))
            }
            None => Err(Error::custom("tried to decode multiple keys in a row without decoding a value"))
        }
    }

    fn size_hint(&self) -> Option<usize> {
        let (_, x) = self.iter.size_hint();
        x
    }
}

/// A "whatever to Json" serializer
#[derive(Default)]
pub struct Serializer;

impl Serializer {
    /// Creates a new serializer
    pub fn new() -> Serializer {
        Serializer
    }
}

macro_rules! serialize_num (
    ($fn_name:ident, $ty:ty) => {
        fn $fn_name(self, val: $ty) -> Result<Json, Error> {
            Ok(Json(JsonInner::Number(format!("{}", val))))
        }
    }
);

impl serde::Serializer for Serializer {
    type Ok = Json;
    type Error = Error;
    type SerializeSeq = SerializeSeq<Json>;
    type SerializeTuple = SerializeSeq<Json>;
    type SerializeTupleStruct = SerializeSeq<Json>;
    type SerializeTupleVariant = SerializeTupleVariant<Json>;
    type SerializeMap = SerializeSeq<(String, Json)>;
    type SerializeStruct = SerializeSeq<(String, Json)>;
    type SerializeStructVariant = SerializeTupleVariant<(String, Json)>;

    fn serialize_bool(self, val: bool) -> Result<Json, Error> {
        Ok(Json(JsonInner::Bool(val)))
    }

    serialize_num!(serialize_i8, i8);
    serialize_num!(serialize_i16, i16);
    serialize_num!(serialize_i32, i32);
    serialize_num!(serialize_i64, i64);
    serialize_num!(serialize_u8, u8);
    serialize_num!(serialize_u16, u16);
    serialize_num!(serialize_u32, u32);
    serialize_num!(serialize_u64, u64);
    serialize_num!(serialize_f32, f32);
    serialize_num!(serialize_f64, f64);

    fn serialize_char(self, val: char) -> Result<Json, Error> {
        let mut s = String::new();
        s.push(val);
        Ok(Json(JsonInner::String(s)))
    }

    fn serialize_str(self, val: &str) -> Result<Json, Error> {
        Ok(Json(JsonInner::String(val.to_owned())))
    }

    fn serialize_bytes(self, val: &[u8]) -> Result<Json, Error> {
        use std::fmt::Write;

        let mut s = String::new();
        for ch in val {
            write!(s, "{:02x}", *ch).expect("failed to write hex to string");
        }
        Ok(Json(JsonInner::String(s)))
    }

    fn serialize_unit(self) -> Result<Json, Error> {
        Ok(Json(JsonInner::Null))
    }

    fn serialize_unit_struct(self, _: &'static str) -> Result<Json, Error> {
        Ok(Json(JsonInner::Null))
    }

    fn serialize_unit_variant(self, _: &'static str, _: u32, variant: &'static str) -> Result<Json, Error> {
        Ok(Json(JsonInner::String(variant.to_owned())))
    }

    fn serialize_newtype_struct<T: ser::Serialize + ?Sized>(self, name: &'static str, value: &T) -> Result<Json, Error> {
        if name.as_ptr() == sentinel::SENTINEL_STR.as_ptr() {
            unsafe {
                let json = value as *const _ as *const Json;
                Ok((*json).clone())
            }
        } else {
            value.serialize(self)
        }
    }

    fn serialize_newtype_variant<T: ser::Serialize + ?Sized>(self, _: &'static str, _: u32, variant: &'static str, value: &T) -> Result<Json, Error> {
        let val = value.serialize(self)?;
        Ok(Json(JsonInner::Object(vec![(variant.to_owned(), val)])))
    }

    fn serialize_none(self) -> Result<Json, Error> {
        Ok(Json(JsonInner::Null))
    }

    fn serialize_some<T: ser::Serialize + ?Sized>(self, value: &T) -> Result<Json, Error> {
        value.serialize(self)
    }

    fn serialize_seq(self, len: Option<usize>) -> Result<Self::SerializeSeq, Error> {
        Ok(SerializeSeq {
            buf: Vec::with_capacity(len.unwrap_or(0)),
            next_key: None,
        })
    }

    fn serialize_tuple(self, len: usize) -> Result<Self::SerializeTuple, Error> {
        Ok(SerializeSeq {
            buf: Vec::with_capacity(len),
            next_key: None,
        })
    }

    fn serialize_tuple_struct(self, _: &'static str, len: usize) -> Result<Self::SerializeTupleStruct, Error> {
        Ok(SerializeSeq {
            buf: Vec::with_capacity(len),
            next_key: None,
        })
    }

    fn serialize_tuple_variant(self, _: &'static str, _: u32, variant: &'static str, len: usize) -> Result<Self::SerializeTupleVariant, Error> {
        Ok(SerializeTupleVariant {
            name: variant.to_owned(),
            buf: Vec::with_capacity(len),
        })
    }
    
    fn serialize_map(self, len: Option<usize>) -> Result<Self::SerializeMap, Error> {
        Ok(SerializeSeq {
            buf: Vec::with_capacity(len.unwrap_or(0)),
            next_key: None,
        })
    }

    fn serialize_struct(self, _: &'static str, len: usize) -> Result<Self::SerializeStruct, Error> {
        Ok(SerializeSeq {
            buf: Vec::with_capacity(len),
            next_key: None,
        })
    }

    fn serialize_struct_variant(self, _: &'static str, _: u32, variant: &'static str, len: usize) -> Result<Self::SerializeStructVariant, Error> {
        Ok(SerializeTupleVariant {
            name: variant.to_owned(),
            buf: Vec::with_capacity(len),
        })
    }

    fn is_human_readable(&self) -> bool { true }
}

#[allow(missing_docs)]
pub struct SerializeSeq<T> {
    buf: Vec<T>,
    next_key: Option<String>,
}

impl ser::SerializeSeq for SerializeSeq<Json> {
    type Ok = Json;
    type Error = Error;

    fn serialize_element<T: ser::Serialize + ?Sized>(&mut self, value: &T) -> Result<(), Error> {
        self.buf.push(Json::from_serialize(value)?);
        Ok(())
    }

    fn end(self) -> Result<Json, Error> {
        Ok(Json(JsonInner::Array(self.buf)))
    }
}

impl ser::SerializeTuple for SerializeSeq<Json> {
    type Ok = Json;
    type Error = Error;

    fn serialize_element<T: ser::Serialize + ?Sized>(&mut self, value: &T) -> Result<(), Error> {
        self.buf.push(Json::from_serialize(value)?);
        Ok(())
    }

    fn end(self) -> Result<Json, Error> {
        Ok(Json(JsonInner::Array(self.buf)))
    }
}

impl ser::SerializeMap for SerializeSeq<(String, Json)> {
    type Ok = Json;
    type Error = Error;

    fn serialize_key<T: ser::Serialize + ?Sized>(&mut self, key: &T) -> Result<(), Error> {
        if let Json(JsonInner::String(s)) = Json::from_serialize(key)? {
            self.next_key = Some(s);
            Ok(())
        } else {
            use de::Error;
            Err(Error::custom("can only serialize maps with stringy keys"))
        }
    }

    fn serialize_value<T: ser::Serialize + ?Sized>(&mut self, val: &T) -> Result<(), Error> {
        let key = self.next_key.take().expect("serialize_value called before serialize_key");
        let value = Json::from_serialize(val)?;
        self.buf.push((key, value));
        Ok(())
    }

    fn end(self) -> Result<Json, Error> {
        Ok(Json(JsonInner::Object(self.buf)))
    }
}

impl ser::SerializeTupleStruct for SerializeSeq<Json> {
    type Ok = Json;
    type Error = Error;

    fn serialize_field<T: ser::Serialize + ?Sized>(&mut self, value: &T) -> Result<(), Error> {
        self.buf.push(Json::from_serialize(value)?);
        Ok(())
    }

    fn end(self) -> Result<Json, Error> {
        Ok(Json(JsonInner::Array(self.buf)))
    }
}

impl ser::SerializeStruct for SerializeSeq<(String, Json)> {
    type Ok = Json;
    type Error = Error;

    fn serialize_field<T: ser::Serialize + ?Sized>(&mut self, key: &'static str, val: &T) -> Result<(), Error> {
        let value = Json::from_serialize(val)?;
        self.buf.push((key.to_owned(), value));
        Ok(())
    }

    fn end(self) -> Result<Json, Error> {
        Ok(Json(JsonInner::Object(self.buf)))
    }
}


#[allow(missing_docs)]
pub struct SerializeTupleVariant<T> {
    name: String,
    buf: Vec<T>
}

impl ser::SerializeTupleVariant for SerializeTupleVariant<Json> {
    type Ok = Json;
    type Error = Error;

    fn serialize_field<T: ser::Serialize + ?Sized>(&mut self, value: &T) -> Result<(), Error> {
        self.buf.push(Json::from_serialize(value)?);
        Ok(())
    }

    fn end(self) -> Result<Json, Error> {
        Ok(Json(JsonInner::Object(
            vec![
                (self.name, Json(JsonInner::Array(self.buf)))
            ]
        )))
    }
}

impl ser::SerializeStructVariant for SerializeTupleVariant<(String, Json)> {
    type Ok = Json;
    type Error = Error;

    fn serialize_field<T: ser::Serialize + ?Sized>(&mut self, key: &'static str, value: &T) -> Result<(), Error> {
        self.buf.push((key.to_owned(), Json::from_serialize(value)?));
        Ok(())
    }

    fn end(self) -> Result<Json, Error> {
        Ok(Json(JsonInner::Object(
            vec![
                (self.name, Json(JsonInner::Object(self.buf)))
            ]
        )))
    }
}

#[cfg(test)]
mod tests {
    use serde::de;
    use std::collections::HashMap;
    use {Json, JsonInner};

    use super::{Serializer, Deserializer};

    macro_rules! roundtrip_success(
        ($t:ty, $e:expr) => ({
            let obj: $t = $e;
            match Json::from_serialize(&obj) {
                Ok(val) => {
                    use serde::ser::Serialize;
                    // "Deserialize" as Json
                    let alt_json: Result<Json, _> = val.clone().into_deserialize();
                    assert!(alt_json.is_ok());
                    assert_eq!(alt_json.unwrap(), val);
                    // "Serialize" as Json
                    let serval = val.serialize(Serializer::new()).unwrap();
                    assert_eq!(serval, val);
                    // Deserialize as object
                    let res: Result<$t, _> = val.into_deserialize();
                    assert!(res.is_ok());
                    assert_eq!(res.unwrap(), obj);
                }
                Err(e) => { panic!("Serializing into Json failed: {:?}", e); }
            }
        })
    );

    #[test]
    fn serde_roundtrip() {
        roundtrip_success!((), ());
        roundtrip_success!(String, "".to_owned());
        roundtrip_success!(String, "Thing".to_owned());
        roundtrip_success!(bool, false);
        roundtrip_success!(bool, true);
        roundtrip_success!(f64, 1.125);
        roundtrip_success!(f32, 1.125);

        macro_rules! check_num(
           ($t:ident) => ({
               use std::$t;
               roundtrip_success!($t, 0);
               roundtrip_success!($t, 100);
               roundtrip_success!($t, $t::MIN);
               roundtrip_success!($t, $t::MAX);
           })
        );
        check_num!(usize);
        check_num!(isize);
        check_num!(u64);
        check_num!(i64);
        check_num!(u32);
        check_num!(i32);
        check_num!(u16);
        check_num!(i16);
        check_num!(u8);
        check_num!(i8);

        roundtrip_success!(Vec<bool>, vec![]);
        roundtrip_success!(Vec<bool>, vec![true, false, true, true]);
        roundtrip_success!(Vec<String>, vec!["b".to_owned(), "i".to_owned(), "t".to_owned()]);

        roundtrip_success!(Vec<(String, bool)>, vec![("b".to_owned(), true), ("i".to_owned(), false)]);

        let mut map = HashMap::new();
        map.insert("Test".to_owned(), "Testval".to_owned());
        map.insert("Test2".to_owned(), "another".to_owned());
        roundtrip_success!(HashMap<String, String>, map);
    }

    macro_rules! deserialize_test(
        ($e:expr, $result:expr) => ({
            let d = Deserializer::new($e);
            let json: Result<Json, _> = de::Deserialize::deserialize(d);
            assert!(json.is_ok());
            let json_str = json.unwrap().to_string();
            assert_eq!(json_str, $result);
        })
    );

    #[test]
    fn serde_deserialize() {
        macro_rules! check_num(
           ($t:ident) => ({
               use std::$t;
               let mut val: $t;

               val = 0;
               deserialize_test!(val, format!("{}", val));
               val = 100;
               deserialize_test!(val, format!("{}", val));
               val = $t::MIN;
               deserialize_test!(val, format!("{}", val));
               val = $t::MAX;
               deserialize_test!(val, format!("{}", val));
           })
        );

        check_num!(u8);
        check_num!(u16);
        check_num!(u32);
        check_num!(u64);
        check_num!(usize);
        check_num!(i8);
        check_num!(i16);
        check_num!(i32);
        check_num!(i64);
        check_num!(isize);

        deserialize_test!(0.375f32, "0.375");
        deserialize_test!(0.375f64, "0.375");
        deserialize_test!("Test1".to_string(), "\"Test1\"");
        deserialize_test!((), "null");
        deserialize_test!(true, "true");
        deserialize_test!(false, "false");

        deserialize_test!(
            Json(JsonInner::Array(vec![
                Json::from(true),
                Json::from(false),
                Json::from(true),
                Json::from(true)
            ])),
            "[true, false, true, true]"
        );

        // TODO the ordering that HashMap gives us is unpredictable so we can't directly
        // test multiple values
        let mut map = HashMap::new();
        map.insert("Test".to_owned(), "Testval".to_owned());
        let json = Json::from_serialize(map).unwrap();
        deserialize_test!(json, "{\"Test\": \"Testval\"}");
    }

    macro_rules! serialize_test(
        ($s:expr) => ({
            use serde_json;
            assert_eq!(serde_json::to_string(&Json::from_str($s).unwrap()).unwrap(), $s);
        })
    );

    #[test]
    fn serde_serialize() {
        serialize_test!("null");
        serialize_test!("true");
        serialize_test!("false");
        serialize_test!("\"test string\"");
        serialize_test!("[true,false,false,\"thing\"]");
        serialize_test!("{\"obj\":\"val\",\"obj2\":\"val2\"}");
    }
}

