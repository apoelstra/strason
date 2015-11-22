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

use std::vec;

use serde::{de, ser};
use parser::{Error, ErrorType};
use {Json, JsonInner};

static EVIL_SENTINEL: &'static str = "$$$STRASON$$$EVIL$$$MODE$$$";

impl de::Deserialize for Json {
    fn deserialize<D: de::Deserializer>(d: &mut D) -> Result<Json, D::Error> {
        if <D as de::Deserializer>::format() == "strason" {
            // This `EvilVisitor` type is a nasty hack to get a reference
            // to the deserializer's inner state, which will be a Json
            // object if the deserializer is our Deserializer. It has to
            // do this via the de::Visitor trait, which lets us return
            // Json values no problem but not input arbitrary data (just
            // Rust primitive types, none of which are pointers). So we
            // signal the deserializer using the string input of
            // `Deserializer::visit_unit_struct` to say "hey, it's us, Json"
            // and it'll respond by passing a reference to its inner state
            // hidden inside a usize.
            struct EvilVisitor;
            impl de::Visitor for EvilVisitor {
                type Value = Json;
                fn visit_usize<E: de::Error>(&mut self, v: usize) -> Result<Json, E> {
                   unsafe {
                       let ptr = v as *mut Option<Json>;
                       Ok((*ptr).take().unwrap())
                   }
                }
            }

            d.visit_unit_struct(EVIL_SENTINEL, EvilVisitor)
        // After that madness, actual deserialization code follows
        } else {
            struct GoodVisitor;
            impl de::Visitor for GoodVisitor {
                type Value = Json;
                fn visit_bool<E>(&mut self, val: bool) -> Result<Json, E> {
                    Ok(Json(JsonInner::Bool(val)))
                }

                fn visit_i64<E>(&mut self, val: i64) -> Result<Json, E> {
                    Ok(Json(JsonInner::Number(format!("{}", val))))
                }

                fn visit_u64<E>(&mut self, val: u64) -> Result<Json, E> {
                    Ok(Json(JsonInner::Number(format!("{}", val))))
                }

                fn visit_f64<E>(&mut self, val: f64) -> Result<Json, E> {
                    Ok(Json(JsonInner::Number(format!("{}", val))))
                }

                fn visit_str<E>(&mut self, val: &str) -> Result<Json, E> {
                    Ok(Json(JsonInner::String(val.to_owned())))
                }

                fn visit_string<E>(&mut self, val: String) -> Result<Json, E> {
                    Ok(Json(JsonInner::String(val)))
                }

                fn visit_unit<E>(&mut self) -> Result<Json, E> {
                    Ok(Json(JsonInner::Null))
                }

                fn visit_none<E>(&mut self) -> Result<Json, E> {
                    Ok(Json(JsonInner::Null))
                }

                fn visit_some<D: de::Deserializer>(&mut self, d: &mut D) -> Result<Json, D::Error> {
                    de::Deserialize::deserialize(d)
                }

                fn visit_seq<V: de::SeqVisitor>(&mut self, v: V) -> Result<Json, V::Error> {
                    let arr = try!(de::impls::VecVisitor::new().visit_seq(v));
                    Ok(Json(JsonInner::Array(arr)))
                }

                fn visit_map<V: de::MapVisitor>(&mut self, mut v: V) -> Result<Json, V::Error> {
                    let mut ret = vec![];
                    while let Some(keyval) = try!(v.visit()) {
                        ret.push(keyval);
                    }
                    try!(v.end());
                    Ok(Json(JsonInner::Object(ret)))
                }
            }

            d.visit(GoodVisitor)
        }
    }
}


/// A "Json to whatever" deserializer
pub struct Deserializer {
    current: Option<Json>
}

impl Deserializer {
    /// Creates a new deserializer from a Json value
    pub fn new(val: Json) -> Deserializer {
        Deserializer { current: Some(val) }
    }
}

impl de::Deserializer for Deserializer {
    type Error = Error;

    fn visit<V: de::Visitor>(&mut self, mut v: V) -> Result<V::Value, Error> {
        // Extract current value so we can manipulate it without borrowing self
        let current = match self.current.take() {
            Some(val) => val,
            None => { return Err(de::Error::end_of_stream()); }
        };
        // Unwrap it from the outer type
        let Json(current) = current;

        match current {
            JsonInner::Null => v.visit_unit(),
            JsonInner::Bool(b) => v.visit_bool(b),
            JsonInner::Number(s) => v.visit_string(s),
            JsonInner::String(s) => v.visit_string(s),
            JsonInner::Array(arr) => {
                v.visit_seq(SeqVisitor {
                    iter: arr.into_iter()
                })
            }
            JsonInner::Object(map) => {
                v.visit_map(MapVisitor {
                    iter: map.into_iter(),
                    next_val: None
                })
            }
        }
    }

    // Special-case Option to allow absenteeism
    fn visit_option<V: de::Visitor>(&mut self,  mut v: V) -> Result<V::Value, Error> {
       match self.current {
           Some(Json(JsonInner::Null)) => v.visit_none(),
           Some(_) => v.visit_some(self),
           None => { return Err(de::Error::end_of_stream()); }
       }
    }

    fn format() -> &'static str { "strason" }

    // Special-case for evil visitor: if we have a specially-constructed Visitor,
    // which will signal its presence by calling this function with a specific
    // struct name, return our inner state to it disguised as a usize. The
    // visitor will interpret the usize as a pointer and use it to clone our
    // state, allowing us to "deserialize a Json as a Json" without actually
    // doing any deserialization (which'd destroy numeric values)
    fn visit_unit_struct<V: de::Visitor>(&mut self, name: &'static str, mut v: V) -> Result<V::Value, Error> {
        if name == EVIL_SENTINEL {
            match self.current {
                Some(_) => { v.visit_usize(&mut self.current as *mut _ as usize) }
                None => { return Err(de::Error::end_of_stream()); }
            }
        } else {
            self.visit(v)
        }
    }
}

struct SeqVisitor {
    iter: vec::IntoIter<Json>
}

impl de::SeqVisitor for SeqVisitor {
    type Error = Error;

    fn visit<T: de::Deserialize>(&mut self) -> Result<Option<T>, Error> {
        match self.iter.next() {
            Some(val) => Ok(Some(try!(de::Deserialize::deserialize(&mut Deserializer::new(val))))),
            None => Ok(None)
        }
    }

    fn end(&mut self) -> Result<(), Error> {
       let (rem, _) = self.iter.size_hint();
       if rem == 0 { Ok(()) } else { Err(de::Error::length_mismatch(rem)) }
    }

    fn size_hint(&self) -> (usize, Option<usize>) { self.iter.size_hint() }
}

struct MapVisitor {
    iter: vec::IntoIter<(String, Json)>,
    next_val: Option<Json>
}

impl de::MapVisitor for MapVisitor {
    type Error = Error;

    fn visit_key<T: de::Deserialize>(&mut self) -> Result<Option<T>, Error> {
        match self.iter.next() {
            Some((key, val)) => {
                self.next_val = Some(val);
                let mut de = Deserializer::new(Json(JsonInner::String(key)));
                Ok(Some(try!(de::Deserialize::deserialize(&mut de))))
            }
            None => Ok(None)
        }
    }

    fn visit_value<T: de::Deserialize>(&mut self) -> Result<T, Error> {
        let val = self.next_val.take().unwrap();
        Ok(try!(de::Deserialize::deserialize(&mut Deserializer::new(val))))
    }

    fn end(&mut self) -> Result<(), Error> {
       let (rem, _) = self.iter.size_hint();
       if rem == 0 { Ok(()) } else { Err(de::Error::length_mismatch(rem)) }
    }

    fn missing_field<T: de::Deserialize>(&mut self, _: &'static str) -> Result<T, Error> {
        // Try an alternate deserializer that parses everything as a unit;
        // so "missing field" and "field: null" will be equivalent
        struct UnitDeserializer;
        impl de::Deserializer for UnitDeserializer {
            type Error = Error;

            // With no hint, deserialize as a unit
            fn visit<V: de::Visitor>(&mut self, mut v: V) -> Result<V::Value, Error> { v.visit_unit() }
            // With an "expect option" hint, deserialize as a None
            fn visit_option<V: de::Visitor>(&mut self, mut v: V) -> Result<V::Value, Error> { v.visit_none() }
        }
        Ok(try!(de::Deserialize::deserialize(&mut UnitDeserializer)))
    }

    fn size_hint(&self) -> (usize, Option<usize>) { self.iter.size_hint() }
}

enum State {
    // terminal value
    Value(Json),
    // building an array,
    Array(Vec<Json>),
    // building an object,
    Object(Vec<(String, Json)>)
}

/// A "whatever to Json" serializer
pub struct Serializer {
    // stack-based state machine stack
    state: Vec<State>
}

impl Serializer {
    /// Creates a new serializer
    pub fn new() -> Serializer {
        Serializer { state: vec![] }
    }

    /// Unwraps the serialized value. Guaranteed to work iff serialize() was called and did not error.
    pub fn unwrap(mut self) -> Json {
        assert_eq!(self.state.len(), 1);
        match self.state.pop() {
            Some(State::Value(val)) => val,
            _ => panic!("Unwrap on a bad Json serializer")
        }
    }
}

impl ser::Serializer for Serializer {
    type Error = Error;

    fn visit_bool(&mut self, val: bool) -> Result<(), Error> {
        self.state.push(State::Value(Json(JsonInner::Bool(val))));
        Ok(())
    }

    fn visit_i64(&mut self, val: i64) -> Result<(), Error> {
        self.state.push(State::Value(Json(JsonInner::Number(format!("{}", val)))));
        Ok(())
    }

    fn visit_u64(&mut self, val: u64) -> Result<(), Error> {
        self.state.push(State::Value(Json(JsonInner::Number(format!("{}", val)))));
        Ok(())
    }

    fn visit_f64(&mut self, val: f64) -> Result<(), Error> {
        self.state.push(State::Value(Json(JsonInner::Number(format!("{}", val)))));
        Ok(())
    }

    fn visit_str(&mut self, val: &str) -> Result<(), Error> {
        self.state.push(State::Value(Json(JsonInner::String(val.to_owned()))));
        Ok(())
    }

    fn visit_unit(&mut self) -> Result<(), Error> {
        self.state.push(State::Value(Json(JsonInner::Null)));
        Ok(())
    }

    fn visit_none(&mut self) -> Result<(), Error> {
        self.state.push(State::Value(Json(JsonInner::Null)));
        Ok(())
    }

    fn visit_some<V: ser::Serialize>(&mut self, val: V) -> Result<(), Error> {
        val.serialize(self)
    }

    fn visit_seq<V: ser::SeqVisitor>(&mut self, mut v: V) -> Result<(), Error> {
        let arr = Vec::with_capacity(v.len().unwrap_or(0));
        // Push the array onto our stack machine
        self.state.push(State::Array(arr));
        // Parse all the values into this array
        while try!(v.visit(self)).is_some() {}
        // Pop the array off
        let arr = if let Some(State::Array(arr)) = self.state.pop() { arr } else { unreachable!() };
        // Return
        self.state.push(State::Value(Json(JsonInner::Array(arr))));
        Ok(())
    }

    fn visit_seq_elt<T: ser::Serialize>(&mut self, value: T) -> Result<(), Error> {
        try!(value.serialize(self));

        let val = if let Some(State::Value(val)) = self.state.pop() { val } else { unreachable!() };
        if let Some(&mut State::Array(ref mut arr)) = self.state.last_mut() {
            arr.push(val);
        } else {
            unreachable!()
        };
        Ok(())
    }

    fn visit_map<V: ser::MapVisitor>(&mut self, mut v: V) -> Result<(), Error> {
        let map = Vec::with_capacity(v.len().unwrap_or(0));
        // Push the array onto our stack machine
        self.state.push(State::Object(map));
        // Parse all the values into this array
        while try!(v.visit(self)).is_some() {}
        // Pop the array off
        let map = if let Some(State::Object(map)) = self.state.pop() { map } else { unreachable!() };
        // Return
        self.state.push(State::Value(Json(JsonInner::Object(map))));
        Ok(())
    }

    fn visit_map_elt<K: ser::Serialize, V: ser::Serialize>(&mut self, key: K, val: V) -> Result<(), Error> {
        // Serialize key
        try!(key.serialize(self));
        let key = match self.state.pop() {
            Some(State::Value(Json(JsonInner::String(s)))) => s,
            Some(State::Value(_)) => { return Err(From::from(ErrorType::ExpectedString)); }
            _ => unreachable!()
        };

        // Serialize value
        try!(val.serialize(self));
        let val = if let Some(State::Value(val)) = self.state.pop() { val } else { unreachable!() };

        // Add (key, value) to map
        if let Some(&mut State::Object(ref mut arr)) = self.state.last_mut() {
            arr.push((key, val));
        } else {
            unreachable!()
        };
        Ok(())
    }
}

/// Convert an arbitrary object to a Json structure
pub fn from_serialize<T: ser::Serialize>(obj: &T) -> Result<Json, Error> {
    let mut s = Serializer::new();
    try!(obj.serialize(&mut s));
    Ok(s.unwrap())
}

/// Convert a Json structure to an arbitary object
pub fn into_deserialize<T: de::Deserialize>(json: Json) -> Result<T, Error> {
    let mut d = Deserializer::new(json);
    de::Deserialize::deserialize(&mut d)
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use Json;

    macro_rules! roundtrip_success(
        ($t:ty, $e:expr) => ({
            let obj = $e;
            match super::from_serialize(&obj) {
                Ok(val) => {
                    // "Deserialize" as Json
                    let alt_json: Result<Json, _> = val.clone().into_deserialize();
                    assert!(alt_json.is_ok());
                    assert_eq!(alt_json.unwrap(), val);
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
        roundtrip_success!(String, "");
        roundtrip_success!(String, "Thing");
        roundtrip_success!(bool, false);
        roundtrip_success!(bool, true);

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
        roundtrip_success!(Vec<String>, vec!["b", "i", "t"]);

        roundtrip_success!(Vec<(String, bool)>, vec![("b".to_owned(), true), ("i".to_owned(), false)]);

        let mut map = HashMap::new();
        map.insert("Test".to_owned(), "Testval".to_owned());
        map.insert("Test2".to_owned(), "another".to_owned());
        roundtrip_success!(HashMap<String, String>, map);
    }

    macro_rules! deserialize_test(
        ($e:expr, $result:expr) => ({
            use serde::de;
            let mut d = $e.into_deserializer();
            let json: Result<Json, _>  = de::Deserialize::deserialize(&mut d);
            assert!(json.is_ok());
            assert_eq!(json.unwrap().to_bytes(), $result);
        })
    );

    #[test]
    fn serde_deserialize() {
        use serde::de::value::ValueDeserializer;

        macro_rules! check_num(
           ($t:ident) => ({
               use std::$t;
               let mut val: $t;

               val = 0;
               deserialize_test!(val, format!("{}", val).as_bytes());
               val = 100;
               deserialize_test!(val, format!("{}", val).as_bytes());
               val = $t::MIN;
               deserialize_test!(val, format!("{}", val).as_bytes());
               val = $t::MAX;
               deserialize_test!(val, format!("{}", val).as_bytes());
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

        deserialize_test!(0.375f32, b"0.375");
        deserialize_test!(0.375f64, b"0.375");
        deserialize_test!("Test1".to_string(), b"\"Test1\"");
        deserialize_test!((), b"null");
        deserialize_test!(true, b"true");
        deserialize_test!(false, b"false");

        deserialize_test!(vec![true, false, true, true], b"[true, false, true, true]");
        let mut map = HashMap::new();
        map.insert("Test".to_owned(), "Testval".to_owned());
        map.insert("Test2".to_owned(), "another".to_owned());
        deserialize_test!(map, "{\"Test\": \"Testval\", \"Test2\": \"another\"}".as_bytes());
    }
}


