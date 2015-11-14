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

//! # Serialization support
//!

use encoding::{Encoding, EncoderTrap};
use encoding::all::UTF_16BE;
use std::{io, str};
use std::io::Write;

use {Json, JsonInner};

fn serialize_string<W: io::Write>(s: &str, mut w: W) -> io::Result<()> {
// TODO utf16
    try!(w.write(b"\""));
    for ch in s.chars() {
        match ch {
            '\x07' => { try!(w.write(b"\\b")); }
            '\x0c' => { try!(w.write(b"\\f")); }
            '\n' => { try!(w.write(b"\\n")); }
            '\r' => { try!(w.write(b"\\r")); }
            '\t' => { try!(w.write(b"\\t")); }
            '\\' => { try!(w.write(b"\\\\")); }
            '\x00'...'\x7f' => { try!(w.write(&[ch as u8])); }
            _ => {
                let mut ch_bytes = [0u8; 4];
                write!(&mut ch_bytes[..], "{}", ch).unwrap();
                // Unsafety as we are doing unchecked UTF8 conversion. This is safe
                // because we obtained this data by UTF8-encoding a char literally 
                // one line previous.
                let ch_str = unsafe { str::from_utf8_unchecked(&ch_bytes[..]) };
                let unicode = try!(UTF_16BE.encode(ch_str, EncoderTrap::Strict).map_err(|_| io::Error::new(io::ErrorKind::InvalidData, "unencodable utf16")));
                for ch in &unicode {
                    try!(write!(w, "\\u{:04x}", ch));
                }
            }
        }
    }
    try!(w.write(b"\""));
    Ok(())
}

/// The main serialization function
pub fn serialize<W: io::Write>(json: &Json, w: &mut W) -> io::Result<()> {
    match json.0 {
        JsonInner::Null => { try!(w.write(b"null")); }
        JsonInner::Bool(true) => { try!(w.write(b"true")); }
        JsonInner::Bool(false) => { try!(w.write(b"false")); }
        JsonInner::Number(ref s) => { try!(w.write(s.as_bytes())); }
        JsonInner::String(ref s) => { try!(serialize_string(&s[..], &mut *w)); }
        JsonInner::Array(ref v) => {
            try!(w.write(b"["));
            let mut first = true;
            for elem in v {
                if !first {
                    try!(w.write(b", "));
                }
                try!(serialize(elem, &mut *w));
                first = false;
            }
            try!(w.write(b"]"));
        }
        JsonInner::Object(ref m, ref v) => {
            try!(w.write(b"{"));
            let mut first = true;
            // Unsafe block needed since we are borrowing the StringRefs in
            // here. This is OK because these references live as long as the
            // hashmap, which we are borrowing (as `m`) for the duration of
            // this block anyway.
            unsafe {
                for key in v.iter().map(|k| k.borrow()) {
                    if !first {
                        try!(w.write(b", "));
                    }
                    try!(serialize_string(key, &mut *w));
                    try!(w.write(b": "));
                    try!(serialize(&m[key], &mut *w));
                    first = false;
                }
                try!(w.write(b"}"));
            }
        }
    };
    Ok(())
}

#[cfg(test)]
mod tests {
    use Json;

    fn round_trip(s: &str) -> bool {
        let dec = Json::from_str(s).unwrap();
        let enc = dec.to_bytes();

        let mut it1 = s.as_bytes().iter();
        let mut it2 = enc.iter();

        let mut in_string = false;
        loop {
            let mut ch1 = None;
            let mut ch2 = None;

            // Scan both iters to non-whitespace
            if !in_string {
                while let Some(ch) = it1.next() {
                    if *ch != b' ' && *ch != b'\r' && *ch != b'\n' {
                        ch1 = Some(*ch);
                        break;
                    }
                }

                while let Some(ch) = it2.next() {
                    if *ch != b' ' && *ch != b'\r' && *ch != b'\n' {
                        ch2 = Some(*ch);
                        break;
                    }
                }
            } else {
                ch1 = it1.next().map(|c| *c);
                ch2 = it2.next().map(|c| *c);
            }


            // Compare the next character
            match (ch1, ch2) {
                (Some(c1), Some(c2)) => {
                    if c1 != c2 {
                        return false;
                    }
                    // If this character is escaped, compare the next one. We do this
                    // because non-escaped characters might change the in-string status.
                    // (Technically, the \ character is illegal outside of strings, but
                    // here is not the place to be checking this.)
                    if c1 == b'\\' {
                        let (next1, next2) = (it1.next(), it2.next());
                        if next1 != next2 {
                            return false;
                        }
                    } else if c1 == b'"' {
                        in_string = !in_string;
                    }
                }
                // One exhausted but not the other -- failure
                (Some(_), None) | (None, Some(_)) => { return false; }
                // Both exhausted simultaneously -- success :)
                (None, None) => { return true; }
            }
        }
    }

    #[test]
    fn test_round_trip() {
        assert!(round_trip("null"));
        assert!(round_trip("true"));
        assert!(round_trip("false"));
        assert!(round_trip("[ ]"));
        assert!(round_trip("{ }"));
        assert!(round_trip("\"     \\t\\n\\t     \""));

        assert!(round_trip("\"\\n\\r\\f\\b\\\\\""));

        assert!(round_trip("-0"));
        assert!(round_trip("100"));
        assert!(round_trip("-100e5"));
        assert!(round_trip("-100E+5"));
        assert!(round_trip("100E-5"));
        assert!(round_trip("-1.5e-100"));
        assert!(round_trip("1.5e+100"));

        assert!(round_trip("[1, 2, 3, true, null, false, [ 10, 20, 30 ]     ]"));
        assert!(round_trip("{ \"key\": \"val\", \"true\": [] }"));
    }
}

