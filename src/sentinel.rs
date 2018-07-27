// Stringly-Typed JSON Library for Rust
// Written in 2018 by
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

//! # Unsafe functionality to support detecting our de/serializer without a modern compiler supporting specialization
//!
//! The goal of this detection is to have a `Json` object be able to pass a
//! pointer to itself to a `Serializer` or `Deserializer` which is trying
//! to transform it to/from some serialized form. The serde API gives us no
//! ability to pass pointers directly like this, so we will instead smuggle
//! it in an unrelated `usize` parameter which is guaranteed to be large
//! enough to hold a pointer.
//!
//! Since this is inherently extremely unsafe, and if triggered by accident
//! could cause some random piece of memory to be dereferenced, we need to
//! ensure that we are *certain* in our `Serializer`/`Deserializer` code
//! that we are actually dealing with a `Json` object that knows how to set
//! this up.
//!

use std::fmt;
use serde::{de, ser};

/// Trait that distinguishes sentinels from non-sentinels. The trick is
/// that it needs to be implemented for all fmt::Display types so that
/// it will be accessible from our `ser::Error::custom` method.
pub trait IsSentinel {
    fn is_sentinel(&self) -> bool;
}

/// Object that, when constructed, will return `true` for `is_sentinel`,
/// which it does using a creative `Display` implementation.
struct Sentinel;

pub static SENTINEL_STR: &'static str = "$$$STRASON$$$SENTINEL$$$";

impl fmt::Display for Sentinel {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(SENTINEL_STR)
    }
}

impl<T: fmt::Display> IsSentinel for T {
    fn is_sentinel(&self) -> bool {
        struct DummyWrite(bool);
        impl fmt::Write for DummyWrite {
            fn write_str(&mut self, x: &str) -> fmt::Result {
                if x.as_ptr() == SENTINEL_STR.as_ptr() {
                    self.0 = true;
                }
                Ok(())
            }
            fn write_char(&mut self, _: char) -> fmt::Result { Ok(()) }
            fn write_fmt(&mut self, _: fmt::Arguments) -> fmt::Result { Ok(()) }
        }

        let mut check = DummyWrite(false);
        let _ = fmt::write(&mut check, format_args!("{}", self));
        check.0
    }
}

/// Function that detects our `Error` type, which uses 
pub fn detect_our_error_type_de<E: de::Error>() -> bool {
    let err = E::custom(Sentinel);
    err.description().as_ptr() == SENTINEL_STR.as_ptr()
}

/// Function that detects our `Error` type, which uses 
pub fn detect_our_error_type_ser<E: ser::Error>() -> bool {
    let err = E::custom(Sentinel);
    err.description().as_ptr() == SENTINEL_STR.as_ptr()
}

