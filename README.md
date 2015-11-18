[![Status](https://travis-ci.org/apoelstra/strason.png?branch=master)](https://travis-ci.org/apoelstra/strason)

# Strason

Support for stringly-typed Json parsing and serialization.

[Documentation](https://www.wpsoftware.net/rustdoc/strason/)

This project differs from other Json parsers in three main ways:

1. Numbers are read as strings and stored as strings. It is the responsibility
   of the calling code to parse them. (They are validated to be valid Json
   numbers.)

2. The order of fields in Json objects is preserved, for applicatons that care
   about this. Note that this means objects are stored as a `Vec` of (key, value)
   pairs which means accessing fields by name takes linear time. Callers who
   need to do this may have to unload into their own `HashMap`.

   It also means that `Json` objects will not test equal unless the order of their
   fields matches.

3. Multiple fields with the same name can be stored and serialized. This shouldn't
   ever be done, but is useful for interoperating with buggy software.

The library has one type, `Json`, which is defined as follows:
```rust
pub enum Json {
    Null,
    Bool(bool),
    Number(String),
    String(String),
    Array(Vec<Json>),
    Object(Vec<(String, Json)>)
}
```
That is, except for nulls and booleans, all data are represented by strings.
(Actually, the real implementation is hidden, to give me freedom to add, e.g.
indexing support for `Json::Object`s or something without breaking. But this
is what it looks like now.)

These objects can be created from raw byte data by the method
`Json::from_iter`, `Json::from_str` and `Json::from_reader`. They can
be reserialized with the `Json::to_bytes()` method.

Alternately, `Json` objects can be created using `std::From`, such as
```
let s = "A regular old Rust string".to_owned();
let json = From::from(s);
```
Implementations are available for all integer types, as well as `bool`, `String`
and `()`. To construct a JSON number from a string, use `Json::from_str` as
above so that the number can be validated to have correct form.

Full compliance with ECMA 404 is expected. Any deviations are bugs.

All byte inputs should round-trip (deserialize then serialize) to the same
thing, up to (a) variations in whitespace, (b) escaping of the `/` character
in strings, (c) choice of when to use Unicode `\uXXXX` escapes, and the
capitalization of them. Also UCS-2 points that are not UTF-16 (there are few
of these and they are meaningless anyway) will be rejected at parse time,
unlike most parsers in use. Any deviations beyond this are bugs.

# Usage

To use strason, just add the following to your Cargo.toml.

```toml
[dependencies]
strason = "0.2"
```

