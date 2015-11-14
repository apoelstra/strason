[![Status](https://travis-ci.org/apoelstra/strason.png?branch=master)](https://travis-ci.org/apoelstra/strason)

# Strason

Support for stringly-typed Json parsing and serialization.

[Documentation](https://www.wpsoftware.net/rustdoc/strason/)

The main motivation for this project is to support projects which encode bignums as
Json numbers, which would be lossily decoded by libraries such as serde, which
read numbers into Rust floating-point types. A secondary motivation is projects
which manipulate Json structures but don't really process the values, for which
the overhead of de/serializing strings into other types is unnecessary.

The library has one type, `Json`, which is defined as follows:
```rust
pub enum Json {
    Null,
    Bool(bool),
    Number(String),
    String(String),
    Array(Vec<Json>),
    Object(HashMap<String, Json>)
}
```
That is, except for nulls and booleans, all data are represented by strings.
(Actually, the real implementation is hidden, and has some extra data to
track, for example, the order that object entries were added in. This is
to prevent reordering of fields on reserialization.)

These objects can be created from raw byte data by the method
`Json::from_iter`, `Json::from_str` and `Json::from_reader`. They can
be reserialized with the `Json::to_bytes()` method.

Full compliance with ECMA 404 is expected. Any deviations are bugs.

All byte inputs should round-trip (deserialize then serialize) to the same
thing, up to variations in whitespace. Any deviations are bugs.

# Usage

To use strason, just add the following to your Cargo.toml.

```toml
[dependencies]
strason = "0.1"
```

