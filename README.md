# Strason

This project was an early attempt to parse JSON with arbitary-precision numbers
before the `arbitrary_precision` feature of `serde_json` was added.

It did so by abusing the `serde` API to sneak pointers through `usize`s, and
later, by disguising pointers of unrelated types as `&[u8]`s. This involved
unjustifiable assumptions about alignment and was likely unsound even when
those assumptions were true.

It is now archived and unmaintained.

Do not use this crate. Use `serde_json` or another JSON parser instead.

