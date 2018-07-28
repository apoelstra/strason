extern crate strason;

use strason::Json;

fn do_test(data: &[u8]) {
    if let Ok(json) = Json::from_iter(data.iter().map(|x| Ok(*x))) {
        // When de/serializing a strason Json object to a strason Json object,
        // we use unsafe code to short-circuit the serde deserialization to
        // prevent any numbers being stringified.
        let rt_json = Json::from_serialize(json.clone()).expect("from_serialize");
        let rt2_json = rt_json.into_deserialize().expect("into_deserialize");

        assert_eq!(json, rt2_json);
    }
}

#[cfg(feature="honggfuzz")]
#[macro_use]
extern crate honggfuzz;

#[cfg(feature="honggfuzz")]
fn main() {
    loop {
        fuzz!(|d| { do_test(d) });
    }
}

#[cfg(test)]
mod tests {
    fn extend_vec_from_hex(hex: &str, out: &mut Vec<u8>) {
        let mut b = 0;
        for (idx, c) in hex.as_bytes().iter().enumerate() {
            b <<= 4;
            match *c {
                b'A'...b'F' => b |= c - b'A' + 10,
                b'a'...b'f' => b |= c - b'a' + 10,
                b'0'...b'9' => b |= c - b'0',
                _ => panic!("Bad hex"),
            }
            if (idx & 1) == 1 {
                out.push(b);
                b = 0;
            }
        }
    }

    #[test]
    fn duplicate_crash() {
        let mut a = Vec::new();
        extend_vec_from_hex("00000", &mut a);
        super::do_test(&a);
    }
}

