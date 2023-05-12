#[cfg_attr(rustfmt, rustfmt::skip)]
pub mod Convert_ {
    use crate::Native_::{MutCell, ToString, Vec};
    use crate::NativeArray_::{array_from, Array};
    use crate::String_::{string, fromString, substring};
    use core::fmt::{Display, Binary, Octal, LowerHex};
    use core::str::FromStr;

    pub trait TryParse<N>: PartialEq + Default {
        fn try_parse(s: &str, radix: i32) -> Option<N>;
    }

    macro_rules! int_try_parse_impl {
        ($($t:ty)*) => ($(
            impl TryParse<$t> for $t {
                #[inline]
                fn try_parse(s: &str, radix: i32) -> Option<$t> {
                    <$t>::from_str_radix(s, radix as u32).ok()
                }
            }
        )*)
    }

    macro_rules! float_try_parse_impl {
        ($($t:ty)*) => ($(
            impl TryParse<$t> for $t {
                #[inline]
                fn try_parse(s: &str, _radix: i32) -> Option<$t> {
                    <$t>::from_str(s).ok()
                }
            }
        )*)
    }

    int_try_parse_impl!(i8 i16 i32 i64 i128 isize u8 u16 u32 u64 u128 usize);
    float_try_parse_impl!(f32 f64);

    const AllowHexSpecifier: i32 = 512;

    fn radix_from_style(style: i32) -> i32 {
        // TODO: more styles
        if (style & AllowHexSpecifier) != 0 {
            16
        } else {
            10
        }
    }

    fn radix_from_string(s: string) -> (string, i32) {
        if s.starts_with("0x") { (substring(s, 2), 16) }
        else if s.starts_with("0o") { (substring(s, 2), 8) }
        else if s.starts_with("0b") { (substring(s, 2), 2) }
        else { (s, 10) }
    }

    fn from_string_radix<N: TryParse<N>>(s: string, radix: i32) -> N {
        match radix {
            2|8|10|16 => (),
            _ => panic!("Invalid Base."),
        }
        match N::try_parse(s.trim(), radix) {
            Some(d) => d,
            None =>
                panic!("The input string '{}' was not in a correct format.", s),
        }
    }

    fn trim_separators(s: string) -> string {
        if s.contains("_") {
            fromString(s.replace("_", ""))
        } else {
            s
        }
    }

    fn from_string<N: TryParse<N>>(s: string) -> N {
        let s = trim_separators(s);
        let (s, radix) = radix_from_string(s);
        from_string_radix(s, radix)
    }

    fn from_style<N: TryParse<N>>(s: string, style: i32) -> N {
        let radix = radix_from_style(style);
        from_string_radix(s, radix)
    }

    fn from_style_i8(s: string, style: i32) -> i8 {
        if (style & AllowHexSpecifier) != 0 {
            from_style::<u8>(s, style) as i8
        } else {
            from_style(s, style)
        }
    }

    fn from_style_i16(s: string, style: i32) -> i16 {
        if (style & AllowHexSpecifier) != 0 {
            from_style::<u16>(s, style) as i16
        } else {
            from_style(s, style)
        }
    }

    fn from_style_i32(s: string, style: i32) -> i32 {
        if (style & AllowHexSpecifier) != 0 {
            from_style::<u32>(s, style) as i32
        } else {
            from_style(s, style)
        }
    }

    fn from_style_i64(s: string, style: i32) -> i64 {
        if (style & AllowHexSpecifier) != 0 {
            from_style::<u64>(s, style) as i64
        } else {
            from_style(s, style)
        }
    }

    fn from_style_i128(s: string, style: i32) -> i128 {
        if (style & AllowHexSpecifier) != 0 {
            from_style::<u128>(s, style) as i128
        } else {
            from_style(s, style)
        }
    }

    fn from_style_isize(s: string, style: i32) -> isize {
        if (style & AllowHexSpecifier) != 0 {
            from_style::<usize>(s, style) as isize
        } else {
            from_style(s, style)
        }
    }

    // ----------------------------------------------------
    // Public interface
    // ----------------------------------------------------

    pub fn parseBoolean(s: string) -> bool {
        let trimmed = s.trim().to_ascii_lowercase();
        match bool::from_str(trimmed.as_str()) {
            Ok(b) => b,
            Err(_e) => panic!("String '{}' was not recognized as a valid Boolean.", s),
        }
    }

    pub fn parseChar(s: string) -> char {
        match char::from_str(s.as_str()) {
            Ok(b) => b,
            Err(_e) => panic!("String must be exactly one character long."),
        }
    }

    pub fn parseInt8(s: string, style: i32) -> i8 { from_style_i8(s, style) }
    pub fn parseInt16(s: string, style: i32) -> i16 { from_style_i16(s, style) }
    pub fn parseInt32(s: string, style: i32) -> i32 { from_style_i32(s, style) }
    pub fn parseInt64(s: string, style: i32) -> i64 { from_style_i64(s, style) }
    pub fn parseInt128(s: string, style: i32) -> i128 { from_style_i128(s, style) }
    pub fn parseNativeInt(s: string, style: i32) -> isize { from_style_isize(s, style) }
    pub fn parseUInt8(s: string, style: i32) -> u8 { from_style(s, style) }
    pub fn parseUInt16(s: string, style: i32) -> u16 { from_style(s, style) }
    pub fn parseUInt32(s: string, style: i32) -> u32 { from_style(s, style) }
    pub fn parseUInt64(s: string, style: i32) -> u64 { from_style(s, style) }
    pub fn parseUInt128(s: string, style: i32) -> u128 { from_style(s, style) }
    pub fn parseUNativeInt(s: string, style: i32) -> usize { from_style(s, style) }
    // pub fn parseFloat16(s: string, style: i32) -> f16 { from_style(s, style) }
    pub fn parseFloat32(s: string, style: i32) -> f32 { from_style(s, style) }
    pub fn parseFloat64(s: string, style: i32) -> f64 { from_style(s, style) }

    pub fn toInt8(s: string) -> i8 { from_string(s) }
    pub fn toInt16(s: string) -> i16 { from_string(s) }
    pub fn toInt32(s: string) -> i32 { from_string(s) }
    pub fn toInt64(s: string) -> i64 { from_string(s) }
    pub fn toInt128(s: string) -> i128 { from_string(s) }
    pub fn toNativeInt(s: string) -> isize { from_string(s) }
    pub fn toUInt8(s: string) -> u8 { from_string(s) }
    pub fn toUInt16(s: string) -> u16 { from_string(s) }
    pub fn toUInt32(s: string) -> u32 { from_string(s) }
    pub fn toUInt64(s: string) -> u64 { from_string(s) }
    pub fn toUInt128(s: string) -> u128 { from_string(s) }
    pub fn toUNativeInt(s: string) -> usize { from_string(s) }
    // pub fn toFloat16(s: string) -> f16 { from_string(s) }
    pub fn toFloat32(s: string) -> f32 { from_string(s) }
    pub fn toFloat64(s: string) -> f64 { from_string(s) }

    pub fn toBoolean<N: PartialEq + Default>(n: N) -> bool { !(n == N::default()) }

    // pub fn toChar<N>(n: N) -> char {
    //     core::char::from_u32(n.to_u32().unwrap()).unwrap()
    // }

    pub fn toInt8_radix(s: string, radix: i32) -> i8 { from_string_radix(s, radix) }
    pub fn toInt16_radix(s: string, radix: i32) -> i16 { from_string_radix(s, radix) }
    pub fn toInt32_radix(s: string, radix: i32) -> i32 { from_string_radix(s, radix) }
    pub fn toInt64_radix(s: string, radix: i32) -> i64 { from_string_radix(s, radix) }
    pub fn toInt128_radix(s: string, radix: i32) -> i128 { from_string_radix(s, radix) }
    pub fn toNativeInt_radix(s: string, radix: i32) -> isize { from_string_radix(s, radix) }
    pub fn toUInt8_radix(s: string, radix: i32) -> u8 { from_string_radix(s, radix) }
    pub fn toUInt16_radix(s: string, radix: i32) -> u16 { from_string_radix(s, radix) }
    pub fn toUInt32_radix(s: string, radix: i32) -> u32 { from_string_radix(s, radix) }
    pub fn toUInt64_radix(s: string, radix: i32) -> u64 { from_string_radix(s, radix) }
    pub fn toUInt128_radix(s: string, radix: i32) -> u128 { from_string_radix(s, radix) }
    pub fn toUNativeInt_radix(s: string, radix: i32) -> usize { from_string_radix(s, radix) }

    pub fn tryParseBoolean(s: string, res: &MutCell<bool>) -> bool {
        let trimmed = s.trim().to_ascii_lowercase();
        match bool::from_str(trimmed.as_str()) {
            Ok(b) => { res.set(b); true },
            Err(_e) => false,
        }
    }

    pub fn tryParseChar(s: string, res: &MutCell<char>) -> bool {
        match char::from_str(s.as_str()) {
            Ok(c) => { res.set(c); true },
            Err(_e) => false,
        }
    }

    pub fn tryParse<N: TryParse<N>>(s: string, style: i32, res: &MutCell<N>) -> bool {
        let radix = radix_from_style(style);
        match N::try_parse(s.trim(), radix) {
            Some(d) => { res.set(d); true }
            None => false,
        }
    }

    pub fn toStringRadix<N>(n: N, radix: i32) -> string
    where N: Display + Binary + Octal + LowerHex,
    {
        let s = match radix {
            2 => format_args!("{:b}", n).to_string(),
            8 => format_args!("{:o}", n).to_string(),
            10 => format_args!("{}", n).to_string(),
            16 => format_args!("{:x}", n).to_string(),
            _ => panic!("Invalid Base."),
        };
        fromString(s)
    }

    pub fn toHexString(bytes: Array<u8>) -> string {
        fn encode(d: u8) -> u8 {
            match d {
                0..=9 => b'0' + d,
                10..=15 => b'A' + d - 10,
                _ => unreachable!(),
            }
        }
        let s = bytes
            .iter()
            .flat_map(|b| [b >> 4, b & 0x0F])
            .map(|d| encode(d) as char)
            .collect();
        fromString(s)
    }

    pub fn fromHexString(s: string) -> Array<u8> {
        fn decode(c: u8) -> u8 {
            match c {
                b'0'..=b'9' => c - b'0',
                b'A'..=b'F' => c - b'A' + 10,
                b'a'..=b'f' => c - b'a' + 10,
                _ => panic!("The input is not a valid hex string as it contains a non-hex character."),
            }
        }
        let chars = s.as_bytes();
        let bytes = chars
            .chunks_exact(2)
            .map(|x| decode(x[0]) << 4 | decode(x[1]))
            .collect();
        array_from(bytes)
    }

    pub fn toBase64String(bytes: Array<u8>) -> string {
        fn encode(d: u8) -> u8 {
            match d {
                0..=25 => b'A' + d,
                26..=51 => b'a' + d - 26,
                52..=61 => b'0' + d - 52,
                62 => b'+',
                63 => b'/',
                64 => b'=',
                _ => unreachable!()
            }
        }
        fn split(chunk: &[u8]) -> [u8; 4] {
            match chunk.len() {
                1 => [
                    chunk[0] >> 2,
                    (chunk[0] & 0x3) << 4,
                    64,
                    64
                ],
                2 => [
                    chunk[0] >> 2,
                    (chunk[0] & 0x3) << 4 | chunk[1] >> 4,
                    (chunk[1] & 0xF) << 2,
                    64
                ],
                3 => [
                    chunk[0] >> 2,
                    (chunk[0] & 0x3) << 4 | chunk[1] >> 4,
                    (chunk[1] & 0xF) << 2 | chunk[2] >> 6,
                    chunk[2] & 0x3F
                ],
                _ => unreachable!(),
            }
        }
        let s = bytes
            .chunks(3)
            .flat_map(|chunk| split(chunk))
            .map(|d| encode(d) as char)
            .collect();
        fromString(s)
    }

    pub fn fromBase64String(s: string) -> Array<u8> {
        fn decode(c: u8) -> u8 {
            match c {
                b'A'..=b'Z' => c - b'A',
                b'a'..=b'z' => c - b'a' + 26,
                b'0'..=b'9' => c - b'0' + 52,
                b'+' => 62,
                b'/' => 63,
                b'=' => 0,
                _ => panic!("The input is not a valid Base-64 string as it contains a non-base 64 character, more than two padding characters, or an illegal character among the padding characters."),
            }
        }
        fn merge(base64Chunk: &[u8]) -> [u8; 3] {
            let mut chunk = [0u8; 4];
            for i in 0..base64Chunk.len() {
                chunk[i] = decode(base64Chunk[i]);
            }
            match base64Chunk.len() {
                1 => [
                    (chunk[0] & 0x3F) << 2,
                    0,
                    0
                ],
                2 => [
                    (chunk[0] & 0x3F) << 2 | chunk[1] >> 4,
                    (chunk[1] & 0xF) << 4,
                    0
                ],
                3 => [
                    (chunk[0] & 0x3F) << 2 | chunk[1] >> 4,
                    (chunk[1] & 0xF) << 4 | chunk[2] >> 2,
                    (chunk[2] & 0x3) << 6,
                ],
                4 => [
                    (chunk[0] &  0x3F) << 2 | chunk[1] >> 4,
                    (chunk[1] & 0xF) << 4 | chunk[2] >> 2,
                    (chunk[2] & 0x3) << 6 | chunk[3] & 0x3F,
                ],
                _ => unreachable!()
            }
        }
        let chars = s.as_bytes();
        let mut bytes: Vec<u8> = chars
            .chunks(4)
            .flat_map(merge)
            .collect();
        let padding_len =
            if chars.len() >= 1 && chars[chars.len() - 1] == b'=' {
                if chars.len() >= 2 && chars[chars.len() - 2] == b'=' { 2 }
                else { 1 }
            } else { 0 };
        bytes.truncate(3 * (chars.len() - padding_len) / 4);
        array_from(bytes)
    }

}
