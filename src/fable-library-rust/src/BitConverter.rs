pub mod BitConverter_ {
    use crate::Native_::{String};
    use crate::NativeArray_::{new_array, Array};
    use crate::String_::{string, fromString};

    #[cfg(target_endian = "little")]
    const littleEndian: bool = true;
    #[cfg(not(target_endian = "little"))]
    const littleEndian: bool = false;

    pub fn isLittleEndian() -> bool {
        littleEndian
    }

    pub fn getBytesBoolean(value: bool) -> Array<u8> {
        new_array(&(value as u8).to_ne_bytes())
    }

    pub fn getBytesChar(value: char) -> Array<u8> {
        let code = value as u16;
        new_array(&code.to_ne_bytes())
    }

    pub fn getBytesInt16(value: i16) -> Array<u8> {
        new_array(&value.to_ne_bytes())
    }

    pub fn getBytesInt32(value: i32) -> Array<u8> {
        new_array(&value.to_ne_bytes())
    }

    pub fn getBytesInt64(value: i64) -> Array<u8> {
        new_array(&value.to_ne_bytes())
    }

    pub fn getBytesUInt16(value: u16) -> Array<u8> {
        new_array(&value.to_ne_bytes())
    }

    pub fn getBytesUInt32(value: u32) -> Array<u8> {
        new_array(&value.to_ne_bytes())
    }

    pub fn getBytesUInt64(value: u64) -> Array<u8> {
        new_array(&value.to_ne_bytes())
    }

    // pub fn getBytesFloat16(value: f16) -> Array<u8> {
    //     new_array(&value.to_ne_bytes())
    // }

    pub fn getBytesFloat32(value: f32) -> Array<u8> {
        new_array(&value.to_ne_bytes())
    }

    pub fn getBytesFloat64(value: f64) -> Array<u8> {
        new_array(&value.to_ne_bytes())
    }

    pub fn doubleToInt64Bits(value: f64) -> i64 {
        f64::to_bits(value) as i64
    }

    pub fn doubleToUInt64Bits(value: f64) -> u64 {
        f64::to_bits(value)
    }

    pub fn int64BitsToDouble(value: i64) -> f64 {
        f64::from_bits(value as u64)
    }

    pub fn uInt64BitsToDouble(value: u64) -> f64 {
        f64::from_bits(value)
    }

    pub fn singleToInt32Bits(value: f32) -> i32 {
        f32::to_bits(value) as i32
    }

    pub fn singleToUInt32Bits(value: f32) -> u32 {
        f32::to_bits(value)
    }

    pub fn int32BitsToSingle(value: i32) -> f32 {
        f32::from_bits(value as u32)
    }

    pub fn uInt32BitsToSingle(value: u32) -> f32 {
        f32::from_bits(value)
    }

    // pub fn halfToInt16Bits(value: f16) -> i16 {
    //     f16::to_bits(value) as i16
    // }

    // pub fn halfToUInt16Bits(value: f16) -> u16 {
    //     f16::to_bits(value)
    // }

    // pub fn int16BitsToHalf(value: i16) -> f16 {
    //     f16::from_bits(value as u16)
    // }

    // pub fn uInt16BitsToHalf(value: u16) -> f16 {
    //     f16::from_bits(value)
    // }

    #[inline]
    fn copy_from_slice(buf: &mut [u8], bytes: &[u8], index: i32) {
        buf.copy_from_slice(&bytes[(index as usize)..(index as usize) + buf.len()]);
    }

    pub fn toBoolean(bytes: Array<u8>, offset: i32) -> bool {
        let mut buf = u8::to_ne_bytes(0);
        copy_from_slice(&mut buf, &bytes, offset);
        (u8::from_ne_bytes(buf) != 0) as bool
    }

    pub fn toChar(bytes: Array<u8>, offset: i32) -> char {
        let mut buf = u16::to_ne_bytes(0);
        copy_from_slice(&mut buf, &bytes, offset);
        let code = u16::from_ne_bytes(buf);
        char::from_u32(code as u32).unwrap()
    }

    pub fn toInt16(bytes: Array<u8>, offset: i32) -> i16 {
        let mut buf = i16::to_ne_bytes(0);
        copy_from_slice(&mut buf, &bytes, offset);
        i16::from_ne_bytes(buf)
    }

    pub fn toInt32(bytes: Array<u8>, offset: i32) -> i32  {
        let mut buf = i32::to_ne_bytes(0);
        copy_from_slice(&mut buf, &bytes, offset);
        i32::from_ne_bytes(buf)
    }

    pub fn toInt64(bytes: Array<u8>, offset: i32) -> i64  {
        let mut buf = i64::to_ne_bytes(0);
        copy_from_slice(&mut buf, &bytes, offset);
        i64::from_ne_bytes(buf)
    }

    pub fn toUInt16(bytes: Array<u8>, offset: i32) -> u16  {
        let mut buf = u16::to_ne_bytes(0);
        copy_from_slice(&mut buf, &bytes, offset);
        u16::from_ne_bytes(buf)
    }

    pub fn toUInt32(bytes: Array<u8>, offset: i32) -> u32  {
        let mut buf = u32::to_ne_bytes(0);
        copy_from_slice(&mut buf, &bytes, offset);
        u32::from_ne_bytes(buf)
    }

    pub fn toUInt64(bytes: Array<u8>, offset: i32) -> u64  {
        let mut buf = u64::to_ne_bytes(0);
        copy_from_slice(&mut buf, &bytes, offset);
        u64::from_ne_bytes(buf)
    }

    pub fn toSingle(bytes: Array<u8>, offset: i32) -> f32 {
        let mut buf = f32::to_ne_bytes(0.0);
        copy_from_slice(&mut buf, &bytes, offset);
        f32::from_ne_bytes(buf)
    }

    pub fn toDouble(bytes: Array<u8>, offset: i32) -> f64 {
        let mut buf = f64::to_ne_bytes(0.0);
        copy_from_slice(&mut buf, &bytes, offset);
        f64::from_ne_bytes(buf)
    }

    fn bytes_to_string(bytes: &[u8], offset: i32, count: i32) -> string {
        fn encode(d: u8) -> u8 {
            match d {
                0..=9 => b'0' + d,
                10..=15 => b'A' + d - 10,
                _ => unreachable!(),
            }
        }
        let mut s: String = bytes[(offset as usize)..(offset + count) as usize]
            .iter()
            .flat_map(|b| [encode(b >> 4), encode(b & 0x0F), b'-'])
            .map(|d| d as char)
            .collect();
        if s.len() > 0 {
            s.truncate(s.len() - 1); // remove last dash
        }
        fromString(s)
    }

    pub fn toString1(bytes: Array<u8>) -> string {
        bytes_to_string(&bytes, 0, bytes.len() as i32)
    }

    pub fn toString2(bytes: Array<u8>, offset: i32) -> string {
        bytes_to_string(&bytes, offset, bytes.len() as i32 - offset)
    }

    pub fn toString3(bytes: Array<u8>, offset: i32, count: i32) -> string {
        bytes_to_string(&bytes, offset, count)
    }
}
