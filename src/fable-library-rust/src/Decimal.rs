#[cfg(feature = "decimal")]
pub mod Decimal_ {
    use crate::Native_::{compare, Lrc, MutCell, Vec};
    use crate::NativeArray_::{new_array, Array};
    use crate::String_::{string, toString as toString_1};
    use core::cmp::Ordering;

    use rust_decimal::prelude::*;
    pub use rust_decimal::Decimal as decimal;

    pub const Zero: decimal = Decimal::ZERO;
    pub const One: decimal = Decimal::ONE;
    pub const MinusOne: decimal = Decimal::NEGATIVE_ONE;
    pub const MaxValue: decimal = Decimal::MAX;
    pub const MinValue: decimal = Decimal::MIN;

    // pub fn getHashCode(x: decimal) = x.GetHashCode()
    pub fn equals(x: decimal, y: decimal) -> bool { x.eq(&y) }
    pub fn compareTo(x: decimal, y: decimal) -> i32 { compare(&x, &y) }

    pub fn scale(x: decimal) -> u8 { x.scale() as u8 }

    pub fn abs(x: decimal) -> decimal { x.abs() }
    pub fn sign(x: decimal) -> i32 { x.signum().to_i32().unwrap() }

    pub fn max(x: decimal, y: decimal) -> decimal { x.max(y) }
    pub fn min(x: decimal, y: decimal) -> decimal { x.min(y) }

    pub fn maxMagnitude(x: decimal, y: decimal) -> decimal { if abs(x) > abs(y) {x} else {y} }
    pub fn minMagnitude(x: decimal, y: decimal) -> decimal { if abs(x) < abs(y) {x} else {y} }

    pub fn clamp(x: decimal, min: decimal, max: decimal) -> decimal {
        x.clamp(min, max).into()
    }

    pub fn floor(x: decimal) -> decimal { x.floor() }
    pub fn ceiling(x: decimal) -> decimal { x.ceil() }
    pub fn truncate(x: decimal) -> decimal { x.trunc() }
    pub fn pown(x: decimal, n: i32) -> decimal { x.powi(n as i64) }

    pub fn add(x: decimal, y: decimal) -> decimal { x + y }
    pub fn subtract(x: decimal, y: decimal) -> decimal { x - y }
    pub fn multiply(x: decimal, y: decimal) -> decimal { x * y }
    pub fn divide(x: decimal, y: decimal) -> decimal { x / y }
    pub fn remainder(x: decimal, y: decimal) -> decimal { x % y }
    pub fn negate(x: decimal) -> decimal { -x }

    pub fn isNegative(x: decimal) -> bool { x.is_sign_negative() }
    pub fn isPositive(x: decimal) -> bool { x.is_sign_positive() }
    // pub fn isInteger(x: decimal) -> bool { false } //TODO:
    // pub fn isEvenInteger(x: decimal) -> bool { false } //TODO:
    // pub fn isOddInteger(x: decimal) -> bool { false } //TODO:
    // pub fn isCanonical(x: decimal) -> bool { false } //TODO:

    pub fn toInt8(x: decimal) -> i8 { x.to_i8().unwrap() }
    pub fn toUInt8(x: decimal) -> u8 { x.to_u8().unwrap() }
    pub fn toInt16(x: decimal) -> i16 { x.to_i16().unwrap() }
    pub fn toUInt16(x: decimal) -> u16 { x.to_u16().unwrap() }
    pub fn toInt32(x: decimal) -> i32 { x.to_i32().unwrap() }
    pub fn toUInt32(x: decimal) -> u32 { x.to_u32().unwrap() }
    pub fn toInt64(x: decimal) -> i64 { x.to_i64().unwrap() }
    pub fn toUInt64(x: decimal) -> u64 { x.to_u64().unwrap() }
    pub fn toNativeInt(x: decimal) -> isize { x.to_isize().unwrap() }
    pub fn toUNativeInt(x: decimal) -> usize { x.to_usize().unwrap() }

    pub fn toFloat32(x: decimal) -> f32 { x.to_f32().unwrap() }
    pub fn toFloat64(x: decimal) -> f64 { x.to_f64().unwrap() }

    pub fn toDecimal(x: decimal) -> decimal { x }

    pub fn toBoolean(x: decimal) -> bool { !x.is_zero() }

    pub fn toChar(x: decimal) -> char {
        core::char::from_u32(x.to_u32().unwrap()).unwrap()
    }

    pub fn toString(x: decimal) -> string {
        toString_1(&x)
    }

    pub fn tryParse(s: string, res: &MutCell<Decimal>) -> bool {
        match Decimal::from_str(s.trim()) {
            Ok(d) => { res.set(d); true },
            Err(e) => false,
        }
    }

    pub fn parse(s: string) -> decimal {
        match Decimal::from_str(s.trim()) {
            Ok(d) => d,
            Err(e) => panic!("The input string {} was not in a correct format.", s),
        }
    }

    pub fn fromInt8(n: i8) -> decimal { Decimal::from_i8(n).unwrap() }
    pub fn fromUInt8(n: u8) -> decimal { Decimal::from_u8(n).unwrap() }
    pub fn fromInt16(n: i16) -> decimal { Decimal::from_i16(n).unwrap() }
    pub fn fromUInt16(n: u16) -> decimal { Decimal::from_u16(n).unwrap() }
    pub fn fromInt32(n: i32) -> decimal { Decimal::from_i32(n).unwrap() }
    pub fn fromUInt32(n: u32) -> decimal { Decimal::from_u32(n).unwrap() }
    pub fn fromInt64(n: i64) -> decimal { Decimal::from_i64(n).unwrap() }
    pub fn fromUInt64(n: u64) -> decimal { Decimal::from_u64(n).unwrap() }
    pub fn fromNativeInt(n: isize) -> decimal { Decimal::from_isize(n).unwrap() }
    pub fn fromUNativeInt(n: usize) -> decimal { Decimal::from_usize(n).unwrap() }

    pub fn fromFloat32(n: f32) -> decimal { Decimal::from_f32(n).unwrap() }
    pub fn fromFloat64(n: f64) -> decimal { Decimal::from_f64(n).unwrap() }

    pub fn fromDecimal(d: decimal) -> decimal { d }

    pub fn fromBoolean(b: bool) -> decimal {
        Decimal::from_u32(b as u32).unwrap()
    }

    pub fn fromChar(c: char) -> decimal {
        Decimal::from_u32(c as u32).unwrap()
    }

    pub fn fromString(s: string) -> decimal {
        Decimal::from_str(s.trim()).unwrap()
    }

    pub fn fromParts(low: i32, mid: i32, high: i32, isNegative: bool, scale: u8) -> decimal {
        Decimal::from_parts(low as u32, mid as u32, high as u32, isNegative, scale as u32)
    }

    pub fn fromInts(low: i32, mid: i32, high: i32, signExp: i32) -> decimal {
        let isNegative = signExp < 0;
        let scale = ((signExp >> 16) & 0x7F) as u8;
        fromParts(low, mid, high, isNegative, scale)
    }

    pub fn fromIntArray(bits: Array<i32>) -> decimal {
        fromInts(bits[0], bits[1], bits[2], bits[3])
    }

    pub fn getBits(x: decimal) -> Array<i32> {
        let du = x.unpack();
        let low = du.lo as i32;
        let mid = du.mid as i32;
        let high = du.hi as i32;
        let scale = du.scale as i32;
        let signExp =
            if du.negative { -(scale << 16) } else { scale << 16 };
        new_array(&[low, mid, high, signExp])
    }

    pub fn round(x: decimal) -> decimal { x.round() }

    pub fn roundTo(x: decimal, dp: i32) -> decimal {
        x.round_dp(dp as u32)
    }

    pub enum MidpointRounding {
        ToEven = 0,
        AwayFromZero = 1,
        ToZero = 2,
        ToNegativeInfinity = 3,
        ToPositiveInfinity = 4
    }

    pub fn roundToMode(x: decimal, dp: i32, mode: MidpointRounding) -> decimal {
        let strategy = match mode {
            MidpointRounding::ToEven => RoundingStrategy::MidpointNearestEven,
            MidpointRounding::AwayFromZero => RoundingStrategy::MidpointAwayFromZero,
            MidpointRounding::ToZero => RoundingStrategy::ToZero,
            MidpointRounding::ToNegativeInfinity => RoundingStrategy::ToNegativeInfinity,
            MidpointRounding::ToPositiveInfinity => RoundingStrategy::ToPositiveInfinity,
            // _ => RoundingStrategy::MidpointNearestEven,
        };
        x.round_dp_with_strategy(dp as u32, strategy)
    }

    pub fn roundMode(x: decimal, mode: MidpointRounding) -> decimal {
        roundToMode(x, 0, mode)
    }

    // pub fn copySign
    // pub fn createChecked
    // pub fn createSaturating
    // pub fn createTruncating
    // pub fn fromOACurrency
    // pub fn getTypeCode
    // pub fn toOACurrency
    // pub fn tryFormat
    // pub fn tryGetBits
}
