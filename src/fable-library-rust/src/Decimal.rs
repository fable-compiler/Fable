#[cfg(feature = "decimal")]
pub mod Decimal_ {
    use crate::Native_::{array, Array, Lrc, MutCell, Vec};
    use crate::String_::{string, toString as toString_1};

    use rust_decimal::prelude::*;
    pub use rust_decimal::Decimal as decimal;

    pub const Zero: Decimal = Decimal::ZERO;
    pub const One: Decimal = Decimal::ONE;
    pub const MinusOne: Decimal = Decimal::NEGATIVE_ONE;
    pub const MaxValue: Decimal = Decimal::MAX;
    pub const MinValue: Decimal = Decimal::MIN;

    // pub fn hash (x: Decimal) = x.GetHashCode()
    // pub fn compare (x: Decimal, y: Decimal) = compare x y
    // pub fn equals (x: Decimal, y: Decimal) = x == y

    pub fn scale(x: Decimal) -> u8 { x.scale() as u8 }

    pub fn abs(x: Decimal) -> Decimal { x.abs() }
    pub fn sign(x: Decimal) -> Decimal { x.signum() }

    pub fn floor(x: Decimal) -> Decimal { x.floor() }
    pub fn ceiling(x: Decimal) -> Decimal { x.ceil() }
    pub fn truncate(x: Decimal) -> Decimal { x.trunc() }
    pub fn pown(x: Decimal, n: i32) -> Decimal { x.powi(n as i64) }

    pub fn add(x: Decimal, y: Decimal) -> Decimal { x + y }
    pub fn subtract(x: Decimal, y: Decimal) -> Decimal { x - y }
    pub fn multiply(x: Decimal, y: Decimal) -> Decimal { x * y }
    pub fn divide(x: Decimal, y: Decimal) -> Decimal { x / y }
    pub fn remainder(x: Decimal, y: Decimal) -> Decimal { x % y }
    pub fn negate(x: Decimal) -> Decimal { -x }

    // pub fn isNegative(x: Decimal) -> bool { x.is_sign_negative() }
    // pub fn isPositive(x: Decimal) -> bool { x.is_sign_positive() }
    // pub fn isInteger(x: Decimal) -> bool { false } //TODO:
    // pub fn isOddInteger(x: Decimal) -> bool { false } //TODO:
    // pub fn isEvenInteger(x: Decimal) -> bool { false } //TODO:
    // pub fn isCanonical(x: Decimal) -> bool { false } //TODO:

    pub fn toInt8 (x: Decimal) -> i8 { x.to_i8().unwrap() }
    pub fn toUInt8 (x: Decimal) -> u8 { x.to_u8().unwrap() }
    pub fn toInt16 (x: Decimal) -> i16 { x.to_i16().unwrap() }
    pub fn toUInt16 (x: Decimal) -> u16 { x.to_u16().unwrap() }
    pub fn toInt32 (x: Decimal) -> i32 { x.to_i32().unwrap() }
    pub fn toUInt32 (x: Decimal) -> u32 { x.to_u32().unwrap() }
    pub fn toInt64 (x: Decimal) -> i64 { x.to_i64().unwrap() }
    pub fn toUInt64 (x :Decimal) -> u64 { x.to_u64().unwrap() }
    pub fn toFloat32 (x: Decimal) -> f32 { x.to_f32().unwrap() }
    pub fn toFloat64 (x: Decimal) -> f64 { x.to_f64().unwrap() }

    pub fn toBoolean (x: Decimal) -> bool { !x.is_zero() }

    pub fn toChar (x: Decimal) -> char {
        core::char::from_u32(x.to_u32().unwrap()).unwrap()
    }

    pub fn toString (x: Decimal) -> string {
        toString_1(&x)
    }

    pub fn tryParse(s: string, res: &MutCell<Decimal>) -> bool {
        match Decimal::from_str(s.as_ref()) {
            Ok(d) => { res.set(d); true },
            Err(e) => false,
        }
    }

    pub fn parse(s: string) -> Decimal {
        match Decimal::from_str(s.as_ref()) {
            Ok(d) => d,
            Err(e) => panic!("Input string was not in a correct format."),
        }
    }

    pub fn fromString(s: string) -> Decimal {
        Decimal::from_str(s.as_ref()).unwrap()
    }

    pub fn fromInt8(n: i8) -> Decimal { Decimal::from_i8(n).unwrap() }
    pub fn fromUInt8(n: u8) -> Decimal { Decimal::from_u8(n).unwrap() }
    pub fn fromInt16(n: i16) -> Decimal { Decimal::from_i16(n).unwrap() }
    pub fn fromUInt16(n: u16) -> Decimal { Decimal::from_u16(n).unwrap() }
    pub fn fromInt32(n: i32) -> Decimal { Decimal::from_i32(n).unwrap() }
    pub fn fromUInt32(n: u32) -> Decimal { Decimal::from_u32(n).unwrap() }
    pub fn fromInt64(n: i64) -> Decimal { Decimal::from_i64(n).unwrap() }
    pub fn fromUInt64(n: u64) -> Decimal { Decimal::from_u64(n).unwrap() }
    pub fn fromFloat32(n: f32) -> Decimal { Decimal::from_f32(n).unwrap() }
    pub fn fromFloat64(n: f64) -> Decimal { Decimal::from_f64(n).unwrap() }
    pub fn fromNativeInt(n: isize) -> Decimal { Decimal::from_isize(n).unwrap() }
    pub fn fromUNativeInt(n: usize) -> Decimal { Decimal::from_usize(n).unwrap() }

    pub fn fromParts(low: i32, mid: i32, high: i32, isNegative: bool, scale: u8) -> Decimal {
        Decimal::from_parts(low as u32, mid as u32, high as u32, isNegative, scale as u32)
    }

    pub fn fromInts(low: i32, mid: i32, high: i32, signExp: i32) -> Decimal {
        let isNegative = signExp < 0;
        let scale = ((signExp >> 16) & 0x7F) as u8;
        fromParts(low, mid, high, isNegative, scale)
    }

    pub fn fromIntArray(bits: Array<i32>) -> Decimal {
        fromInts(bits[0], bits[1], bits[2], bits[3])
    }

    pub fn getBits(x: Decimal) -> Array<i32> {
        let du = x.unpack();
        let low = du.lo as i32;
        let mid = du.mid as i32;
        let high = du.hi as i32;
        let scale = du.scale as i32;
        let signExp =
            if du.negative { -(scale << 16) } else { scale << 16 };
        array(&[low, mid, high, signExp])
    }

    pub fn round(x: Decimal) -> Decimal { x.round() }

    pub fn roundTo(x: Decimal, dp: i32) -> Decimal {
        x.round_dp(dp as u32)
    }

    pub enum MidpointRounding {
        ToEven = 0,
        AwayFromZero = 1,
        ToZero = 2,
        ToNegativeInfinity = 3,
        ToPositiveInfinity = 4
    }

    pub fn roundToMode(x: Decimal, dp: i32, mode: MidpointRounding) -> Decimal {
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

    pub fn roundMode(x: Decimal, mode: MidpointRounding) -> Decimal {
        roundToMode(x, 0, mode)
    }

}
