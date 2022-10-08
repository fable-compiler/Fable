#[cfg(feature = "bigint")]
pub mod BigInt_ {
    use crate::Decimal_::{decimal, truncate};
    use crate::Native_::{arrayFrom, Array, Lrc, MutCell, Vec};
    use crate::String_::{string, toString as toString_1};
    use core::cmp::Ordering;

    use num_bigint::*;
    use num_integer::*;
    use num_traits::*;

    pub use num_bigint::BigInt as bigint;

    pub fn zero() -> BigInt { BigInt::zero() }
    pub fn one() -> BigInt { BigInt::one() }
    pub fn minusOne() -> BigInt { -BigInt::one() }

    // pub fn getHashCode(x: &BigInt) = x.GetHashCode()
    pub fn equals(x: &BigInt, y: &BigInt) -> bool { x.eq(y) }
    pub fn compare(x: &BigInt, y: &BigInt) -> i32 {
        match x.cmp(y) {
            Ordering::Less => -1,
            Ordering::Greater => 1,
            Ordering::Equal => 0,
        }
    }

    pub fn abs(x: BigInt) -> BigInt { x.abs() }
    pub fn sign(x: BigInt) -> i32 { x.signum().to_i32().unwrap() }

    pub fn add(x: BigInt, y: BigInt) -> BigInt { x + y }
    pub fn subtract(x: BigInt, y: BigInt) -> BigInt { x - y }
    pub fn multiply(x: BigInt, y: BigInt) -> BigInt { x * y }
    pub fn divide(x: BigInt, y: BigInt) -> BigInt { x / y }
    pub fn remainder(x: BigInt, y: BigInt) -> BigInt { x % y }
    pub fn negate(x: BigInt) -> BigInt { -x }

    pub fn max(x: BigInt, y: BigInt) -> BigInt { x.max(y) }
    pub fn min(x: BigInt, y: BigInt) -> BigInt { x.min(y) }

    pub fn isZero(x: BigInt) -> bool { x.is_zero() }
    pub fn isOne(x: BigInt) -> bool { x.is_one() }
    pub fn isEven(x: BigInt) -> bool { x.is_even() }
    pub fn isOdd(x: BigInt) -> bool { x.is_odd() }
    // pub fn IsPowerOfTwo(x: BigInt) -> bool { false } //TODO:

    pub fn isNegative(x: BigInt) -> bool { x.is_negative() }
    pub fn isPositive(x: BigInt) -> bool { x.is_positive() }
    pub fn isEvenInteger(x: BigInt) -> bool { x.is_even() } //TODO:
    pub fn isOddInteger(x: BigInt) -> bool { x.is_odd() } //TODO:
    // pub fn isPow2(x: BigInt) -> bool { false } //TODO:

    pub fn toInt8 (x: BigInt) -> i8 { x.to_i8().unwrap() }
    pub fn toUInt8 (x: BigInt) -> u8 { x.to_u8().unwrap() }
    pub fn toInt16 (x: BigInt) -> i16 { x.to_i16().unwrap() }
    pub fn toUInt16 (x: BigInt) -> u16 { x.to_u16().unwrap() }
    pub fn toInt32 (x: BigInt) -> i32 { x.to_i32().unwrap() }
    pub fn toUInt32 (x: BigInt) -> u32 { x.to_u32().unwrap() }
    pub fn toInt64 (x: BigInt) -> i64 { x.to_i64().unwrap() }
    pub fn toUInt64 (x: BigInt) -> u64 { x.to_u64().unwrap() }
    pub fn toNativeInt(x: BigInt) -> isize { x.to_isize().unwrap() }
    pub fn toUNativeInt(x: BigInt) -> usize { x.to_usize().unwrap() }
    pub fn toFloat32 (x: BigInt) -> f32 { x.to_f32().unwrap() }
    pub fn toFloat64 (x: BigInt) -> f64 { x.to_f64().unwrap() }

    pub fn toDecimal (x: BigInt) -> decimal {
        decimal::from_str_radix(x.to_string().as_str(), 10).unwrap()
    }
    pub fn toBoolean (x: BigInt) -> bool { !x.is_zero() }

    pub fn toChar (x: BigInt) -> char {
        core::char::from_u32(x.to_u32().unwrap()).unwrap()
    }

    pub fn toString (x: BigInt) -> string {
        toString_1(&x)
    }

    pub fn toByteArray(x: BigInt) -> Array<u8> {
        arrayFrom(x.to_signed_bytes_le())
    }

    pub fn tryParse(s: string, res: &MutCell<BigInt>) -> bool {
        match BigInt::from_str_radix(s.as_ref(), 10) {
            Ok(d) => { res.set(d); true },
            Err(e) => false,
        }
    }

    pub fn parse(s: string) -> BigInt {
        match BigInt::from_str_radix(s.as_ref(), 10) {
            Ok(d) => d,
            Err(e) => panic!("Input string was not in a correct format."),
        }
    }

    pub fn fromZero() -> BigInt { BigInt::zero() }
    pub fn fromOne() -> BigInt { BigInt::one() }

    pub fn fromString(s: string) -> BigInt {
        BigInt::from_str_radix(s.as_ref(), 10).unwrap()
    }

    pub fn fromByteArray(bytes: Array<u8>) -> BigInt {
        BigInt::from_signed_bytes_le(bytes.as_ref())
    }

    pub fn fromInt8(n: i8) -> BigInt { BigInt::from_i8(n).unwrap() }
    pub fn fromUInt8(n: u8) -> BigInt { BigInt::from_u8(n).unwrap() }
    pub fn fromInt16(n: i16) -> BigInt { BigInt::from_i16(n).unwrap() }
    pub fn fromUInt16(n: u16) -> BigInt { BigInt::from_u16(n).unwrap() }
    pub fn fromInt32(n: i32) -> BigInt { BigInt::from_i32(n).unwrap() }
    pub fn fromUInt32(n: u32) -> BigInt { BigInt::from_u32(n).unwrap() }
    pub fn fromInt64(n: i64) -> BigInt { BigInt::from_i64(n).unwrap() }
    pub fn fromUInt64(n: u64) -> BigInt { BigInt::from_u64(n).unwrap() }
    pub fn fromFloat32(n: f32) -> BigInt { BigInt::from_f32(n).unwrap() }
    pub fn fromFloat64(n: f64) -> BigInt { BigInt::from_f64(n).unwrap() }
    pub fn fromNativeInt(n: isize) -> BigInt { BigInt::from_isize(n).unwrap() }
    pub fn fromUNativeInt(n: usize) -> BigInt { BigInt::from_usize(n).unwrap() }

    pub fn fromDecimal(d: decimal) -> BigInt {
        BigInt::from_str_radix(truncate(d).to_string().as_str(), 10).unwrap()
    }

    pub fn pow(x: BigInt, n: i32) -> BigInt {
        if n < 0 {
            panic!("The exponent must be greater than or equal to zero.")
        } else {
            x.pow(n as u32)
        }
    }

    pub fn modPow(x: BigInt, e: BigInt, m: BigInt) -> BigInt {
        x.modpow(&e, &m)
    }

    pub fn divRem(x: BigInt, y: BigInt) -> (BigInt, BigInt) {
        x.div_rem(&y)
    }

    pub fn greatestCommonDivisor(x: BigInt, y: BigInt) -> BigInt {
        x.gcd(&y)
    }

    pub fn clamp(x: BigInt, min: BigInt, max: BigInt) -> BigInt {
        x.clamp(min, max)
    }

    pub fn getBitLength(x: BigInt) -> i64 {
        x.bits() as i64
    }

    // pub fn copySign
    // pub fn createChecked
    // pub fn createSaturating
    // pub fn createTruncating
    // pub fn getByteCount
    // pub fn leadingZeroCount
    // pub fn log
    // pub fn log10
    // pub fn log2
    // pub fn maxMagnitude
    // pub fn minMagnitude
    // pub fn popCount
    // pub fn rotateLeft
    // pub fn rotateRight
    // pub fn trailingZeroCount
    // pub fn tryFormat
    // pub fn tryWriteBytes
}
