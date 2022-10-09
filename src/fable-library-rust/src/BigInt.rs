#[cfg(feature = "bigint")]
pub mod BigInt_ {
    use crate::Decimal_::{decimal, truncate};
    use crate::Native_::{arrayFrom, Array, Lrc, MutCell, Vec};
    use crate::String_::{string, toString as toString_1};
    use core::cmp::Ordering;

    use num_bigint::*;
    use num_integer::*;
    use num_traits::*;

    #[repr(transparent)]
    #[derive(Clone, Debug, Default, Eq, Hash, PartialEq, PartialOrd, Ord)]
    pub struct bigint(Lrc<BigInt>);

    impl From<BigInt> for bigint {
        fn from(x: BigInt) -> bigint {
            bigint(Lrc::from(x))
        }
    }

    impl core::ops::Deref for bigint {
        type Target = BigInt;
        #[inline]
        fn deref(&self) -> &Self::Target {
            self.0.as_ref()
        }
    }

    impl core::fmt::Display for bigint {
        fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
            write!(f, "{}", self.0.as_ref()) //TODO:
        }
    }

    macro_rules! un_op {
        ($op_name:ident, $op_fn:ident, $op:tt) => {
            impl core::ops::$op_name for bigint {
                type Output = Self;
                #[inline]
                fn $op_fn(self) -> Self { ($op self.0.as_ref()).into() }
            }
        };
    }

    macro_rules! bin_op {
        ($op_name:ident, $op_fn:ident, $op:tt) => {
            impl core::ops::$op_name for bigint {
                type Output = Self;
                #[inline]
                fn $op_fn(self, rhs: Self) -> Self { (self.0.as_ref() $op rhs.0.as_ref()).into() }
            }
        };
    }

    macro_rules! shift_op {
        ($op_name:ident, $op_fn:ident, $op:tt) => {
            impl core::ops::$op_name<i32> for bigint {
                type Output = Self;
                #[inline]
                fn $op_fn(self, rhs: i32) -> Self { (self.0.as_ref() $op rhs).into() }
            }
        };
    }

    un_op!(Neg, neg, -);
    un_op!(Not, not, !);

    bin_op!(Add, add, +);
    bin_op!(Sub, sub, -);
    bin_op!(Mul, mul, *);
    bin_op!(Div, div, /);
    bin_op!(Rem, rem, %);
    bin_op!(BitAnd, bitand, &);
    bin_op!(BitOr, bitor, |);
    bin_op!(BitXor, bitxor, ^);

    shift_op!(Shl, shl, <<);
    shift_op!(Shr, shr, >>);

    pub fn zero() -> bigint { BigInt::zero().into() }
    pub fn one() -> bigint { BigInt::one().into() }
    pub fn minusOne() -> bigint { (-BigInt::one()).into() }

    // pub fn getHashCode(x: bigint) = x.GetHashCode()
    pub fn equals(x: bigint, y: bigint) -> bool { x.eq(&y) }
    pub fn compare(x: bigint, y: bigint) -> i32 {
        match x.cmp(&y) {
            Ordering::Less => -1,
            Ordering::Greater => 1,
            Ordering::Equal => 0,
        }
    }

    pub fn abs(x: bigint) -> bigint { x.abs().into() }
    pub fn sign(x: bigint) -> i32 { x.signum().to_i32().unwrap() }

    pub fn add(x: bigint, y: bigint) -> bigint { x + y }
    pub fn subtract(x: bigint, y: bigint) -> bigint { x - y }
    pub fn multiply(x: bigint, y: bigint) -> bigint { x * y }
    pub fn divide(x: bigint, y: bigint) -> bigint { x / y }
    pub fn remainder(x: bigint, y: bigint) -> bigint { x % y }
    pub fn negate(x: bigint) -> bigint { -x }

    pub fn max(x: bigint, y: bigint) -> bigint { x.max(y).into() }
    pub fn min(x: bigint, y: bigint) -> bigint { x.min(y).into() }

    pub fn isZero(x: bigint) -> bool { x.is_zero() }
    pub fn isOne(x: bigint) -> bool { x.is_one() }
    pub fn isEven(x: bigint) -> bool { x.is_even() }
    pub fn isOdd(x: bigint) -> bool { x.is_odd() }
    // pub fn IsPowerOfTwo(x: bigint) -> bool { false } //TODO:

    pub fn isNegative(x: bigint) -> bool { x.is_negative() }
    pub fn isPositive(x: bigint) -> bool { x.is_positive() }
    pub fn isEvenInteger(x: bigint) -> bool { x.is_even() } //TODO:
    pub fn isOddInteger(x: bigint) -> bool { x.is_odd() } //TODO:
    // pub fn isPow2(x: bigint) -> bool { false } //TODO:

    pub fn toInt8 (x: bigint) -> i8 { x.to_i8().unwrap() }
    pub fn toUInt8 (x: bigint) -> u8 { x.to_u8().unwrap() }
    pub fn toInt16 (x: bigint) -> i16 { x.to_i16().unwrap() }
    pub fn toUInt16 (x: bigint) -> u16 { x.to_u16().unwrap() }
    pub fn toInt32 (x: bigint) -> i32 { x.to_i32().unwrap() }
    pub fn toUInt32 (x: bigint) -> u32 { x.to_u32().unwrap() }
    pub fn toInt64 (x: bigint) -> i64 { x.to_i64().unwrap() }
    pub fn toUInt64 (x: bigint) -> u64 { x.to_u64().unwrap() }
    pub fn toNativeInt(x: bigint) -> isize { x.to_isize().unwrap() }
    pub fn toUNativeInt(x: bigint) -> usize { x.to_usize().unwrap() }
    pub fn toFloat32 (x: bigint) -> f32 { x.to_f32().unwrap() }
    pub fn toFloat64 (x: bigint) -> f64 { x.to_f64().unwrap() }

    pub fn toDecimal (x: bigint) -> decimal {
        decimal::from_str_radix(x.to_string().as_str(), 10).unwrap()
    }
    pub fn toBoolean (x: bigint) -> bool { !x.is_zero() }

    pub fn toChar (x: bigint) -> char {
        core::char::from_u32(x.to_u32().unwrap()).unwrap()
    }

    pub fn toString (x: bigint) -> string {
        toString_1(&x)
    }

    pub fn toByteArray(x: bigint) -> Array<u8> {
        arrayFrom(x.to_signed_bytes_le())
    }

    pub fn tryParse(s: string, res: &MutCell<bigint>) -> bool {
        match BigInt::from_str_radix(s.as_ref(), 10) {
            Ok(d) => { res.set(d.into()); true },
            Err(e) => false,
        }
    }

    pub fn parse(s: string) -> bigint {
        match BigInt::from_str_radix(s.as_ref(), 10) {
            Ok(d) => d.into(),
            Err(e) => panic!("Input string was not in a correct format."),
        }
    }

    pub fn fromZero() -> bigint { BigInt::zero().into() }
    pub fn fromOne() -> bigint { BigInt::one().into() }

    pub fn fromString(s: string) -> bigint {
        BigInt::from_str_radix(s.as_ref(), 10).unwrap().into()
    }

    pub fn fromByteArray(bytes: Array<u8>) -> bigint {
        BigInt::from_signed_bytes_le(bytes.as_ref()).into()
    }

    pub fn fromInt8(n: i8) -> bigint { BigInt::from_i8(n).unwrap().into() }
    pub fn fromUInt8(n: u8) -> bigint { BigInt::from_u8(n).unwrap().into() }
    pub fn fromInt16(n: i16) -> bigint { BigInt::from_i16(n).unwrap().into() }
    pub fn fromUInt16(n: u16) -> bigint { BigInt::from_u16(n).unwrap().into() }
    pub fn fromInt32(n: i32) -> bigint { BigInt::from_i32(n).unwrap().into() }
    pub fn fromUInt32(n: u32) -> bigint { BigInt::from_u32(n).unwrap().into() }
    pub fn fromInt64(n: i64) -> bigint { BigInt::from_i64(n).unwrap().into() }
    pub fn fromUInt64(n: u64) -> bigint { BigInt::from_u64(n).unwrap().into() }
    pub fn fromFloat32(n: f32) -> bigint { BigInt::from_f32(n).unwrap().into() }
    pub fn fromFloat64(n: f64) -> bigint { BigInt::from_f64(n).unwrap().into() }
    pub fn fromNativeInt(n: isize) -> bigint { BigInt::from_isize(n).unwrap().into() }
    pub fn fromUNativeInt(n: usize) -> bigint { BigInt::from_usize(n).unwrap().into() }

    pub fn fromDecimal(d: decimal) -> bigint {
        BigInt::from_str_radix(truncate(d).to_string().as_str(), 10).unwrap().into()
    }

    pub fn pow(x: bigint, n: i32) -> bigint {
        if n < 0 {
            panic!("The exponent must be greater than or equal to zero.")
        } else {
            x.0.as_ref().pow(n as u32).into()
        }
    }

    pub fn modPow(x: bigint, e: bigint, m: bigint) -> bigint {
        x.modpow(&e, &m).into()
    }

    pub fn divRem(x: bigint, y: bigint) -> (bigint, bigint) {
        let (div, rem) = x.div_rem(&y);
        (div.into(), rem.into())
    }

    pub fn divRemOut(x: bigint, y: bigint, remainder: &MutCell<bigint>) -> bigint {
        let (div, rem) = x.div_rem(&y);
        remainder.set(rem.into());
        div.into()
    }

    pub fn greatestCommonDivisor(x: bigint, y: bigint) -> bigint {
        x.gcd(&y).into()
    }

    pub fn clamp(x: bigint, min: bigint, max: bigint) -> bigint {
        x.clamp(min, max).into()
    }

    pub fn getBitLength(x: bigint) -> i64 {
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
