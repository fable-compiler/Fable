#[cfg(feature = "decimal")]
pub mod Decimal_ {
    use crate::Native_::{compare, format, hash_code, DivideByInt, Hashable, Lrc, MutCell, String};
    use crate::NativeArray_::{new_array, Array};
    use crate::String_::{fromString as fromString_1, string, toString as toString_1};
    use core::cmp::Ordering;

    use rust_decimal::prelude::*;
    pub use rust_decimal::Decimal as decimal;

    pub const Zero: decimal = Decimal::ZERO;
    pub const One: decimal = Decimal::ONE;
    pub const MinusOne: decimal = Decimal::NEGATIVE_ONE;
    pub const MaxValue: decimal = Decimal::MAX;
    pub const MinValue: decimal = Decimal::MIN;

    const NUMBER_STYLE_NUMBER: i32 = 0x6F;
    const NUMBER_STYLE_FLOAT: i32 = 0xA7;

    impl DivideByInt for decimal {
        #[inline]
        fn divide_by_int(self, rhs: i32) -> Self {
            self / Decimal::from_i32(rhs).unwrap()
        }
    }

    impl Hashable for decimal {
        #[inline]
        fn getHashCode(&self) -> i32 {
            hash_code(self)
        }
    }

    // pub fn getHashCode(x: decimal) -> i32 { hash_code(&x) }

    pub fn equals(x: decimal, y: decimal) -> bool { x.eq(&y) }
    pub fn compareTo(x: decimal, y: decimal) -> i32 { compare(&x, &y) }

    pub fn scale(x: decimal) -> u8 { x.scale() as u8 }

    pub fn abs(x: decimal) -> decimal { x.abs() }
    pub fn sign(x: decimal) -> i32 { x.signum().to_i32().unwrap() }

    pub fn copySign(x: decimal, sign: decimal) -> decimal {
        if x.is_sign_negative() == sign.is_sign_negative() { x } else { -x }
    }

    pub fn max(x: decimal, y: decimal) -> decimal { if x < y { y } else { x } }
    pub fn min(x: decimal, y: decimal) -> decimal { if x < y { x } else { y } }

    pub fn maxMagnitude(x: decimal, y: decimal) -> decimal { if abs(x) < abs(y) {y} else {x} }
    pub fn minMagnitude(x: decimal, y: decimal) -> decimal { if abs(x) < abs(y) {x} else {y} }

    pub fn clamp(x: decimal, min: decimal, max: decimal) -> decimal {
        x.clamp(min, max).into()
    }

    pub fn floor(x: decimal) -> decimal { x.floor() }
    pub fn ceiling(x: decimal) -> decimal { x.ceil() }
    pub fn truncate(x: decimal) -> decimal { x.trunc() }
    pub fn pown(x: decimal, n: i32) -> decimal { x.powi(n as i64) }

    #[inline]
    fn preserveScale(mut value: decimal, scale: u32) -> decimal {
        if value.scale() < scale {
            value.rescale(scale);
        }
        value
    }

    pub fn add(x: decimal, y: decimal) -> decimal {
        let value = x.checked_add(y).unwrap_or_else(|| panic!(""));
        preserveScale(value, x.scale().max(y.scale()))
    }
    pub fn subtract(x: decimal, y: decimal) -> decimal {
        let value = x.checked_sub(y).unwrap_or_else(|| panic!(""));
        preserveScale(value, x.scale().max(y.scale()))
    }
    pub fn multiply(x: decimal, y: decimal) -> decimal {
        let value = x.checked_mul(y).unwrap_or_else(|| panic!(""));
        preserveScale(value, (x.scale() + y.scale()).min(28))
    }
    pub fn divide(x: decimal, y: decimal) -> decimal {
        let quotient = x.checked_div(y).unwrap_or_else(|| panic!(""));
        if quotient.checked_mul(y) == Some(x) {
            let quotient = quotient.normalize();
            let scale = quotient.scale().max(x.scale().saturating_sub(y.scale()));
            preserveScale(quotient, scale)
        } else {
            quotient
        }
    }
    pub fn remainder(x: decimal, y: decimal) -> decimal { x % y }
    pub fn negate(x: decimal) -> decimal { if x.is_zero() { x.abs() } else { -x } }

    pub fn isNegative(x: decimal) -> bool { x.is_sign_negative() }
    pub fn isPositive(x: decimal) -> bool { x.is_sign_positive() }
    pub fn isInteger(x: decimal) -> bool { x.fract() == Decimal::ZERO }
    pub fn isEvenInteger(x: decimal) -> bool { isInteger(x) && (x % Decimal::from(2)) == Decimal::ZERO }
    pub fn isOddInteger(x: decimal) -> bool { isInteger(x) && (x % Decimal::from(2)) != Decimal::ZERO }
    // Canonical means the stored scale is already minimal (no representable trailing
    // zero digits), e.g. 1.0M and 1.20M are non-canonical, but 1M and 3.14M are.
    pub fn isCanonical(x: decimal) -> bool { x.scale() == x.normalize().scale() }

    pub fn toInt8(x: decimal) -> i8 { x.trunc().to_i8().unwrap() }
    pub fn toUInt8(x: decimal) -> u8 { x.trunc().normalize().to_u8().unwrap() }
    pub fn toInt16(x: decimal) -> i16 { x.trunc().to_i16().unwrap() }
    pub fn toUInt16(x: decimal) -> u16 { x.trunc().normalize().to_u16().unwrap() }
    pub fn toInt32(x: decimal) -> i32 { x.trunc().to_i32().unwrap() }
    pub fn toUInt32(x: decimal) -> u32 { x.trunc().normalize().to_u32().unwrap() }
    pub fn toInt64(x: decimal) -> i64 { x.trunc().to_i64().unwrap() }
    pub fn toUInt64(x: decimal) -> u64 { x.trunc().normalize().to_u64().unwrap() }
    pub fn toNativeInt(x: decimal) -> isize { x.trunc().to_isize().unwrap() }
    pub fn toUNativeInt(x: decimal) -> usize { x.trunc().normalize().to_usize().unwrap() }

    pub fn toFloat32(x: decimal) -> f32 { x.to_f32().unwrap() }
    pub fn toFloat64(x: decimal) -> f64 { x.to_f64().unwrap() }

    pub fn toDecimal(x: decimal) -> decimal { x }

    pub fn toOACurrency(x: decimal) -> i64 {
        let scaled = x * Decimal::from_i32(10000).unwrap();
        scaled
            .round_dp_with_strategy(0, RoundingStrategy::MidpointNearestEven)
            .to_i64()
            .unwrap()
    }

    pub fn toBoolean(x: decimal) -> bool { !x.is_zero() }

    pub fn toChar(x: decimal) -> char {
        core::char::from_u32(x.to_u32().unwrap()).unwrap()
    }

    pub fn toString(x: decimal) -> string {
        toString_1(&x)
    }

    fn padExponent(mut value: String, minimumDigits: usize) -> String {
        let Some(index) = value.find('E').or_else(|| value.find('e')) else {
            return value;
        };
        let exponentStart = index + 1;
        let signLength = match value.as_bytes().get(exponentStart) {
            Some(&b'+') | Some(&b'-') => 1,
            _ => 0,
        };
        let digitCount = value.len() - exponentStart - signLength;
        let zeroCount = minimumDigits.saturating_sub(digitCount);
        let signCount = 1 - signLength;
        value.reserve(zeroCount + signCount);
        if signCount > 0 {
            value.insert(exponentStart, '+');
        }
        let digitsStart = exponentStart + 1;
        for _ in 0..zeroCount {
            value.insert(digitsStart, '0');
        }
        value
    }

    fn exponentialFormat(x: decimal, precision: usize, lowercase: bool) -> string {
        if x.is_zero() {
            let formatted = format!(
                "{:.*}{}+000",
                precision,
                x.normalize(),
                if lowercase { 'e' } else { 'E' }
            );
            return fromString_1(formatted);
        }

        let formatted = if lowercase {
            format!("{:.*e}", precision, x)
        } else {
            format!("{:.*E}", precision, x)
        };
        fromString_1(padExponent(formatted, 3))
    }

    fn generalFormat(x: decimal, precision: usize, lowercase: bool) -> string {
        if x.is_zero() {
            return toString_1(&Decimal::ZERO);
        }

        let normalized = x.abs().normalize();
        let mantissa = normalized.mantissa() as u128;
        let digitCount = mantissa.ilog10() + 1;
        let exponent = digitCount as i32 - normalized.scale() as i32 - 1;

        if exponent >= precision as i32 || exponent < -4 {
            let formatted = if lowercase {
                format!("{:.*e}", precision - 1, x)
            } else {
                format!("{:.*E}", precision - 1, x)
            };
            fromString_1(padExponent(formatted, 2))
        } else {
            let decimalPlaces = (precision as i32 - exponent - 1).min(28) as u32;
            let rounded = x
                .round_dp_with_strategy(decimalPlaces, RoundingStrategy::MidpointAwayFromZero)
                .normalize();
            toString_1(&rounded)
        }
    }

    pub fn toStringWithFormat(x: decimal, formatSpec: string) -> string {
        let formatSpec = formatSpec.as_str();
        let Some(token) = formatSpec.chars().next() else {
            return toString(x);
        };
        let precision = formatSpec
            .get(1..)
            .and_then(|value| value.parse::<usize>().ok())
            .filter(|value| *value > 0)
            .unwrap_or(0);

        match token {
            'G' | 'g' if precision > 0 => generalFormat(x, precision, token == 'g'),
            'G' | 'g' | 'R' | 'r' => toString(x),
            'E' | 'e' => exponentialFormat(x, if precision > 0 { precision } else { 6 }, token == 'e'),
            'F' | 'f' => fromString_1(format!("{:.*}", if precision > 0 { precision } else { 2 }, x)),
            _ => toString(x),
        }
    }

    pub fn getTypeCode() -> i32 { 15 }

    fn incrementDigits(digits: &mut String) {
        let mut carry = true;
        let mut index = digits.len();
        while index > 0 && carry {
            index -= 1;
            let digit = digits.as_bytes()[index];
            if digit == b'9' {
                digits.replace_range(index..index + 1, "0");
            } else {
                let nextDigit = [digit + 1];
                digits.replace_range(
                    index..index + 1,
                    core::str::from_utf8(&nextDigit).unwrap(),
                );
                carry = false;
            }
        }
        if carry {
            digits.insert(0, '1');
        }
    }

    fn roundInputToScale28(value: &str) -> Option<decimal> {
        let bytes = value.as_bytes();
        let signLength = match bytes.first() {
            Some(b'+' | b'-') => 1,
            _ => 0,
        };
        if signLength == bytes.len() {
            return None;
        }

        let decimalIndex = value.find('.').unwrap_or(bytes.len());
        let integerDigits = &bytes[signLength..decimalIndex];
        let fraction = if decimalIndex < bytes.len() {
            &bytes[decimalIndex + 1..]
        } else {
            &bytes[bytes.len()..]
        };
        if !bytes.iter().enumerate().all(|(index, byte)| {
            (index == 0 && signLength == 1 && (*byte == b'+' || *byte == b'-'))
                || (index == decimalIndex && decimalIndex < bytes.len() && *byte == b'.')
                || (index != decimalIndex && byte.is_ascii_digit())
        }) || (integerDigits.is_empty() && fraction.is_empty())
        {
            return None;
        }

        let integerLength = integerDigits.len().max(1);
        let maxScale = fraction.len().min(28);
        let isNegative = bytes.first() == Some(&b'-');

        for scale in (0..=maxScale).rev() {
            let discarded = &fraction[scale..];
            let mut digits = String::with_capacity(integerLength + scale + 1);
            if integerDigits.is_empty() {
                digits.push('0');
            } else {
                digits.push_str(core::str::from_utf8(integerDigits).ok()?);
            }
            digits.push_str(core::str::from_utf8(&fraction[..scale]).ok()?);

            let roundUp = match discarded.first() {
                Some(b'0'..=b'4') | None => false,
                Some(b'6'..=b'9') => true,
                Some(b'5') => {
                    discarded[1..].iter().any(|digit| *digit != b'0')
                        || digits.as_bytes().last().unwrap_or(&b'0') % 2 == 1
                }
                _ => return None,
            };
            let originalLength = digits.len();
            if roundUp {
                incrementDigits(&mut digits);
            }

            let roundedIntegerLength = integerLength + digits.len().saturating_sub(originalLength);
            let mut candidate = String::with_capacity(digits.len() + 2);
            if isNegative {
                candidate.push('-');
            }
            candidate.push_str(&digits[..roundedIntegerLength]);
            if scale > 0 {
                candidate.push('.');
                candidate.push_str(&digits[roundedIntegerLength..]);
            }

            if let Ok(parsed) = Decimal::from_str(&candidate) {
                return Some(if parsed.is_zero() { parsed.abs() } else { parsed });
            }
        }
        None
    }

    fn parseDecimal(value: &str) -> Option<decimal> {
        roundInputToScale28(value)
    }

    fn expandExponent(value: &str, exponent: i32) -> Option<String> {
        if exponent > 1000 {
            return None;
        }
        if exponent < -1000 {
            return Some(String::from("0"));
        }

        let signLength = usize::from(value.starts_with('-'));
        let unsigned = &value[signLength..];
        let integerLength = unsigned.find('.').unwrap_or(unsigned.len());
        let digits: String = unsigned.chars().filter(|digit| *digit != '.').collect();
        let decimalPosition = integerLength as i32 + exponent;
        let mut expanded = String::with_capacity(digits.len() + exponent.unsigned_abs() as usize + 2);

        if signLength > 0 {
            expanded.push('-');
        }
        if decimalPosition <= 0 {
            expanded.push('0');
            expanded.push('.');
            for _ in 0..decimalPosition.unsigned_abs() {
                expanded.push('0');
            }
            expanded.push_str(&digits);
        } else if decimalPosition as usize >= digits.len() {
            expanded.push_str(&digits);
            for _ in digits.len()..decimalPosition as usize {
                expanded.push('0');
            }
        } else {
            let decimalPosition = decimalPosition as usize;
            expanded.push_str(&digits[..decimalPosition]);
            expanded.push('.');
            expanded.push_str(&digits[decimalPosition..]);
        }

        Some(expanded)
    }

    fn parseDecimalWithStyle(value: &str, style: i32) -> Option<decimal> {
        let allowLeadingWhite = style & 0x01 != 0;
        let allowTrailingWhite = style & 0x02 != 0;
        let allowLeadingSign = style & 0x04 != 0;
        let allowTrailingSign = style & 0x08 != 0;
        let allowParentheses = style & 0x10 != 0;
        let allowDecimalPoint = style & 0x20 != 0;
        let allowThousands = style & 0x40 != 0;
        let allowExponent = style & 0x80 != 0;
        let allowCurrencySymbol = style & 0x100 != 0;

        let mut value = value;
        if allowLeadingWhite {
            value = value.trim_start();
        } else if value.chars().next().is_some_and(char::is_whitespace) {
            return None;
        }
        if allowTrailingWhite {
            value = value.trim_end();
        } else if value.chars().last().is_some_and(char::is_whitespace) {
            return None;
        }
        if value.is_empty() {
            return None;
        }

        let mut isNegative = false;
        if value.starts_with('(') || value.ends_with(')') {
            if !allowParentheses || !value.starts_with('(') || !value.ends_with(')') {
                return None;
            }
            value = &value[1..value.len() - 1];
            isNegative = true;
        }

        if allowCurrencySymbol {
            if let Some(stripped) = value.strip_prefix('$') {
                value = stripped;
            } else if let Some(stripped) = value.strip_suffix('$') {
                value = stripped;
            }
        }

        let leadingSign = value.as_bytes().first().copied();
        if matches!(leadingSign, Some(b'+' | b'-')) {
            if !allowLeadingSign || isNegative {
                return None;
            }
            isNegative = leadingSign == Some(b'-');
            value = &value[1..];
        } else if matches!(value.as_bytes().last(), Some(b'+' | b'-')) {
            if !allowTrailingSign || isNegative {
                return None;
            }
            isNegative = value.as_bytes().last() == Some(&b'-');
            value = &value[..value.len() - 1];
        }

        let (mantissa, exponent) = match value.find(|character| character == 'e' || character == 'E') {
            Some(index) => {
                if !allowExponent || value[index + 1..].contains(['e', 'E']) {
                    return None;
                }
                let exponentText = &value[index + 1..];
                let exponentDigits = exponentText.strip_prefix('+').or_else(|| exponentText.strip_prefix('-')).unwrap_or(exponentText);
                if exponentDigits.is_empty() || !exponentDigits.bytes().all(|digit| digit.is_ascii_digit()) {
                    return None;
                }
                ( &value[..index], exponentText.parse::<i32>().ok()? )
            }
            None => (value, 0),
        };

        let bytes = mantissa.as_bytes();
        let mut integerDigits = String::new();
        let mut fractionDigits = String::new();
        let mut hasDecimalPoint = false;
        let mut digitCount = 0;

        for (index, byte) in bytes.iter().copied().enumerate() {
            match byte {
                b'0'..=b'9' => {
                    digitCount += 1;
                    if hasDecimalPoint {
                        fractionDigits.push(byte as char);
                    } else {
                        integerDigits.push(byte as char);
                    }
                }
                b'.' if allowDecimalPoint && !hasDecimalPoint => hasDecimalPoint = true,
                b',' if allowThousands
                    && !hasDecimalPoint
                    && index > 0
                    && index + 1 < bytes.len()
                    && bytes[index - 1].is_ascii_digit()
                    && bytes[index + 1].is_ascii_digit() => {}
                _ => return None,
            }
        }

        if digitCount == 0 {
            return None;
        }
        if integerDigits.is_empty() {
            integerDigits.push('0');
        }

        let mut normalized = String::new();
        if isNegative {
            normalized.push('-');
        }
        normalized.push_str(&integerDigits);
        if !fractionDigits.is_empty() {
            normalized.push('.');
            normalized.push_str(&fractionDigits);
        }

        let normalized = expandExponent(&normalized, exponent)?;
        parseDecimal(&normalized)
    }

    pub fn tryParseWithStyle(s: string, style: i32, res: &MutCell<Decimal>) -> bool {
        match parseDecimalWithStyle(&s, style) {
            Some(d) => { res.set(d); true },
            None => false,
        }
    }

    pub fn tryParse(s: string, res: &MutCell<Decimal>) -> bool {
        tryParseWithStyle(s, NUMBER_STYLE_NUMBER, res)
    }

    pub fn parseWithStyle(s: string, style: i32) -> decimal {
        match parseDecimalWithStyle(&s, style) {
            Some(d) => d,
            None => panic!(""),
        }
    }

    pub fn parse(s: string) -> decimal { parseWithStyle(s, NUMBER_STYLE_NUMBER) }

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

    fn fromFloatString(value: &str) -> decimal {
        match parseDecimalWithStyle(value, NUMBER_STYLE_FLOAT) {
            Some(value) if value.is_zero() => Decimal::ZERO,
            Some(value) => value.normalize(),
            None => panic!(""),
        }
    }

    pub fn fromFloat32(n: f32) -> decimal { fromFloatString(&format!("{:.6e}", n)) }
    pub fn fromFloat64(n: f64) -> decimal { fromFloatString(&format!("{:.14e}", n)) }

    pub fn fromDecimal(d: decimal) -> decimal { d }

    pub fn fromOACurrency(value: i64) -> decimal {
        Decimal::from_i64(value).unwrap() / Decimal::from_i32(10000).unwrap()
    }

    pub fn fromBoolean(b: bool) -> decimal {
        Decimal::from_u32(b as u32).unwrap()
    }

    pub fn fromChar(c: char) -> decimal {
        Decimal::from_u32(c as u32).unwrap()
    }

    pub fn fromString(s: string) -> decimal {
        parseWithStyle(s, NUMBER_STYLE_NUMBER)
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

    // pub fn createChecked
    // pub fn createSaturating
    // pub fn createTruncating
    // pub fn tryFormat
    // pub fn tryGetBits
}
