#[cfg(feature = "decimal")]
pub mod Decimal_ {
    use crate::Native_::{compare, format, hash_code, DivideByInt, Hashable, Lrc, MutCell, NullableRef, String};
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

    pub fn maxMagnitude(x: decimal, y: decimal) -> decimal {
        match abs(x).cmp(&abs(y)) {
            Ordering::Less => y,
            Ordering::Greater => x,
            Ordering::Equal => if x.is_sign_negative() { y } else { x },
        }
    }
    pub fn minMagnitude(x: decimal, y: decimal) -> decimal {
        match abs(x).cmp(&abs(y)) {
            Ordering::Less => x,
            Ordering::Greater => y,
            Ordering::Equal => if x.is_sign_negative() { x } else { y },
        }
    }

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
    pub fn negate(x: decimal) -> decimal { -x }

    pub fn isNegative(x: decimal) -> bool { x.is_sign_negative() }
    pub fn isPositive(x: decimal) -> bool { x.is_sign_positive() }
    pub fn isInteger(x: decimal) -> bool { x.scale() == 0 || x.fract() == Decimal::ZERO }
    pub fn isEvenInteger(x: decimal) -> bool { isInteger(x) && x.trunc().mantissa() % 2 == 0 }
    pub fn isOddInteger(x: decimal) -> bool { isInteger(x) && x.trunc().mantissa() % 2 != 0 }
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
        toString_1(&if x.is_zero() { x.abs() } else { x })
    }

    fn exponentialFormatString(x: decimal, precision: usize, lowercase: bool) -> String {
        if x.is_zero() {
            return format!(
                "{:.*}{}+000",
                precision,
                x.normalize(),
                if lowercase { 'e' } else { 'E' }
            );
        }

        let mantissa = x.mantissa().unsigned_abs();
        let digitCount = mantissa.ilog10() as usize + 1;
        let mut exponent = digitCount as i32 - x.scale() as i32 - 1;
        let significantDigits = precision.saturating_add(1);
        let rounded = if significantDigits < digitCount {
            let divisor = 10u128.pow((digitCount - significantDigits) as u32);
            let mut rounded = mantissa / divisor + u128::from(mantissa % divisor >= divisor / 2);
            if rounded.ilog10() as usize + 1 > significantDigits {
                rounded /= 10;
                exponent += 1;
            }
            rounded
        } else {
            mantissa
        };
        let digits = format!("{rounded}");
        let mut formatted = String::new();
        if x.is_sign_negative() {
            formatted.push('-');
        }
        formatted.push(digits.as_bytes()[0] as char);
        if precision > 0 {
            formatted.push('.');
            formatted.push_str(&digits[1..]);
            for _ in digits.len()..significantDigits {
                formatted.push('0');
            }
        }
        formatted.push(if lowercase { 'e' } else { 'E' });
        formatted.push(if exponent < 0 { '-' } else { '+' });
        formatted.push_str(&format!("{:03}", exponent.unsigned_abs()));
        formatted
    }

    fn exponentialFormat(x: decimal, precision: usize, lowercase: bool) -> string {
        fromString_1(exponentialFormatString(x, precision, lowercase))
    }

    fn generalFormat(x: decimal, precision: usize, lowercase: bool) -> string {
        if x.is_zero() {
            return toString_1(&Decimal::ZERO);
        }

        let normalized = x.abs().normalize();
        let mantissa = normalized.mantissa() as u128;
        let digitCount = mantissa.ilog10() + 1;
        let exponent = digitCount as i32 - normalized.scale() as i32 - 1;
        let rounded = if exponent < precision as i32 {
            let decimalPlaces = (precision as i32 - exponent - 1).clamp(0, 28) as u32;
            x.round_dp_with_strategy(decimalPlaces, RoundingStrategy::MidpointAwayFromZero).normalize()
        } else {
            x.normalize()
        };
        let magnitude = rounded.abs();
        let exponent = (magnitude.mantissa() as u128).ilog10() as i32 - magnitude.scale() as i32;

        if exponent >= precision as i32 || exponent < -4 {
            let mut formatted = exponentialFormatString(rounded, precision - 1, lowercase);
            if let Some(point) = formatted.find('.') {
                let exponentIndex = formatted.find(['e', 'E']).unwrap();
                let mut end = exponentIndex;
                while end > point + 1 && formatted.as_bytes()[end - 1] == b'0' {
                    end -= 1;
                }
                if end == point + 1 {
                    end = point;
                }
                formatted.replace_range(end..exponentIndex, "");
            }
            let exponentIndex = formatted.find(['e', 'E']).unwrap();
            if formatted.as_bytes().get(exponentIndex + 2) == Some(&b'0') {
                formatted.remove(exponentIndex + 2);
            }
            fromString_1(formatted)
        } else {
            toString_1(&rounded)
        }
    }

    pub fn toStringWithFormat(x: decimal, formatSpec: string) -> string {
        let formatSpec = formatSpec.as_str();
        let Some(token) = formatSpec.chars().next() else {
            return toString(x);
        };
        let specifiedPrecision = formatSpec
            .get(1..)
            .and_then(|value| value.parse::<usize>().ok());
        let precision = specifiedPrecision.unwrap_or(0);

        match token {
            'G' | 'g' if precision > 0 => generalFormat(x, precision, token == 'g'),
            'G' | 'g' | 'R' | 'r' => toString(x),
            'E' | 'e' => exponentialFormat(x, specifiedPrecision.unwrap_or(6), token == 'e'),
            'F' | 'f' => {
                let precision = specifiedPrecision.unwrap_or(2);
                let rounded = x.round_dp_with_strategy(precision.min(28) as u32, RoundingStrategy::MidpointAwayFromZero);
                let rounded = if rounded.is_zero() { rounded.abs() } else { rounded };
                fromString_1(format!("{:.*}", precision, rounded))
            },
            _ => toString(x),
        }
    }

    pub fn getTypeCode() -> i32 { 15 }

    // System.Globalization.NumberStyles
    #[allow(dead_code)]
    mod NumberStyles {
        pub const None: i32 = 0x000;
        pub const AllowLeadingWhite: i32 = 0x001;
        pub const AllowTrailingWhite: i32 = 0x002;
        pub const AllowLeadingSign: i32 = 0x004;
        pub const AllowTrailingSign: i32 = 0x008;
        pub const AllowParentheses: i32 = 0x010;
        pub const AllowDecimalPoint: i32 = 0x020;
        pub const AllowThousands: i32 = 0x040;
        pub const AllowExponent: i32 = 0x080;
        pub const AllowCurrencySymbol: i32 = 0x100;
        pub const AllowHexSpecifier: i32 = 0x200;
        pub const AllowBinarySpecifier: i32 = 0x400;

        pub const Integer: i32 = AllowLeadingWhite | AllowTrailingWhite | AllowLeadingSign;
        pub const HexNumber: i32 = AllowLeadingWhite | AllowTrailingWhite | AllowHexSpecifier;
        pub const BinaryNumber: i32 = AllowLeadingWhite | AllowTrailingWhite | AllowBinarySpecifier;
        pub const Number: i32 =
            AllowLeadingWhite
            | AllowTrailingWhite
            | AllowLeadingSign
            | AllowTrailingSign
            | AllowDecimalPoint
            | AllowThousands;
        pub const Float: i32 =
            AllowLeadingWhite | AllowTrailingWhite | AllowLeadingSign | AllowDecimalPoint | AllowExponent;
        pub const Currency: i32 =
            AllowLeadingWhite
            | AllowTrailingWhite
            | AllowLeadingSign
            | AllowTrailingSign
            | AllowParentheses
            | AllowDecimalPoint
            | AllowThousands
            | AllowCurrencySymbol;
        pub const Any: i32 = Currency | AllowExponent;
    }

    mod parsing {
        use super::decimal;
        use super::NumberStyles;
        use rust_decimal::Decimal;

        #[derive(Clone, Copy, PartialEq, Eq)]
        enum DecimalParseError {
            Format,
            Overflow,
        }

        fn parseFailure(error: DecimalParseError, value: &str) -> ! {
            match error {
                DecimalParseError::Format => panic!("The input string '{}' was not in a correct format.", value),
                DecimalParseError::Overflow => panic!("Value was either too large or too small for a Decimal."),
            }
        }

        pub(super) fn overflowFailure() -> ! { parseFailure(DecimalParseError::Overflow, "") }

        const MAX_MANTISSA: u128 = (1_u128 << 96) - 1;
        const SIGNIFICANT_WINDOW: usize = 30;
        const EXPONENT_LIMIT: i128 = 1_i128 << 62;

        // The significant digits of a number, without leading zeros. Parsing never needs more than the first
        // SIGNIFICANT_WINDOW of them: a mantissa has at most 29 digits and one more is needed for rounding.
        // `hasTail` records whether any later digit is non-zero, which is all the rounding needs from them.
        struct SignificantDigits {
            window: [u8; SIGNIFICANT_WINDOW],
            length: usize,
            hasTail: bool,
        }

        impl SignificantDigits {
            fn push(&mut self, digit: u8) {
                if self.length < SIGNIFICANT_WINDOW {
                    self.window[self.length] = digit;
                } else if digit != 0 {
                    self.hasTail = true;
                }
                self.length += 1;
            }

            // Index 0 is the first significant digit. Negative indexes are the zeros between the decimal point
            // and the first significant digit.
            fn digitAt(&self, index: i128) -> u128 {
                if index < 0 {
                    0
                } else {
                    u128::from(self.window[index as usize])
                }
            }

            // `start` is never past SIGNIFICANT_WINDOW, so every digit recorded in `hasTail` is at or after it.
            fn hasNonZeroFrom(&self, start: usize) -> bool {
                let stored = self.length.min(SIGNIFICANT_WINDOW);
                self.hasTail || self.window[start.min(stored)..stored].iter().any(|digit| *digit != 0)
            }
        }

        // Reads the mantissa and the exponent in a single pass. Returns the significant digits and the power of
        // ten of their last digit, so the value is digits * 10^power.
        fn scanNumber(
            value: &str,
            allowDecimalPoint: bool,
            allowThousands: bool,
            allowExponent: bool,
        ) -> Result<(SignificantDigits, i128), DecimalParseError> {
            let bytes = value.as_bytes();
            let mut digits = SignificantDigits {
                window: [0; SIGNIFICANT_WINDOW],
                length: 0,
                hasTail: false,
            };
            let mut digitCount = 0_usize;
            let mut fractionLength = 0_i128;
            let mut hasDecimalPoint = false;
            let mut hasIntegerDigit = false;
            let mut index = 0;
            while index < bytes.len() {
                match bytes[index] {
                    byte @ b'0'..=b'9' => {
                        digitCount += 1;
                        if hasDecimalPoint {
                            fractionLength += 1;
                        } else {
                            hasIntegerDigit = true;
                        }
                        if digits.length > 0 || byte != b'0' {
                            digits.push(byte - b'0');
                        }
                    }
                    b'.' if allowDecimalPoint && !hasDecimalPoint => hasDecimalPoint = true,
                    b',' if allowThousands && !hasDecimalPoint && hasIntegerDigit => {}
                    b'e' | b'E' => break,
                    _ => return Err(DecimalParseError::Format),
                }
                index += 1;
            }
            if digitCount == 0 {
                return Err(DecimalParseError::Format);
            }

            let exponent = if index < bytes.len() {
                if !allowExponent {
                    return Err(DecimalParseError::Format);
                }
                parseExponent(&bytes[index + 1..])?
            } else {
                0
            };
            Ok((digits, exponent - fractionLength))
        }

        // Parses the text after the exponent marker. Magnitudes are clamped to EXPONENT_LIMIT so the arithmetic
        // cannot overflow; any exponent beyond the clamp gives the same result.
        fn parseExponent(text: &[u8]) -> Result<i128, DecimalParseError> {
            let (isNegative, digits) = match text.first() {
                Some(b'-') => (true, &text[1..]),
                Some(b'+') => (false, &text[1..]),
                _ => (false, text),
            };
            if digits.is_empty() || !digits.iter().all(u8::is_ascii_digit) {
                return Err(DecimalParseError::Format);
            }
            let magnitude = digits.iter().fold(0_i128, |magnitude, digit| {
                (magnitude * 10 + i128::from(*digit - b'0')).min(EXPONENT_LIMIT)
            });
            Ok(if isNegative { -magnitude } else { magnitude })
        }

        fn fromMantissa(mantissa: u128, scale: u32, isNegative: bool) -> decimal {
            let mut parsed = Decimal::from_parts(
                mantissa as u32,
                (mantissa >> 32) as u32,
                (mantissa >> 64) as u32,
                isNegative,
                scale,
            );
            parsed.set_sign_negative(isNegative);
            parsed
        }

        // Rounds digits * 10^power to a Decimal with at most 28 fraction digits, rounding half to even.
        fn roundDigits(digits: &SignificantDigits, power: i128, isNegative: bool) -> Result<decimal, DecimalParseError> {
            if digits.length == 0 {
                return Ok(fromMantissa(0, (-power).clamp(0, 28) as u32, isNegative));
            }
            // Common case: every digit fits the mantissa and the scale, so nothing is rounded.
            if digits.length <= 28 && (-28..=0).contains(&power) {
                let mantissa = digits.window[..digits.length]
                    .iter()
                    .fold(0_u128, |mantissa, digit| mantissa * 10 + u128::from(*digit));
                return Ok(fromMantissa(mantissa, (-power) as u32, isNegative));
            }
            let position = digits.length as i128 + power;
            if position > 29 {
                return Err(DecimalParseError::Overflow);
            }
            if position < -28 {
                return Ok(fromMantissa(0, 28, isNegative));
            }
            // The mantissa takes the digits up to the decimal point (zeros past the last digit when power >= 0),
            // then as many fraction digits as still fit.
            let fractionLength = -power;
            let mut mantissa = 0_u128;
            if position > 0 {
                for index in 0..position {
                    mantissa = mantissa * 10 + digits.digitAt(index);
                }
                if mantissa > MAX_MANTISSA {
                    return Err(DecimalParseError::Overflow);
                }
            }
            let mut scale = 0_i128;
            while scale < fractionLength.min(28) {
                let next = mantissa * 10 + digits.digitAt(position + scale);
                if next > MAX_MANTISSA {
                    break;
                }
                mantissa = next;
                scale += 1;
            }

            let roundUp = scale < fractionLength
                && match digits.digitAt(position + scale) {
                    0..=4 => false,
                    5 => mantissa % 2 == 1 || digits.hasNonZeroFrom((position + scale + 1) as usize),
                    _ => true,
                };
            if !roundUp || mantissa < MAX_MANTISSA {
                return Ok(fromMantissa(mantissa + u128::from(roundUp), scale as u32, isNegative));
            }
            if scale == 0 {
                return Err(DecimalParseError::Overflow);
            }
            // Rounding up would exceed MAX_MANTISSA, so the result keeps one fraction digit fewer.
            Ok(fromMantissa(MAX_MANTISSA / 10 + 1, scale as u32 - 1, isNegative))
        }

        // Space and U+0009..U+000D, the whitespace .NET accepts around a number.
        fn isParseWhitespace(c: char) -> bool { matches!(c, ' ' | '\t'..='\r') }

        pub(super) fn validateStyle(style: i32) {
            if style & !NumberStyles::Any != 0 {
                panic!("An undefined NumberStyles value is being used.");
            }
        }

        fn parseDecimalWithStyle(value: &str, style: i32) -> Result<decimal, DecimalParseError> {
            validateStyle(style);
            let has = |flag: i32| style & flag != 0;
            let allowLeadingWhite = has(NumberStyles::AllowLeadingWhite);
            let allowTrailingWhite = has(NumberStyles::AllowTrailingWhite);
            let allowLeadingSign = has(NumberStyles::AllowLeadingSign);
            let allowTrailingSign = has(NumberStyles::AllowTrailingSign);
            let allowParentheses = has(NumberStyles::AllowParentheses);
            let allowCurrencySymbol = has(NumberStyles::AllowCurrencySymbol);

            // Disallowed whitespace is left in place and rejected by scanNumber.
            let mut value = value.trim_end_matches('\0');

            let mut isNegative = false;
            let mut hasSign = false;
            let mut hasParentheses = false;
            let mut hasCurrencySymbol = false;
            loop {
                if allowLeadingWhite && (!hasSign || hasCurrencySymbol) {
                    value = value.trim_start_matches(isParseWhitespace);
                }
                if allowLeadingSign && !hasSign && matches!(value.as_bytes().first(), Some(b'+' | b'-')) {
                    isNegative = value.starts_with('-');
                    hasSign = true;
                    value = &value[1..];
                } else if allowParentheses && !hasSign && value.starts_with('(') {
                    isNegative = true;
                    hasSign = true;
                    hasParentheses = true;
                    value = &value[1..];
                } else if allowCurrencySymbol && !hasCurrencySymbol && value.starts_with('\u{00a4}') {
                    hasCurrencySymbol = true;
                    value = &value['\u{00a4}'.len_utf8()..];
                } else {
                    break;
                }
            }

            loop {
                if allowTrailingWhite {
                    value = value.trim_end_matches(isParseWhitespace);
                }
                if allowTrailingSign && !hasSign && matches!(value.as_bytes().last(), Some(b'+' | b'-')) {
                    isNegative = value.ends_with('-');
                    hasSign = true;
                    value = &value[..value.len() - 1];
                } else if hasParentheses && value.ends_with(')') {
                    hasParentheses = false;
                    value = &value[..value.len() - 1];
                } else if allowCurrencySymbol && !hasCurrencySymbol && value.ends_with('\u{00a4}') {
                    hasCurrencySymbol = true;
                    value = &value[..value.len() - '\u{00a4}'.len_utf8()];
                } else {
                    break;
                }
            }
            if hasParentheses {
                return Err(DecimalParseError::Format);
            }

            let (digits, power) = scanNumber(
                value,
                has(NumberStyles::AllowDecimalPoint),
                has(NumberStyles::AllowThousands),
                has(NumberStyles::AllowExponent),
            )?;
            roundDigits(&digits, power, isNegative)
        }

        pub(super) fn tryParseDecimal(value: &str, style: i32) -> Option<decimal> {
            parseDecimalWithStyle(value, style).ok()
        }

        pub(super) fn parseDecimal(value: &str, style: i32) -> decimal {
            match parseDecimalWithStyle(value, style) {
                Ok(d) => d,
                Err(error) => parseFailure(error, value),
            }
        }

        // Parses the text produced by float formatting and drops insignificant trailing zeros.
        pub(super) fn fromFloatString(value: &str) -> decimal {
            let parsed = parseDecimal(value, NumberStyles::Float);
            if parsed.is_zero() { Decimal::ZERO } else { parsed.normalize() }
        }
    }

    pub fn tryParseWithStyle(s: string, style: i32, res: &MutCell<Decimal>) -> bool {
        if s.is_null() {
            parsing::validateStyle(style);
            res.set(Decimal::ZERO);
            return false;
        }
        match parsing::tryParseDecimal(&s, style) {
            Some(d) => { res.set(d); true },
            None => { res.set(Decimal::ZERO); false },
        }
    }

    pub fn tryParse(s: string, res: &MutCell<Decimal>) -> bool {
        tryParseWithStyle(s, NumberStyles::Number, res)
    }

    pub fn parseWithStyle(s: string, style: i32) -> decimal {
        if s.is_null() { panic!("Value cannot be null."); }
        parsing::parseDecimal(&s, style)
    }

    pub fn parse(s: string) -> decimal {
        parseWithStyle(s, NumberStyles::Number)
    }

    pub fn convert(s: string) -> decimal {
        if s.is_null() { Decimal::ZERO } else { parse(s) }
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

    pub fn fromFloat32(n: f32) -> decimal {
        if !n.is_finite() {
            parsing::overflowFailure();
        }
        parsing::fromFloatString(&format!("{:.6e}", n))
    }

    pub fn fromFloat64(n: f64) -> decimal {
        if !n.is_finite() {
            parsing::overflowFailure();
        }
        parsing::fromFloatString(&format!("{:.14e}", n))
    }

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

    // F# `decimal` on strings uses NumberStyles.Float by default.
    pub fn fromString(s: string) -> decimal {
        parseWithStyle(s, NumberStyles::Float)
    }

    pub fn fromParts(low: i32, mid: i32, high: i32, isNegative: bool, scale: u8) -> decimal {
        assert!(scale <= 28, "Invalid decimal scale");
        let mut value = Decimal::from_parts(low as u32, mid as u32, high as u32, isNegative, scale as u32);
        value.set_sign_negative(isNegative);
        value
    }

    pub fn fromInts(low: i32, mid: i32, high: i32, signExp: i32) -> decimal {
        assert!(signExp as u32 & 0x7F00_FFFF == 0, "Invalid decimal flags");
        let isNegative = signExp < 0;
        let scale = ((signExp >> 16) & 0xFF) as u8;
        fromParts(low, mid, high, isNegative, scale)
    }

    pub fn fromIntArray(bits: Array<i32>) -> decimal {
        assert!(bits.len() == 4, "Invalid decimal bits length");
        fromInts(bits[0], bits[1], bits[2], bits[3])
    }

    pub fn getBits(x: decimal) -> Array<i32> {
        let du = x.unpack();
        let low = du.lo as i32;
        let mid = du.mid as i32;
        let high = du.hi as i32;
        let scale = du.scale as i32;
        let signExp = (scale << 16) | if du.negative { i32::MIN } else { 0 };
        new_array(&[low, mid, high, signExp])
    }

    pub fn round(x: decimal) -> decimal { x.round() }

    pub fn roundTo(x: decimal, dp: i32) -> decimal {
        assert!((0..=28).contains(&dp), "Invalid decimal precision");
        x.round_dp(dp as u32)
    }

    pub mod MidpointRounding {
        pub const ToEven: i32 = 0;
        pub const AwayFromZero: i32 = 1;
        pub const ToZero: i32 = 2;
        pub const ToNegativeInfinity: i32 = 3;
        pub const ToPositiveInfinity: i32 = 4;
    }

    pub fn roundToMode(x: decimal, dp: i32, mode: i32) -> decimal {
        assert!((0..=28).contains(&dp), "Invalid decimal precision");
        let strategy = match mode {
            MidpointRounding::ToEven => RoundingStrategy::MidpointNearestEven,
            MidpointRounding::AwayFromZero => RoundingStrategy::MidpointAwayFromZero,
            MidpointRounding::ToZero => RoundingStrategy::ToZero,
            MidpointRounding::ToNegativeInfinity => RoundingStrategy::ToNegativeInfinity,
            MidpointRounding::ToPositiveInfinity => RoundingStrategy::ToPositiveInfinity,
            _ => panic!("Invalid decimal rounding mode"),
        };
        x.round_dp_with_strategy(dp as u32, strategy)
    }

    pub fn roundMode(x: decimal, mode: i32) -> decimal {
        roundToMode(x, 0, mode)
    }

    // pub fn createChecked
    // pub fn createSaturating
    // pub fn createTruncating
    // pub fn tryFormat
    // pub fn tryGetBits
}
