pub mod Rune_ {
    use crate::Char_::{
        GetNumericValue, GetUnicodeCategory, IsControl, IsDigit, IsLetter, IsLetterOrDigit,
        IsLower, IsNumber, IsPunctuation, IsSeparator, IsSymbol, IsUpper, IsWhiteSpace,
        ToLower, ToLowerInvariant, ToUpper, ToUpperInvariant,
    };
    use crate::Native_::MutCell;
    use crate::String_::{ofChar, string};

    pub fn zero() -> char {
        '\u{0000}'
    }

    pub fn replacementChar() -> char {
        '\u{FFFD}'
    }

    fn isValidScalar(value: u32) -> bool {
        value <= 0x10FFFF && !(0xD800..=0xDFFF).contains(&value)
    }

    fn newScalar(value: u32) -> char {
        if isValidScalar(value) {
            char::from_u32(value).unwrap()
        } else {
            panic!("Value is not a valid Unicode scalar value")
        }
    }

    pub fn newInt(value: i32) -> char {
        if value < 0 {
            panic!("Value is not a valid Unicode scalar value")
        }
        newScalar(value as u32)
    }

    pub fn newUInt(value: u32) -> char {
        newScalar(value)
    }

    pub fn newPair(highSurrogate: char, lowSurrogate: char) -> char {
        let high = highSurrogate as u32;
        let low = lowSurrogate as u32;
        if (0xD800..=0xDBFF).contains(&high) && (0xDC00..=0xDFFF).contains(&low) {
            let value = 0x10000 + ((high - 0xD800) << 10) + (low - 0xDC00);
            newScalar(value)
        } else {
            panic!("The values do not form a valid UTF-16 surrogate pair")
        }
    }

    pub fn tryCreateChar(value: char, result: &MutCell<char>) -> bool {
        result.set(value);
        true
    }

    pub fn tryCreatePair(highSurrogate: char, lowSurrogate: char, result: &MutCell<char>) -> bool {
        let high = highSurrogate as u32;
        let low = lowSurrogate as u32;
        if (0xD800..=0xDBFF).contains(&high) && (0xDC00..=0xDFFF).contains(&low) {
            result.set(newScalar(0x10000 + ((high - 0xD800) << 10) + (low - 0xDC00)));
            true
        } else {
            result.set(zero());
            false
        }
    }

    pub fn tryCreateInt(value: i32, result: &MutCell<char>) -> bool {
        if value >= 0 && isValidScalar(value as u32) {
            result.set(char::from_u32(value as u32).unwrap());
            true
        } else {
            result.set(zero());
            false
        }
    }

    pub fn tryCreateUInt(value: u32, result: &MutCell<char>) -> bool {
        if isValidScalar(value) {
            result.set(char::from_u32(value).unwrap());
            true
        } else {
            result.set(zero());
            false
        }
    }

    pub fn value(value: char) -> i32 {
        value as i32
    }

    pub fn utf8SequenceLength(value: char) -> i32 {
        value.len_utf8() as i32
    }

    pub fn utf16SequenceLength(value: char) -> i32 {
        value.len_utf16() as i32
    }

    pub fn isAscii(value: char) -> bool {
        value.is_ascii()
    }

    pub fn isBmp(value: char) -> bool {
        value as u32 <= 0xFFFF
    }

    pub fn plane(value: char) -> i32 {
        (value as u32 >> 16) as i32
    }

    pub fn getNumericValue(value: char) -> f64 {
        GetNumericValue(value)
    }

    pub fn getUnicodeCategory(value: char) -> i32 {
        GetUnicodeCategory(value)
    }

    pub fn isControl(value: char) -> bool {
        IsControl(value)
    }

    pub fn isDigit(value: char) -> bool {
        IsDigit(value)
    }

    pub fn isLetter(value: char) -> bool {
        IsLetter(value)
    }

    pub fn isLetterOrDigit(value: char) -> bool {
        IsLetterOrDigit(value)
    }

    pub fn isLower(value: char) -> bool {
        IsLower(value)
    }

    pub fn isNumber(value: char) -> bool {
        IsNumber(value)
    }

    pub fn isPunctuation(value: char) -> bool {
        IsPunctuation(value)
    }

    pub fn isSeparator(value: char) -> bool {
        IsSeparator(value)
    }

    pub fn isSymbol(value: char) -> bool {
        IsSymbol(value)
    }

    pub fn isUpper(value: char) -> bool {
        IsUpper(value)
    }

    pub fn isWhiteSpace(value: char) -> bool {
        IsWhiteSpace(value)
    }

    pub fn isValidInt(value: i32) -> bool {
        value >= 0 && isValidScalar(value as u32)
    }

    pub fn isValidUInt(value: u32) -> bool {
        isValidScalar(value)
    }

    pub fn toLower(value: char) -> char {
        ToLower(value)
    }

    pub fn toLowerInvariant(value: char) -> char {
        ToLowerInvariant(value)
    }

    pub fn toUpper(value: char) -> char {
        ToUpper(value)
    }

    pub fn toUpperInvariant(value: char) -> char {
        ToUpperInvariant(value)
    }

    fn tryGetRune(input: &string, index: i32) -> Option<char> {
        if index < 0 {
            return None;
        }

        let target = index as usize;
        let mut offset = 0;
        for value in input.as_str().chars() {
            if target == offset {
                return Some(value);
            }

            offset += value.len_utf16();
            if target < offset {
                return None;
            }
        }

        None
    }

    pub fn getRuneAt(input: string, index: i32) -> char {
        match tryGetRune(&input, index) {
            Some(value) => value,
            None => panic!("Cannot extract a Unicode scalar value from the specified index")
        }
    }

    pub fn tryGetRuneAt(input: string, index: i32, result: &MutCell<char>) -> bool {
        if index < 0 {
            panic!("Index was out of range")
        }

        let target = index as usize;
        let mut offset = 0;
        for value in input.as_str().chars() {
            if target == offset {
                result.set(value);
                return true;
            }

            offset += value.len_utf16();
            if target < offset {
                result.set(zero());
                return false;
            }
        }

        panic!("Index was out of range")
    }

    pub fn parse(input: string) -> char {
        let mut chars = input.as_str().chars();

        match (chars.next(), chars.next()) {
            (Some(value), None) => value,
            _ => panic!("Input must contain exactly one Unicode scalar value"),
        }
    }

    pub fn tryParse(input: string, result: &MutCell<char>) -> bool {
        let mut chars = input.as_str().chars();

        match (chars.next(), chars.next()) {
            (Some(value), None) => {
                result.set(value);
                true
            }
            _ => {
                result.set(zero());
                false
            }
        }
    }

    pub fn toString(value: char) -> string {
        ofChar(value)
    }

    pub fn compareTo(left: char, right: char) -> i32 {
        match left.cmp(&right) {
            core::cmp::Ordering::Less => -1,
            core::cmp::Ordering::Equal => 0,
            core::cmp::Ordering::Greater => 1,
        }
    }

    pub fn equals(left: char, right: char) -> bool {
        left == right
    }

    pub fn getHashCode(value: char) -> i32 {
        value as i32
    }
}
