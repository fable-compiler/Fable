pub mod Char_ {
    use crate::NativeArray_::{array_from, Array};
    use crate::Native_::{compare, MutCell, ToString};
    use crate::String_::{getCharAt, length, string, toString};

    // https://docs.microsoft.com/en-us/dotnet/api/system.globalization.unicodecategory?view=net-6.0
    pub mod UnicodeCategory {
        pub const UppercaseLetter: u8 = 0;
        pub const LowercaseLetter: u8 = 1;
        pub const TitlecaseLetter: u8 = 2;
        pub const ModifierLetter: u8 = 3;
        pub const OtherLetter: u8 = 4;
        pub const NonSpacingMark: u8 = 5;
        pub const SpacingCombiningMark: u8 = 6;
        pub const EnclosingMark: u8 = 7;
        pub const DecimalDigitNumber: u8 = 8;
        pub const LetterNumber: u8 = 9;
        pub const OtherNumber: u8 = 10;
        pub const SpaceSeparator: u8 = 11;
        pub const LineSeparator: u8 = 12;
        pub const ParagraphSeparator: u8 = 13;
        pub const Control: u8 = 14;
        pub const Format: u8 = 15;
        pub const Surrogate: u8 = 16;
        pub const PrivateUse: u8 = 17;
        pub const ConnectorPunctuation: u8 = 18;
        pub const DashPunctuation: u8 = 19;
        pub const OpenPunctuation: u8 = 20;
        pub const ClosePunctuation: u8 = 21;
        pub const InitialQuotePunctuation: u8 = 22;
        pub const FinalQuotePunctuation: u8 = 23;
        pub const OtherPunctuation: u8 = 24;
        pub const MathSymbol: u8 = 25;
        pub const CurrencySymbol: u8 = 26;
        pub const ModifierSymbol: u8 = 27;
        pub const OtherSymbol: u8 = 28;
        pub const OtherNotAssigned: u8 = 29;
    }

    // The maximum character value.
    pub const MaxValue: char = '\u{FFFF}';
    // The minimum character value.
    pub const MinValue: char = '\u{0000}';

    // const IsWhiteSpaceFlag: u8 = 0x80;
    // const IsUpperCaseLetterFlag: u8 = 0x40;
    // const IsLowerCaseLetterFlag: u8 = 0x20;
    const UnicodeCategoryMask: u8 = 0x1F;

    // const isControlMask: u32 = 0 | 1 << UnicodeCategory::Control;
    // const isDigitMask: u32 = 0 | 1 << UnicodeCategory::DecimalDigitNumber;
    // const isLetterMask: u32 = 0
    //     | 1 << UnicodeCategory::UppercaseLetter
    //     | 1 << UnicodeCategory::LowercaseLetter
    //     | 1 << UnicodeCategory::TitlecaseLetter
    //     | 1 << UnicodeCategory::ModifierLetter
    //     | 1 << UnicodeCategory::OtherLetter;
    // const isLetterOrDigitMask: u32 = 0 | isLetterMask | isDigitMask;
    // const isUpperMask: u32 = 0 | 1 << UnicodeCategory::UppercaseLetter;
    // const isLowerMask: u32 = 0 | 1 << UnicodeCategory::LowercaseLetter;
    // const isNumberMask: u32 = 0
    //     | 1 << UnicodeCategory::DecimalDigitNumber
    //     | 1 << UnicodeCategory::LetterNumber
    //     | 1 << UnicodeCategory::OtherNumber;
    // const isPunctuationMask: u32 = 0
    //     | 1 << UnicodeCategory::ConnectorPunctuation
    //     | 1 << UnicodeCategory::DashPunctuation
    //     | 1 << UnicodeCategory::OpenPunctuation
    //     | 1 << UnicodeCategory::ClosePunctuation
    //     | 1 << UnicodeCategory::InitialQuotePunctuation
    //     | 1 << UnicodeCategory::FinalQuotePunctuation
    //     | 1 << UnicodeCategory::OtherPunctuation;
    // const isSeparatorMask: u32 = 0
    //     | 1 << UnicodeCategory::SpaceSeparator
    //     | 1 << UnicodeCategory::LineSeparator
    //     | 1 << UnicodeCategory::ParagraphSeparator;
    // const isSymbolMask: u32 = 0
    //     | 1 << UnicodeCategory::MathSymbol
    //     | 1 << UnicodeCategory::CurrencySymbol
    //     | 1 << UnicodeCategory::ModifierSymbol
    //     | 1 << UnicodeCategory::OtherSymbol;
    // const isWhiteSpaceMask: u32 = 0
    //     | 1 << UnicodeCategory::SpaceSeparator
    //     | 1 << UnicodeCategory::LineSeparator
    //     | 1 << UnicodeCategory::ParagraphSeparator;

    // Contains information about the C0, Basic Latin, C1, and Latin-1 Supplement ranges [ U+0000..U+00FF ], with:
    // - 0x80 bit if set means 'is whitespace'
    // - 0x40 bit if set means 'is uppercase letter'
    // - 0x20 bit if set means 'is lowercase letter'
    // - bottom 5 bits are the of: UnicodeCategory the character
    #[cfg_attr(rustfmt, rustfmt::skip)]
    const Latin1CharInfo: &[u8; 256] = &[
        // 0     1     2     3     4     5     6     7     8     9     A     B     C     D     E     F
        0x0E, 0x0E, 0x0E, 0x0E, 0x0E, 0x0E, 0x0E, 0x0E, 0x0E, 0x8E, 0x8E, 0x8E, 0x8E, 0x8E, 0x0E, 0x0E, // U+0000..U+000F
        0x0E, 0x0E, 0x0E, 0x0E, 0x0E, 0x0E, 0x0E, 0x0E, 0x0E, 0x0E, 0x0E, 0x0E, 0x0E, 0x0E, 0x0E, 0x0E, // U+0010..U+001F
        0x8B, 0x18, 0x18, 0x18, 0x1A, 0x18, 0x18, 0x18, 0x14, 0x15, 0x18, 0x19, 0x18, 0x13, 0x18, 0x18, // U+0020..U+002F
        0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x18, 0x18, 0x19, 0x19, 0x19, 0x18, // U+0030..U+003F
        0x18, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, // U+0040..U+004F
        0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x14, 0x18, 0x15, 0x1B, 0x12, // U+0050..U+005F
        0x1B, 0x21, 0x21, 0x21, 0x21, 0x21, 0x21, 0x21, 0x21, 0x21, 0x21, 0x21, 0x21, 0x21, 0x21, 0x21, // U+0060..U+006F
        0x21, 0x21, 0x21, 0x21, 0x21, 0x21, 0x21, 0x21, 0x21, 0x21, 0x21, 0x14, 0x19, 0x15, 0x19, 0x0E, // U+0070..U+007F
        0x0E, 0x0E, 0x0E, 0x0E, 0x0E, 0x8E, 0x0E, 0x0E, 0x0E, 0x0E, 0x0E, 0x0E, 0x0E, 0x0E, 0x0E, 0x0E, // U+0080..U+008F
        0x0E, 0x0E, 0x0E, 0x0E, 0x0E, 0x0E, 0x0E, 0x0E, 0x0E, 0x0E, 0x0E, 0x0E, 0x0E, 0x0E, 0x0E, 0x0E, // U+0090..U+009F
        0x8B, 0x18, 0x1A, 0x1A, 0x1A, 0x1A, 0x1C, 0x18, 0x1B, 0x1C, 0x04, 0x16, 0x19, 0x0F, 0x1C, 0x1B, // U+00A0..U+00AF
        0x1C, 0x19, 0x0A, 0x0A, 0x1B, 0x21, 0x18, 0x18, 0x1B, 0x0A, 0x04, 0x17, 0x0A, 0x0A, 0x0A, 0x18, // U+00B0..U+00BF
        0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, // U+00C0..U+00CF
        0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x19, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x21, // U+00D0..U+00DF
        0x21, 0x21, 0x21, 0x21, 0x21, 0x21, 0x21, 0x21, 0x21, 0x21, 0x21, 0x21, 0x21, 0x21, 0x21, 0x21, // U+00E0..U+00EF
        0x21, 0x21, 0x21, 0x21, 0x21, 0x21, 0x21, 0x19, 0x21, 0x21, 0x21, 0x21, 0x21, 0x21, 0x21, 0x21, // U+00F0..U+00FF
    ];

    // true for all characters below or equal U+00ff, which is ASCII + Latin-1 Supplement.
    #[inline]
    fn isLatin1(c: char) -> bool {
        c as u32 <= 0xFF
    }

    // fn isUnicodeCategory(c: char, uc_mask: u32) -> bool {
    //     let uc = GetUnicodeCategory(c);
    //     ((1 << uc) & uc_mask) != 0
    // }

    pub fn GetUnicodeCategory(c: char) -> i32 {
        let category = if (isLatin1(c)) {
            (Latin1CharInfo[c as usize] & UnicodeCategoryMask)
        } else {
            match c {
                // very incomplete, TODO: get real unicode categories
                c if IsUpper(c) => UnicodeCategory::UppercaseLetter,
                c if IsLower(c) => UnicodeCategory::LowercaseLetter,
                c if IsLetter(c) => UnicodeCategory::OtherLetter,
                c if IsDigit(c) => UnicodeCategory::DecimalDigitNumber,
                c if IsControl(c) => UnicodeCategory::Control,
                c if IsSurrogate(c) => UnicodeCategory::Surrogate,
                c => UnicodeCategory::OtherNotAssigned,
            }
        };
        category as i32
    }

    pub fn GetUnicodeCategory_2(s: string, index: i32) -> i32 {
        let c: char = getCharAt(s, index);
        GetUnicodeCategory(c)
    }

    pub fn GetNumericValue(c: char) -> f64 {
        match c.to_digit(10) {
            Some(d) => d as f64,
            None => -1.0,
        }
    }

    pub fn GetNumericValue_2(s: string, index: i32) -> f64 {
        let c: char = getCharAt(s, index);
        GetNumericValue(c)
    }

    pub fn fromCharCode(code: u32) -> char {
        // unsafe { char::from_u32_unchecked(code) }
        char::from_u32(code).unwrap()
    }

    pub fn ConvertFromUtf32(utf32: i32) -> string {
        let c: char = char::from_u32(utf32 as u32).unwrap();
        toString(c)
    }

    pub fn ConvertToUtf32_2(s: string, index: i32) -> i32 {
        let c: char = getCharAt(s, index);
        c as i32
    }

    pub fn GetHashCode(c: char) -> i32 {
        // Calculate a hashcode for a 2 byte Unicode character.
        // c as i32 | ((c as i32) << 16)
        c as i32
    }

    pub fn Equals(c: char, v: char) -> bool {
        c == v
    }

    pub fn CompareTo(c: char, value: char) -> i32 {
        c as i32 - value as i32
    }

    pub fn ToString(c: char) -> string {
        toString(c)
    }

    pub fn Parse(s: string) -> char {
        if (length(s.clone()) != 1) {
            panic!("Input must be a single-character string");
        }
        getCharAt(s, 0)
    }

    pub fn TryParse(s: string, result: &MutCell<char>) -> bool {
        if (length(s.clone()) != 1) {
            result.set(char::default());
            false
        } else {
            result.set(getCharAt(s, 0));
            true
        }
    }

    pub fn IsBetween(c: char, minInclusive: char, maxInclusive: char) -> bool {
        (c as u32 >= minInclusive as u32) && (c as u32 <= maxInclusive as u32)
    }

    // ----------------------------------------------------

    pub fn IsAscii(c: char) -> bool {
        // c as u32 <= 0x7F
        c.is_ascii()
    }

    pub fn IsAsciiDigit(c: char) -> bool {
        // matches!(c, '0'..='9')
        c.is_ascii_digit()
    }

    pub fn IsAsciiLetter(c: char) -> bool {
        // matches!(c, 'A'..='Z') | matches!(c, 'a'..='z')
        c.is_ascii_alphabetic()
    }

    pub fn IsAsciiLetterLower(c: char) -> bool {
        // matches!(c, 'a'..='z')
        c.is_ascii_lowercase()
    }

    pub fn IsAsciiLetterUpper(c: char) -> bool {
        // matches!(c, 'A'..='Z')
        c.is_ascii_uppercase()
    }

    pub fn IsAsciiLetterOrDigit(c: char) -> bool {
        // matches!(c, 'A'..='Z') | matches!(c, 'a'..='z') | matches!(c, '0'..='9')
        c.is_ascii_alphanumeric()
    }

    pub fn IsAsciiHexDigit(c: char) -> bool {
        // matches!(c, '0'..='9') | matches!(c, 'A'..='F') | matches!(c, 'a'..='f')
        c.is_ascii_hexdigit()
    }

    pub fn IsAsciiHexDigitLower(c: char) -> bool {
        // matches!(c, '0'..='9') | matches!(c, 'a'..='f')
        c.is_ascii_hexdigit() && c.is_ascii_lowercase()
    }

    pub fn IsAsciiHexDigitUpper(c: char) -> bool {
        // matches!(c, '0'..='9') | matches!(c, 'A'..='F')
        c.is_ascii_hexdigit() && c.is_ascii_uppercase()
    }

    pub fn IsControl(c: char) -> bool {
        // isUnicodeCategory(c, isControlMask)
        c.is_control()
    }

    pub fn IsDigit(c: char) -> bool {
        // if (IsLatin1(c)) {
        //     matches!(c, '0'..='9')
        // } else {
        //     isUnicodeCategory(c, isDigitMask)
        // }
        c.is_ascii_digit()
    }

    pub fn IsLetter(c: char) -> bool {
        // if (IsAscii(c)) {
        //     // For the version of the Unicode standard the type: char is locked to, the
        //     // ASCII range doesn't include letters in categories other than "upper" and "lower".
        //     (Latin1CharInfo[c as usize] & (IsUpperCaseLetterFlag | IsLowerCaseLetterFlag)) != 0
        // } else {
        //     isUnicodeCategory(c, isLetterMask)
        // }
        c.is_alphabetic()
    }

    pub fn IsLetterOrDigit(c: char) -> bool {
        // isUnicodeCategory(c, LetterOrDigitMask)
        c.is_ascii_digit() || c.is_alphabetic()
    }

    pub fn IsLower(c: char) -> bool {
        // if (IsLatin1(c)) {
        //     (Latin1CharInfo[c as usize] & IsLowerCaseLetterFlag) != 0
        // } else {
        //     isUnicodeCategory(c, isLowerMask)
        // }
        c.is_lowercase()
    }

    pub fn IsUpper(c: char) -> bool {
        // if (IsLatin1(c)) {
        //     (Latin1CharInfo[c as usize] & IsUpperCaseLetterFlag) != 0
        // } else {
        //     isUnicodeCategory(c, isUpperMask)
        // }
        c.is_uppercase()
    }

    pub fn IsNumber(c: char) -> bool {
        // if (IsAscii(c)) {
        //     matches!(c, '0'..='9')
        // } else {
        //     isUnicodeCategory(c, isNumberMask)
        // }
        c.is_numeric()
    }

    pub fn IsPunctuation(c: char) -> bool {
        if (IsAscii(c)) {
            c.is_ascii_punctuation()
        } else {
            c.is_ascii_punctuation() //TODO: imprecise, fix this
                                     // isUnicodeCategory(c, isPunctuationMask)
        }
    }

    pub fn IsSeparator(c: char) -> bool {
        if (isLatin1(c)) {
            c == '\u{0020}' || c == '\u{00a0}'
        } else {
            // isUnicodeCategory(c, isSeparatorMask)
            c.is_whitespace() //TODO: imprecise, fix this
        }
    }

    pub fn IsSymbol(c: char) -> bool {
        // isUnicodeCategory(c, isSymbolMask)
        c.is_ascii_punctuation() //TODO: imprecise, fix this
    }

    pub fn IsWhiteSpace(c: char) -> bool {
        // if (IsLatin1(c)) {
        //     (Latin1CharInfo[c as usize] & IsWhiteSpaceFlag) != 0
        // } else {
        //     isUnicodeCategory(c, isWhiteSpaceMask)
        // }
        c.is_whitespace()
    }

    // ----------------------------------------------------

    pub fn IsControl_2(s: string, index: i32) -> bool {
        let c: char = getCharAt(s, index);
        IsControl(c)
    }

    pub fn IsDigit_2(s: string, index: i32) -> bool {
        let c: char = getCharAt(s, index);
        IsDigit(c)
    }

    pub fn IsLetter_2(s: string, index: i32) -> bool {
        let c: char = getCharAt(s, index);
        IsLetter(c)
    }

    pub fn IsLetterOrDigit_2(s: string, index: i32) -> bool {
        let c: char = getCharAt(s, index);
        IsLetterOrDigit(c)
    }

    pub fn IsLower_2(s: string, index: i32) -> bool {
        let c: char = getCharAt(s, index);
        IsLower(c)
    }

    pub fn IsUpper_2(s: string, index: i32) -> bool {
        let c: char = getCharAt(s, index);
        IsUpper(c)
    }

    pub fn IsNumber_2(s: string, index: i32) -> bool {
        let c: char = getCharAt(s, index);
        IsNumber(c)
    }

    pub fn IsPunctuation_2(s: string, index: i32) -> bool {
        let c: char = getCharAt(s, index);
        IsPunctuation(c)
    }

    pub fn IsSeparator_2(s: string, index: i32) -> bool {
        let c: char = getCharAt(s, index);
        IsSeparator(c)
    }

    pub fn IsSymbol_2(s: string, index: i32) -> bool {
        let c: char = getCharAt(s, index);
        IsSymbol(c)
    }

    pub fn IsWhiteSpace_2(s: string, index: i32) -> bool {
        let c: char = getCharAt(s, index);
        IsWhiteSpace(c)
    }

    // ----------------------------------------------------

    pub fn ToUpper(c: char) -> char {
        if (IsAscii(c)) {
            c.to_ascii_uppercase()
        } else {
            //TODO: when Uppercase is more than one character
            c.to_uppercase().next().unwrap()
        }
    }

    pub fn ToUpperInvariant(c: char) -> char {
        ToUpper(c) //TODO: use invariant culture
    }

    pub fn ToLower(c: char) -> char {
        if (IsAscii(c)) {
            c.to_ascii_lowercase()
        } else {
            //TODO: when Lowercase is more than one character
            c.to_lowercase().next().unwrap()
        }
    }

    pub fn ToLowerInvariant(c: char) -> char {
        ToLower(c) //TODO: use invariant culture
    }

    // ----------------------------------------------------
    // Rust chars are Unicode scalar values, so surrogate tests will be false
    // ----------------------------------------------------

    pub fn IsSurrogate(c: char) -> bool {
        c as u32 >= 0xD800 && c as u32 <= 0xDFFF
    }

    pub fn IsSurrogate_2(s: string, index: i32) -> bool {
        let c: char = getCharAt(s, index);
        IsSurrogate(c)
    }

    pub fn IsHighSurrogate(c: char) -> bool {
        c as u32 >= 0xD800 && c as u32 <= 0xDBFF
    }

    pub fn IsHighSurrogate_2(s: string, index: i32) -> bool {
        let c: char = getCharAt(s, index);
        IsHighSurrogate(c)
    }

    pub fn IsLowSurrogate(c: char) -> bool {
        c as u32 >= 0xDC00 && c as u32 <= 0xDFFF
    }

    pub fn IsLowSurrogate_2(s: string, index: i32) -> bool {
        let c: char = getCharAt(s, index);
        IsLowSurrogate(c)
    }

    pub fn IsSurrogatePair(c1: char, c2: char) -> bool {
        IsHighSurrogate(c1) && IsLowSurrogate(c2)
    }

    pub fn IsSurrogatePair_2(s: string, index: i32) -> bool {
        let c1: char = getCharAt(s.clone(), index);
        if (index + 1 < length(s.clone())) {
            let c2: char = getCharAt(s.clone(), index + 1);
            IsSurrogatePair(c1, c2)
        } else {
            false
        }
    }
}
