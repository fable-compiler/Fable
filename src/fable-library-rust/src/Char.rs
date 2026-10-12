pub mod Char_ {
    use crate::NativeArray_::{array_from, Array};
    use crate::Native_::{compare, MutCell, ToString};
    use crate::String_::{getCharAt, length, string, toString};
    #[cfg(feature = "unicode")]
    use unicode_general_category::{get_general_category, GeneralCategory};

    // https://docs.microsoft.com/en-us/dotnet/api/system.globalization.unicodecategory
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    #[repr(u8)]
    pub enum UnicodeCategory {
        UppercaseLetter = 0,
        LowercaseLetter = 1,
        TitlecaseLetter = 2,
        ModifierLetter = 3,
        OtherLetter = 4,
        NonSpacingMark = 5,
        SpacingCombiningMark = 6,
        EnclosingMark = 7,
        DecimalDigitNumber = 8,
        LetterNumber = 9,
        OtherNumber = 10,
        SpaceSeparator = 11,
        LineSeparator = 12,
        ParagraphSeparator = 13,
        Control = 14,
        Format = 15,
        Surrogate = 16,
        PrivateUse = 17,
        ConnectorPunctuation = 18,
        DashPunctuation = 19,
        OpenPunctuation = 20,
        ClosePunctuation = 21,
        InitialQuotePunctuation = 22,
        FinalQuotePunctuation = 23,
        OtherPunctuation = 24,
        MathSymbol = 25,
        CurrencySymbol = 26,
        ModifierSymbol = 27,
        OtherSymbol = 28,
        OtherNotAssigned = 29,
    }

    impl core::fmt::Display for UnicodeCategory {
        fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
            write!(f, "{:?}", self)
        }
    }

    // map GeneralCategory to UnicodeCategory
    const UnicodeCategoryMap: [UnicodeCategory; 30] = [
        UnicodeCategory::ClosePunctuation,
        UnicodeCategory::ConnectorPunctuation,
        UnicodeCategory::Control,
        UnicodeCategory::CurrencySymbol,
        UnicodeCategory::DashPunctuation,
        UnicodeCategory::DecimalDigitNumber,
        UnicodeCategory::EnclosingMark,
        UnicodeCategory::FinalQuotePunctuation,
        UnicodeCategory::Format,
        UnicodeCategory::InitialQuotePunctuation,
        UnicodeCategory::LetterNumber,
        UnicodeCategory::LineSeparator,
        UnicodeCategory::LowercaseLetter,
        UnicodeCategory::MathSymbol,
        UnicodeCategory::ModifierLetter,
        UnicodeCategory::ModifierSymbol,
        UnicodeCategory::NonSpacingMark,
        UnicodeCategory::OpenPunctuation,
        UnicodeCategory::OtherLetter,
        UnicodeCategory::OtherNumber,
        UnicodeCategory::OtherPunctuation,
        UnicodeCategory::OtherSymbol,
        UnicodeCategory::ParagraphSeparator,
        UnicodeCategory::PrivateUse,
        UnicodeCategory::SpaceSeparator,
        UnicodeCategory::SpacingCombiningMark,
        UnicodeCategory::Surrogate,
        UnicodeCategory::TitlecaseLetter,
        UnicodeCategory::OtherNotAssigned,
        UnicodeCategory::UppercaseLetter,
    ];

    // The maximum character value.
    pub const MaxValue: char = '\u{FFFF}';
    // The minimum character value.
    pub const MinValue: char = '\u{0000}';

    const IsWhiteSpaceFlag: u8 = 0x80;
    const IsUpperCaseLetterFlag: u8 = 0x40;
    const IsLowerCaseLetterFlag: u8 = 0x20;
    const UnicodeCategoryMask: u8 = 0x1F;

    const isControlMask: u32 = 0 | 1 << UnicodeCategory::Control as u8;
    const isDigitMask: u32 = 0 | 1 << UnicodeCategory::DecimalDigitNumber as u8;
    const isLetterMask: u32 = 0
        | 1 << UnicodeCategory::UppercaseLetter as u8
        | 1 << UnicodeCategory::LowercaseLetter as u8
        | 1 << UnicodeCategory::TitlecaseLetter as u8
        | 1 << UnicodeCategory::ModifierLetter as u8
        | 1 << UnicodeCategory::OtherLetter as u8;
    const isLetterOrDigitMask: u32 = 0 | isLetterMask | isDigitMask;
    const isUpperMask: u32 = 0 | 1 << UnicodeCategory::UppercaseLetter as u8;
    const isLowerMask: u32 = 0 | 1 << UnicodeCategory::LowercaseLetter as u8;
    const isNumberMask: u32 = 0
        | 1 << UnicodeCategory::DecimalDigitNumber as u8
        | 1 << UnicodeCategory::LetterNumber as u8
        | 1 << UnicodeCategory::OtherNumber as u8;
    const isPunctuationMask: u32 = 0
        | 1 << UnicodeCategory::ConnectorPunctuation as u8
        | 1 << UnicodeCategory::DashPunctuation as u8
        | 1 << UnicodeCategory::OpenPunctuation as u8
        | 1 << UnicodeCategory::ClosePunctuation as u8
        | 1 << UnicodeCategory::InitialQuotePunctuation as u8
        | 1 << UnicodeCategory::FinalQuotePunctuation as u8
        | 1 << UnicodeCategory::OtherPunctuation as u8;
    const isSeparatorMask: u32 = 0
        | 1 << UnicodeCategory::SpaceSeparator as u8
        | 1 << UnicodeCategory::LineSeparator as u8
        | 1 << UnicodeCategory::ParagraphSeparator as u8;
    const isSymbolMask: u32 = 0
        | 1 << UnicodeCategory::MathSymbol as u8
        | 1 << UnicodeCategory::CurrencySymbol as u8
        | 1 << UnicodeCategory::ModifierSymbol as u8
        | 1 << UnicodeCategory::OtherSymbol as u8;
    const isWhiteSpaceMask: u32 = 0
        | 1 << UnicodeCategory::SpaceSeparator as u8
        | 1 << UnicodeCategory::LineSeparator as u8
        | 1 << UnicodeCategory::ParagraphSeparator as u8;

    // Contains information about the C0, Basic Latin, C1, and Latin-1 Supplement ranges [ U+0000..U+00FF ], with:
    // - 0x80 bit if set means 'is whitespace'
    // - 0x40 bit if set means 'is uppercase letter'
    // - 0x20 bit if set means 'is lowercase letter'
    // - bottom 5 bits are the of: UnicodeCategory the character
    #[cfg(not(feature = "unicode"))]
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

    #[inline]
    fn isUnicodeCategory(c: char, uc_mask: u32) -> bool {
        ((1u32 << (GetUnicodeCategory(c) as u32)) & uc_mask) != 0
    }

    #[cfg(feature = "unicode")]
    pub fn GetUnicodeCategory(c: char) -> i32 {
        let category = get_general_category(c);
        UnicodeCategoryMap[category as usize] as i32
    }

    #[cfg(not(feature = "unicode"))]
    pub fn GetUnicodeCategory(c: char) -> i32 {
        if (isLatin1(c)) {
            // complete, but only for Latin1 char set
            let cat = (Latin1CharInfo[c as usize] & UnicodeCategoryMask);
            // let uc: UnicodeCategory = unsafe { std::mem::transmute(cat) };
            cat as i32
        } else {
            (match c {
                // very incomplete, enable the unicode feature to get all unicode categories
                c if c.is_uppercase() => UnicodeCategory::UppercaseLetter,
                c if c.is_lowercase() => UnicodeCategory::LowercaseLetter,
                c if c.is_alphabetic() => match c {
                    c if c.is_numeric() => UnicodeCategory::LetterNumber,
                    _ => UnicodeCategory::OtherLetter,
                },
                c if c.is_ascii_digit() => UnicodeCategory::DecimalDigitNumber, // incomplete
                c if c.is_ascii_punctuation() => UnicodeCategory::ConnectorPunctuation, // incomplete
                c if c.is_numeric() => UnicodeCategory::OtherNumber,
                c if c.is_control() => UnicodeCategory::Control,
                c if c.is_whitespace() => match c {
                    // only whitespace characters that are not control chars
                    '\u{2028}' => UnicodeCategory::LineSeparator,
                    '\u{2029}' => UnicodeCategory::ParagraphSeparator,
                    _ => UnicodeCategory::SpaceSeparator,
                },
                c if IsSurrogate(c) => UnicodeCategory::Surrogate,
                // other categories are incomplete
                c => UnicodeCategory::OtherNotAssigned,
            }) as i32
        }
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

    pub fn ConvertToUtf32(c1: char, c2: char) -> i32 {
        let first = c1 as u32;
        let second = c2 as u32;
        if (0xD800..=0xDBFF).contains(&first) && (0xDC00..=0xDFFF).contains(&second) {
            (0x10000 + ((first - 0xD800) << 10) + (second - 0xDC00)) as i32
        } else {
            panic!("The values do not form a valid UTF-16 surrogate pair")
        }
    }

    pub fn GetHashCode(c: char) -> i32 {
        // Calculate a hashcode for a 2 byte Unicode character.
        // c as i32 | ((c as i32) << 16)
        c as i32
    }

    pub fn GetTypeCode(_c: char) -> i32 {
        4 // TypeCode.Char
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

    // ----------------------------------------------------

    #[cfg(feature = "unicode")]
    pub fn IsControl(c: char) -> bool {
        isUnicodeCategory(c, isControlMask)
    }

    #[cfg(not(feature = "unicode"))]
    pub fn IsControl(c: char) -> bool {
        c.is_control()
    }

    #[cfg(feature = "unicode")]
    pub fn IsDigit(c: char) -> bool {
        isUnicodeCategory(c, isDigitMask)
    }

    #[cfg(not(feature = "unicode"))]
    pub fn IsDigit(c: char) -> bool {
        c.is_ascii_digit() // TODO: very incomplete
    }

    #[cfg(feature = "unicode")]
    pub fn IsLetter(c: char) -> bool {
        isUnicodeCategory(c, isLetterMask)
    }

    #[cfg(not(feature = "unicode"))]
    pub fn IsLetter(c: char) -> bool {
        c.is_alphabetic() // TODO: not precise, includes Marks
    }

    #[cfg(feature = "unicode")]
    pub fn IsLetterOrDigit(c: char) -> bool {
        isUnicodeCategory(c, isLetterOrDigitMask)
    }

    #[cfg(not(feature = "unicode"))]
    pub fn IsLetterOrDigit(c: char) -> bool {
        IsLetter(c) || IsDigit(c)
    }

    #[cfg(feature = "unicode")]
    pub fn IsLower(c: char) -> bool {
        isUnicodeCategory(c, isLowerMask)
    }

    #[cfg(not(feature = "unicode"))]
    pub fn IsLower(c: char) -> bool {
        c.is_lowercase()
    }

    #[cfg(feature = "unicode")]
    pub fn IsUpper(c: char) -> bool {
        isUnicodeCategory(c, isUpperMask)
    }

    #[cfg(not(feature = "unicode"))]
    pub fn IsUpper(c: char) -> bool {
        c.is_uppercase()
    }

    #[cfg(feature = "unicode")]
    pub fn IsNumber(c: char) -> bool {
        isUnicodeCategory(c, isNumberMask)
    }

    #[cfg(not(feature = "unicode"))]
    pub fn IsNumber(c: char) -> bool {
        c.is_numeric()
    }

    #[cfg(feature = "unicode")]
    pub fn IsSeparator(c: char) -> bool {
        isUnicodeCategory(c, isSeparatorMask)
    }

    #[cfg(not(feature = "unicode"))]
    pub fn IsSeparator(c: char) -> bool {
        c.is_whitespace() && !c.is_control()
    }

    #[cfg(feature = "unicode")]
    pub fn IsPunctuation(c: char) -> bool {
        isUnicodeCategory(c, isPunctuationMask)
    }

    #[cfg(not(feature = "unicode"))]
    pub fn IsPunctuation(c: char) -> bool {
        c.is_ascii_punctuation() // TODO: very incomplete
    }

    #[cfg(feature = "unicode")]
    pub fn IsSymbol(c: char) -> bool {
        isUnicodeCategory(c, isSymbolMask)
    }

    #[cfg(not(feature = "unicode"))]
    pub fn IsSymbol(c: char) -> bool {
        c.is_ascii_punctuation() // TODO: very incomplete
    }

    #[cfg(feature = "unicode")]
    pub fn IsWhiteSpace(c: char) -> bool {
        // if (isLatin1(c)) {
        //     (Latin1CharInfo[c as usize] & IsWhiteSpaceFlag) != 0
        // } else {
        //     isUnicodeCategory(c, isWhiteSpaceMask)
        // }
        c.is_whitespace()
    }

    #[cfg(not(feature = "unicode"))]
    pub fn IsWhiteSpace(c: char) -> bool {
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
            // .NET Char.ToUpper never expands: when the full-case mapping is not a
            // single char (e.g. 'ß' -> "SS"), the original char is returned unchanged.
            let mut it = c.to_uppercase();
            match (it.next(), it.next()) {
                (Some(u), None) => u,
                _ => c,
            }
        }
    }

    pub fn ToUpperInvariant(c: char) -> char {
        ToUpper(c) // TODO: use invariant culture
    }

    pub fn ToLower(c: char) -> char {
        if (IsAscii(c)) {
            c.to_ascii_lowercase()
        } else {
            // .NET Char.ToLower never expands: when the full-case mapping is not a
            // single char, the original char is returned unchanged.
            let mut it = c.to_lowercase();
            match (it.next(), it.next()) {
                (Some(l), None) => l,
                _ => c,
            }
        }
    }

    pub fn ToLowerInvariant(c: char) -> char {
        ToLower(c) // TODO: use invariant culture
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
