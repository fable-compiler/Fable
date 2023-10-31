export type char = string;
export declare const enum UnicodeCategory {
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
    OtherNotAssigned = 29
}
export declare const getUnicodeCategory: (s: string) => number;
export declare const isControl: (s: string) => boolean;
export declare const isDigit: (s: string) => boolean;
export declare const isLetter: (s: string) => boolean;
export declare const isLetterOrDigit: (s: string) => boolean;
export declare const isUpper: (s: string) => boolean;
export declare const isLower: (s: string) => boolean;
export declare const isNumber: (s: string) => boolean;
export declare const isPunctuation: (s: string) => boolean;
export declare const isSeparator: (s: string) => boolean;
export declare const isSymbol: (s: string) => boolean;
export declare const isWhiteSpace: (s: string) => boolean;
export declare const isHighSurrogate: (s: string) => boolean;
export declare const isLowSurrogate: (s: string) => boolean;
export declare const isSurrogate: (s: string) => boolean;
export declare function getUnicodeCategory2(s: string, index: number): number;
export declare function isControl2(s: string, index: number): boolean;
export declare function isDigit2(s: string, index: number): boolean;
export declare function isLetter2(s: string, index: number): boolean;
export declare function isLetterOrDigit2(s: string, index: number): boolean;
export declare function isUpper2(s: string, index: number): boolean;
export declare function isLower2(s: string, index: number): boolean;
export declare function isNumber2(s: string, index: number): boolean;
export declare function isPunctuation2(s: string, index: number): boolean;
export declare function isSeparator2(s: string, index: number): boolean;
export declare function isSymbol2(s: string, index: number): boolean;
export declare function isWhiteSpace2(s: string, index: number): boolean;
export declare function isHighSurrogate2(s: string, index: number): boolean;
export declare function isLowSurrogate2(s: string, index: number): boolean;
export declare function isSurrogate2(s: string, index: number): boolean;
export declare function isSurrogatePair(s: string, index: string | number): boolean;
export declare function parse(input: string): string;
