import * as Unicode from "./Unicode.13.0.0.js";

export type char = string;

function getCategoryFunc() {
  // unpack Unicode codepoint ranges (delta encoded) and general categories
  const offset = 35; // offsets unprintable characters
  const a1 = [...Unicode.rangeDeltas].map((ch) => (ch.codePointAt(0) ?? 0) - offset);
  const a2 = [...Unicode.categories].map((ch) => (ch.codePointAt(0) ?? 0) - offset);
  const codepoints = new Uint32Array(a1);
  const categories = new Uint8Array(a2);
  for (let i = 1; i < codepoints.length; ++i) {
    codepoints[i] += codepoints[i - 1];
  }
  // binary search in unicode ranges
  return (cp: number) => {
    let hi = codepoints.length;
    let lo = 0;
    while (hi - lo > 1) {
      const mid = Math.floor((hi + lo) / 2);
      const test = codepoints[mid];
      if (cp < test) {
        hi = mid;
      } else if (cp === test) {
        hi = lo = mid;
        break;
      } else if (test < cp) {
        lo = mid;
      }
    }
    return categories[lo];
  };
}

export const enum UnicodeCategory {
  UppercaseLetter,
  LowercaseLetter,
  TitlecaseLetter,
  ModifierLetter,
  OtherLetter,
  NonSpacingMark,
  SpacingCombiningMark,
  EnclosingMark,
  DecimalDigitNumber,
  LetterNumber,
  OtherNumber,
  SpaceSeparator,
  LineSeparator,
  ParagraphSeparator,
  Control,
  Format,
  Surrogate,
  PrivateUse,
  ConnectorPunctuation,
  DashPunctuation,
  OpenPunctuation,
  ClosePunctuation,
  InitialQuotePunctuation,
  FinalQuotePunctuation,
  OtherPunctuation,
  MathSymbol,
  CurrencySymbol,
  ModifierSymbol,
  OtherSymbol,
  OtherNotAssigned,
}

const isControlMask = 1 << UnicodeCategory.Control;
const isDigitMask = 1 << UnicodeCategory.DecimalDigitNumber;
const isLetterMask = 0
  | 1 << UnicodeCategory.UppercaseLetter
  | 1 << UnicodeCategory.LowercaseLetter
  | 1 << UnicodeCategory.TitlecaseLetter
  | 1 << UnicodeCategory.ModifierLetter
  | 1 << UnicodeCategory.OtherLetter;
const isLetterOrDigitMask = isLetterMask | isDigitMask;
const isUpperMask = 1 << UnicodeCategory.UppercaseLetter;
const isLowerMask = 1 << UnicodeCategory.LowercaseLetter;
const isNumberMask = 0
  | 1 << UnicodeCategory.DecimalDigitNumber
  | 1 << UnicodeCategory.LetterNumber
  | 1 << UnicodeCategory.OtherNumber;
const isPunctuationMask = 0
  | 1 << UnicodeCategory.ConnectorPunctuation
  | 1 << UnicodeCategory.DashPunctuation
  | 1 << UnicodeCategory.OpenPunctuation
  | 1 << UnicodeCategory.ClosePunctuation
  | 1 << UnicodeCategory.InitialQuotePunctuation
  | 1 << UnicodeCategory.FinalQuotePunctuation
  | 1 << UnicodeCategory.OtherPunctuation;
const isSeparatorMask = 0
  | 1 << UnicodeCategory.SpaceSeparator
  | 1 << UnicodeCategory.LineSeparator
  | 1 << UnicodeCategory.ParagraphSeparator;
const isSymbolMask = 0
  | 1 << UnicodeCategory.MathSymbol
  | 1 << UnicodeCategory.CurrencySymbol
  | 1 << UnicodeCategory.ModifierSymbol
  | 1 << UnicodeCategory.OtherSymbol;
const isWhiteSpaceMask = 0
  | 1 << UnicodeCategory.SpaceSeparator
  | 1 << UnicodeCategory.LineSeparator
  | 1 << UnicodeCategory.ParagraphSeparator;

const unicodeCategoryFunc = getCategoryFunc();

function charCodeAt(s: string, index: number) {
  if (index >= 0 && index < s.length) {
    return s.charCodeAt(index);
  } else {
    throw new Error("Index out of range.");
  }
}

export const getUnicodeCategory = (s: string) => getUnicodeCategory2(s, 0);
export const isControl = (s: string) => isControl2(s, 0);
export const isDigit = (s: string) => isDigit2(s, 0);
export const isLetter = (s: string) => isLetter2(s, 0);
export const isLetterOrDigit = (s: string) => isLetterOrDigit2(s, 0);
export const isUpper = (s: string) => isUpper2(s, 0);
export const isLower = (s: string) => isLower2(s, 0);
export const isNumber = (s: string) => isNumber2(s, 0);
export const isPunctuation = (s: string) => isPunctuation2(s, 0);
export const isSeparator = (s: string) => isSeparator2(s, 0);
export const isSymbol = (s: string) => isSymbol2(s, 0);
export const isWhiteSpace = (s: string) => isWhiteSpace2(s, 0);
export const isHighSurrogate = (s: string) => isHighSurrogate2(s, 0);
export const isLowSurrogate = (s: string) => isLowSurrogate2(s, 0);
export const isSurrogate = (s: string) => isSurrogate2(s, 0);

export function getUnicodeCategory2(s: string, index: number) {
  const cp = charCodeAt(s, index);
  return unicodeCategoryFunc(cp);
}

export function isControl2(s: string, index: number) {
  const test = 1 << getUnicodeCategory2(s, index);
  return (test & isControlMask) !== 0;
}

export function isDigit2(s: string, index: number) {
  const test = 1 << getUnicodeCategory2(s, index);
  return (test & isDigitMask) !== 0;
}

export function isLetter2(s: string, index: number) {
  const test = 1 << getUnicodeCategory2(s, index);
  return (test & isLetterMask) !== 0;
}

export function isLetterOrDigit2(s: string, index: number) {
  const test = 1 << getUnicodeCategory2(s, index);
  return (test & isLetterOrDigitMask) !== 0;
}

export function isUpper2(s: string, index: number) {
  const test = 1 << getUnicodeCategory2(s, index);
  return (test & isUpperMask) !== 0;
}

export function isLower2(s: string, index: number) {
  const test = 1 << getUnicodeCategory2(s, index);
  return (test & isLowerMask) !== 0;
}

export function isNumber2(s: string, index: number) {
  const test = 1 << getUnicodeCategory2(s, index);
  return (test & isNumberMask) !== 0;
}

export function isPunctuation2(s: string, index: number) {
  const test = 1 << getUnicodeCategory2(s, index);
  return (test & isPunctuationMask) !== 0;
}

export function isSeparator2(s: string, index: number) {
  const test = 1 << getUnicodeCategory2(s, index);
  return (test & isSeparatorMask) !== 0;
}

export function isSymbol2(s: string, index: number) {
  const test = 1 << getUnicodeCategory2(s, index);
  return (test & isSymbolMask) !== 0;
}

export function isWhiteSpace2(s: string, index: number) {
  const test = 1 << getUnicodeCategory2(s, index);
  if ((test & isWhiteSpaceMask) !== 0) {
    return true;
  }
  const cp = charCodeAt(s, index);
  return (0x09 <= cp && cp <= 0x0D) || cp === 0x85 || cp === 0xA0;
}

export function isHighSurrogate2(s: string, index: number) {
  const cp = charCodeAt(s, index);
  return (0xD800 <= cp && cp <= 0xDBFF);
}

export function isLowSurrogate2(s: string, index: number) {
  const cp = charCodeAt(s, index);
  return (0xDC00 <= cp && cp <= 0xDFFF);
}

export function isSurrogate2(s: string, index: number) {
  const cp = charCodeAt(s, index);
  return (0xD800 <= cp && cp <= 0xDFFF);
}

export function isSurrogatePair(s: string, index: string | number) {
  return typeof index === "number"
    ? isHighSurrogate2(s, index) && isLowSurrogate2(s, index + 1)
    : isHighSurrogate(s) && isLowSurrogate(index);
}

export function parse(input: string) {
  if (input.length === 1) {
    return input[0];
  } else {
    throw new Error("String must be exactly one character long.");
  }
}
