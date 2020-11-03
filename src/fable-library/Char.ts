// Adapted from: https://github.com/hakatashi/general-category
import * as Encoding from "./Encoding.js";
import packedUnicode from "./Unicode.9.0.0.js";

function decodeVByteToIntegerArray(buffer: Uint8Array) {
  const ret = [];
  let carried = 0;
  let register = 0;
  for (let i = 0; i < buffer.length; ++i) {
    const byte = buffer[i] ^ 0xFF;
    register += (byte & 127) << carried * 7;
    carried++;
    if ((byte & 128) !== 0) {
      ret.push(register - 1);
      carried = register = 0;
    }
  }
  return ret;
}

function getCategoryFunc() {
  // unpack Unicode ranges and categories (delta encoded, vbyte encoded, utf8 encoded)
  const unicodeBuffer = Encoding.get_UTF8().getBytes(packedUnicode);
  const unicodeDeltas = decodeVByteToIntegerArray(unicodeBuffer);
  const codepoints = new Uint32Array(unicodeDeltas.length / 2);
  const categories = new Uint8Array(unicodeDeltas.length / 2);
  const categoryEnum = new Uint8Array(
    [14, 15, 29, 17, 16, 1, 3, 4, 2, 0, 6, 7, 5, 8, 9, 10, 18, 19, 21, 23, 22, 24, 20, 26, 27, 25, 28, 12, 13, 11]);
  let currentCodepoint = 0;
  for (let i = 0; i < unicodeDeltas.length; i += 2) {
    codepoints[i / 2] = (currentCodepoint += unicodeDeltas[i]);
    categories[i / 2] = unicodeDeltas[i + 1];
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
    return categoryEnum[categories[lo]];
  };
}

export const enum UnicodeCategory {
  Control = 14,
  Format = 15,
  OtherNotAssigned = 29,
  PrivateUse = 17,
  Surrogate = 16,
  LowercaseLetter = 1,
  ModifierLetter = 3,
  OtherLetter = 4,
  TitlecaseLetter = 2,
  UppercaseLetter = 0,
  SpacingCombiningMark = 6,
  EnclosingMark = 7,
  NonSpacingMark = 5,
  DecimalDigitNumber = 8,
  LetterNumber = 9,
  OtherNumber = 10,
  ConnectorPunctuation = 18,
  DashPunctuation = 19,
  ClosePunctuation = 21,
  FinalQuotePunctuation = 23,
  InitialQuotePunctuation = 22,
  OtherPunctuation = 24,
  OpenPunctuation = 20,
  CurrencySymbol = 26,
  ModifierSymbol = 27,
  MathSymbol = 25,
  OtherSymbol = 28,
  LineSeparator = 12,
  ParagraphSeparator = 13,
  SpaceSeparator = 11,
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
