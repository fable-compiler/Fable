import { Exception } from "./Util.ts";

export const NumberStyles = {
  None: 0,
  AllowLeadingWhite: 1,
  AllowTrailingWhite: 2,
  AllowLeadingSign: 4,
  AllowTrailingSign: 8,
  AllowParentheses: 16,
  AllowDecimalPoint: 32,
  AllowThousands: 64,
  AllowExponent: 128,
  AllowCurrencySymbol: 256,
  AllowHexSpecifier: 512,
  AllowBinarySpecifier: 1024,
  Integer: 7,
  HexNumber: 515,
  Number: 111,
  Float: 167,
} as const;

const allNumberStyles = 0x7ff;
export const radixStyles = NumberStyles.AllowHexSpecifier | NumberStyles.AllowBinarySpecifier;

// Returns the ArgumentException message instead of throwing: importing System.ts here is a circular import that breaks module initialisation
export function invalidStyleMessage(style: number, fixedSize: boolean): string | undefined {
  if (style & ~allNumberStyles) {
    return "An undefined NumberStyles value is being used.";
  }
  const radix = style & radixStyles;
  // Unlike BigInteger, the fixed size integers also reject AllowBinarySpecifier with anything but white space
  if (fixedSize && radix && (radix === radixStyles || style & ~NumberStyles.HexNumber & ~radixStyles)) {
    return "With the AllowHexSpecifier or AllowBinarySpecifier bit set in the enum bit field, the only other valid bits that can be combined into the enum value must be AllowLeadingWhite and AllowTrailingWhite.";
  }
  if (!fixedSize && style & NumberStyles.AllowHexSpecifier && style & ~NumberStyles.HexNumber) {
    return "With the AllowHexSpecifier bit set in the enum bit field, the only other valid bits that can be combined into the enum value must be a subset of those in HexNumber.";
  }
  return undefined;
}

export interface NumberParts {
  digits: string;
  negative: boolean;
}

// The digits of a number string once white space, sign and parentheses are stripped.
// .NET allows group separators anywhere after the first digit of the integral part, even repeated.
function digitsPattern(style: number): RegExp {
  const integral = style & NumberStyles.AllowThousands ? "\\d+(?:,\\d*)*" : "\\d+";
  const point = style & NumberStyles.AllowDecimalPoint ? "(?:\\.\\d*)?" : "";
  const leadingPoint = style & NumberStyles.AllowDecimalPoint ? "|\\.\\d+" : "";
  const exponent = style & NumberStyles.AllowExponent ? "(?:[eE][+-]?\\d+)?" : "";
  return new RegExp(`^(?:${integral}${point}${leadingPoint})${exponent}$`);
}

export function trimEndChars(s: string, chars: string): string {
  let end = s.length;
  while (end > 0 && chars.includes(s[end - 1])) {
    end--;
  }
  return s.slice(0, end);
}

const currencySymbol = "\u00a4";

function isWhite(ch: string): boolean {
  return ch === " " || (ch >= "\t" && ch <= "\r");
}

function isSign(ch: string): boolean {
  return ch === "+" || ch === "-";
}

// Returns undefined when the string does not have the format allowed by the style
export function splitNumber(str: string, style: number): NumberParts | undefined {
  if (style & NumberStyles.AllowHexSpecifier) {
    throw new Exception("The number style AllowHexSpecifier is not supported on floating point data types.");
  }
  let negative = false;
  let hasSign = false;
  let openParenthesis = false;
  let hasCurrency = false;
  let currencyAllowed = (style & NumberStyles.AllowCurrencySymbol) !== 0;
  let i = 0;
  for (; i < str.length; i++) {
    const ch = str[i];
    // .NET only skips white space after a sign when a currency symbol came before it
    if (isWhite(ch) && style & NumberStyles.AllowLeadingWhite && (!hasSign || hasCurrency)) {
      continue;
    }
    if (style & NumberStyles.AllowLeadingSign && !hasSign && isSign(ch)) {
      hasSign = true;
      negative = ch === "-";
    } else if (ch === "(" && style & NumberStyles.AllowParentheses && !hasSign) {
      hasSign = true;
      openParenthesis = true;
      negative = true;
    } else if (currencyAllowed && ch === currencySymbol) {
      hasCurrency = true;
      currencyAllowed = false;
    } else {
      break;
    }
  }
  const digits = /^[\d.,]*(?:[eE][+-]?\d+)?/.exec(str.slice(i))![0];
  if (!digitsPattern(style).test(digits)) {
    return undefined;
  }
  for (i += digits.length; i < str.length; i++) {
    const ch = str[i];
    if (isWhite(ch) && style & NumberStyles.AllowTrailingWhite) {
      continue;
    }
    if (style & NumberStyles.AllowTrailingSign && !hasSign && isSign(ch)) {
      hasSign = true;
      negative = ch === "-";
    } else if (ch === ")" && openParenthesis) {
      openParenthesis = false;
    } else if (currencyAllowed && ch === currencySymbol) {
      currencyAllowed = false;
    } else {
      break;
    }
  }
  // .NET ignores trailing NULs
  if (openParenthesis || trimEndChars(str.slice(i), "\0") !== "") {
    return undefined;
  }
  return { digits: digits.replace(/,/g, ""), negative };
}

// UInt64.MaxValue has 20 digits
const maxIntegerDigits = 20;

// Returns undefined where .NET overflows before its range check: a fraction, or a negative unsigned number with a decimal point
export function integerDigits(parts: NumberParts, unsigned: boolean): string | undefined {
  const [, integral, fraction, exponent] = /^(\d*)\.?(\d*)(?:[eE]([+-]?\d+))?$/.exec(parts.digits)!;
  if (unsigned && parts.negative && parts.digits.includes(".")) {
    return undefined;
  }
  const trimmed = trimEndChars(integral + fraction, "0");
  const significant = trimmed.replace(/^0+/, "");
  if (significant === "") {
    return "0";
  }
  const scale = Number(exponent ?? 0) + integral.length - trimmed.length;
  if (scale < 0 || significant.length + scale > maxIntegerDigits) {
    return undefined;
  }
  return significant + "0".repeat(scale);
}
