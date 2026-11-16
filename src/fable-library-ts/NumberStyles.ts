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
