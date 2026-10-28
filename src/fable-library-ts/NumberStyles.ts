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
  AllowHexSpecifier: 512,
  Integer: 7,
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

// Returns undefined when the string does not have the format allowed by the style
export function splitNumber(str: string, style: number): NumberParts | undefined {
  if (style & NumberStyles.AllowHexSpecifier) {
    throw new Exception("The number style AllowHexSpecifier is not supported on floating point data types.");
  }
  // .NET ignores trailing NULs and its white space is only [\t\n\v\f\r ]
  let s = str.replace(/\0+$/, "");
  if (style & NumberStyles.AllowLeadingWhite) {
    s = s.replace(/^[\t-\r ]+/, "");
  }
  if (style & NumberStyles.AllowTrailingWhite) {
    s = s.replace(/[\t-\r ]+$/, "");
  }
  let negative = false;
  if (style & NumberStyles.AllowParentheses && s.startsWith("(") && s.endsWith(")")) {
    s = s.slice(1, -1);
    negative = true;
  }
  if (style & NumberStyles.AllowLeadingSign && /^[+-]/.test(s)) {
    negative = s[0] === "-" ? !negative : negative;
    s = s.slice(1);
  } else if (style & NumberStyles.AllowTrailingSign && /[+-]$/.test(s)) {
    negative = s[s.length - 1] === "-" ? !negative : negative;
    s = s.slice(0, -1);
  }
  if (!digitsPattern(style).test(s)) {
    return undefined;
  }
  return { digits: s.replace(/,/g, ""), negative };
}
