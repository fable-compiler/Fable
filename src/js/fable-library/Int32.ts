export enum NumberStyles {
  // None = 0x00000000,
  // AllowLeadingWhite = 0x00000001,
  // AllowTrailingWhite = 0x00000002,
  // AllowLeadingSign = 0x00000004,
  // AllowTrailingSign = 0x00000008,
  // AllowParentheses = 0x00000010,
  // AllowDecimalPoint = 0x00000020,
  // AllowThousands = 0x00000040,
  // AllowExponent = 0x00000080,
  // AllowCurrencySymbol = 0x00000100,
  AllowHexSpecifier = 0x00000200,

  // Integer = AllowLeadingWhite | AllowTrailingWhite | AllowLeadingSign,
  // HexNumber = AllowLeadingWhite | AllowTrailingWhite | AllowHexSpecifier,
  // Number = AllowLeadingWhite | AllowTrailingWhite | AllowLeadingSign |
  //          AllowTrailingSign | AllowDecimalPoint | AllowThousands,
  // Float = AllowLeadingWhite | AllowTrailingWhite | AllowLeadingSign |
  //         AllowDecimalPoint | AllowExponent,
  // Currency = AllowLeadingWhite | AllowTrailingWhite | AllowLeadingSign | AllowTrailingSign |
  //            AllowParentheses | AllowDecimalPoint | AllowThousands | AllowCurrencySymbol,
  // Any = AllowLeadingWhite | AllowTrailingWhite | AllowLeadingSign | AllowTrailingSign |
  //       AllowParentheses | AllowDecimalPoint | AllowThousands | AllowCurrencySymbol | AllowExponent,
}

function validResponse(regexMatch: RegExpExecArray, radix: number) {
  const [_all, sign, prefix, digits] = regexMatch;
  return {
    sign: sign || "",
    prefix: prefix || "",
    digits,
    radix,
  };
}

function getRange(unsigned: boolean, bitsize: number): [number, number] {
  switch (bitsize) {
    case 8: return unsigned ? [0, 255] : [-128, 127];
    case 16: return unsigned ? [0, 65535] : [-32768, 32767];
    case 32: return unsigned ? [0, 4294967295] : [-2147483648, 2147483647];
    default: throw new Error("Invalid bit size.");
  }
}

function getInvalidDigits(radix: number): RegExp {
  switch (radix) {
    case 2: return /[^0-1]/;
    case 8: return /[^0-7]/;
    case 10: return /[^0-9]/;
    case 16: return /[^0-9a-fA-F]/;
    default:
      throw new Error("Invalid Base.");
  }
}

export function getRadix(prefix: string, style: number) {
  if (style & NumberStyles.AllowHexSpecifier) {
    return 16;
  } else {
    switch (prefix) {
      case "0b": case "0B": return 2;
      case "0o": case "0O": return 8;
      case "0x": case "0X": return 16;
      default: return 10;
    }
  }
}

export function isValid(str: string, style: number, radix?: number) {
  const integerRegex = /^\s*([\+\-])?(0[xXoObB])?([0-9a-fA-F]+)\s*$/;
  const res = integerRegex.exec(str.replace("_", ""));
  if (res != null) {
    const [_all, sign, prefix, digits] = res;
    radix = radix || getRadix(prefix, style);
    const invalidDigits = getInvalidDigits(radix);
    if (!invalidDigits.test(digits)) {
      return validResponse(res, radix);
    }
  }
  return null;
}

export function tryParse(
  str: string, style: number, unsigned: boolean, bitsize: number, radix?: number): [boolean, number] {
  try {
    const res = isValid(str, style, radix);
    if (res != null) {
      const [min, max] = getRange(unsigned, bitsize);
      const v = parseInt(res.sign + res.digits, res.radix);
      if (!Number.isNaN(v) && v >= min && v <= max) {
        return [true, v];
      }
    }
  } catch {
    // supress error
  }
  return [false, 0];
}

export function parse(str: string, style: number, unsigned: boolean, bitsize: number, radix?: number): number {
  const [ok, value] = tryParse(str, style, unsigned, bitsize, radix);
  if (ok) {
    return value;
  } else {
    throw new Error("Input string was not in a correct format.");
  }
}
