const parseRadix = /^\s*([\+\-])?(0[xXoObB])?([0-9a-fA-F]+)\s*$/;
const invalidRadix2 = /[^01]/;
const invalidRadix8 = /[^0-7]/;
const invalidRadix10 = /[^0-9]/;

function validResponse(regexMatch: RegExpExecArray, radix: number) {
  const [, prefix, , digits] = regexMatch;
  return {
    prefix: prefix || "",
    digits,
    radix,
  };
}

export function isValid(s: string, radix?: number) {
  const res = parseRadix.exec(s);
  if (res != null) {
    if (radix == null) {
      switch (res[2]) {
        case "0b": case "0B": radix = 2; break;
        case "0o": case "0O": radix = 8; break;
        case "0x": case "0X": radix = 16; break;
        default: radix = 10; break;
      }
    }
    switch (radix) {
      case 2:
        return invalidRadix2.test(res[3]) ? null : validResponse(res, 2);
      case 8:
        return invalidRadix8.test(res[3]) ? null : validResponse(res, 8);
      case 10:
        return invalidRadix10.test(res[3]) ? null : validResponse(res, 10);
      case 16:
        return validResponse(res, 16);
      default:
        throw new Error("Invalid Base.");
    }
  }
  return null;
}

// TODO does this perfectly match the .NET behavior ?
export function tryParse(s: string | null, radix?: number, initial?: number): [boolean, number] {
  const res = isValid(s, radix);
  if (res !== null) {
    const v = parseInt(res.prefix + res.digits, res.radix);
    if (!Number.isNaN(v)) {
      return [true, v];
    }
  }
  return [false, initial];
}

export function parse(s: string, radix?: number): number {
  const a = tryParse(s, radix, 0);
  if (a[0]) {
    return a[1];
  } else {
    throw new Error("Input string was not in a correct format.");
  }
}
