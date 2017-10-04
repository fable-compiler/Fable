const parseRadix = /^\s*([\+\-])?(0[xob])?([0-9a-fA-F]+)\s*$/;
const invalidRadix2 = /[^01]/;
const invalidRadix8 = /[^0-7]/;
const invalidRadix10 = /[^0-9]/;

export function isValid(s: string, radix?: number): [string[], number] {
  const res = parseRadix.exec(s);
  if (res != null) {
    if (radix != null) {
      return [res, radix];
    } else {
      switch (res[2]) {
        case "0b":
          return invalidRadix2.test(res[3]) ? null : [res, 2];
        case "0o":
          return invalidRadix8.test(res[3]) ? null : [res, 8];
        case "0x":
          return [res, 16];
        default:
          return invalidRadix10.test(res[3]) ? null : [res, 10];
      }
    }
  }
  return null;
}

// TODO does this perfectly match the .NET behavior ?
export function tryParse(s: string | null, radix?: number, initial?: number): [boolean, number] {
  const a = isValid(s, radix);
  if (a !== null) {
    const [[, prefix, , digits], radix_] = a;
    const v = parseInt((prefix || "") + digits, radix_);
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
