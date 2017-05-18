// TODO verify that this matches the behavior of .NET
const parseRadix10 = /^ *([\+\-]?[0-9]+) *$/;

// TODO verify that this matches the behavior of .NET
const parseRadix16 = /^ *([\+\-]?[0-9a-fA-F]+) *$/;

export function isValid(s: string | null, radix: number): string[] | null {
  if (s != null) {
    if (radix === 16) {
      return parseRadix16.exec(s);

    } else if (radix <= 10) {
      return parseRadix10.exec(s);
    }
  }

  return null;
}

// TODO does this perfectly match the .NET behavior ?
export function tryParse(s: string | null, radix: number, initial: number): [boolean, number] {
  const a = isValid(s, radix);

  if (a !== null) {
    const v = parseInt(a[1], radix);

    if (!Number.isNaN(v)) {
      return [true, v];
    }
  }

  return [false, initial];
}

export function parse(s: string | null, radix: number = 10): number {
  const a = tryParse(s, radix, 0);

  if (a[0]) {
    return a[1];

  } else {
    // TODO FormatException ?
    throw new Error("Input string was not in a correct format.");
  }
}
