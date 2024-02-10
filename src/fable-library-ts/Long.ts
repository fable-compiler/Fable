import { getPrefix, isValid } from "./Int32.js";
import { fromString } from "./BigInt.js";
import { FSharpRef } from "./Types.js";

function getMaxValue(unsigned: boolean, radix: number, isNegative: boolean) {
  switch (radix) {
    case 2: return unsigned ?
      "1111111111111111111111111111111111111111111111111111111111111111" :
      (isNegative ? "1000000000000000000000000000000000000000000000000000000000000000"
        : "111111111111111111111111111111111111111111111111111111111111111");
    case 8: return unsigned ?
      "1777777777777777777777" :
      (isNegative ? "1000000000000000000000" : "777777777777777777777");
    case 10: return unsigned ?
      "18446744073709551615" :
      (isNegative ? "9223372036854775808" : "9223372036854775807");
    case 16: return unsigned ?
      "FFFFFFFFFFFFFFFF" :
      (isNegative ? "8000000000000000" : "7FFFFFFFFFFFFFFF");
    default: throw new Error("Invalid radix.");
  }
}

export function parse(str: string, style: number, unsigned: boolean, _bitsize: number, radix?: number) {
  const res = isValid(str, style, radix);
  if (res != null) {
    const lessOrEqual = (x: string, y: string) => {
      const len = Math.max(x.length, y.length);
      return x.padStart(len, "0") <= y.padStart(len, "0");
    };
    const isNegative = res.sign === "-";
    const maxValue = getMaxValue(unsigned || res.radix !== 10, res.radix, isNegative);
    if (lessOrEqual(res.digits.toUpperCase(), maxValue)) {
      str = getPrefix(res.radix) + res.digits;
      str = isNegative ? res.sign + str : str;
      return fromString(str);
    }
  }
  throw new Error(`The input string ${str} was not in a correct format.`);
}

export function tryParse(str: string, style: number, unsigned: boolean, bitsize: number, defValue: FSharpRef<bigint>) {
  try {
    defValue.contents = parse(str, style, unsigned, bitsize);
    return true;
  } catch {
    return false;
  }
}
