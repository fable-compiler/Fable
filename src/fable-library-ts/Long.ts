import { getPrefix, isValid } from "./Int32.ts";
import { fromString } from "./BigInt.ts";
import { FSharpRef } from "./Types.ts";
import { Exception } from "./Util.ts";

function getRange(unsigned: boolean, bitsize: number): [bigint, bigint] {
  switch (bitsize) {
    case 64: return unsigned ?
      [0n, 18446744073709551615n] :
      [-9223372036854775808n, 9223372036854775807n];
    default: throw new Exception("Invalid bit size.");
  }
}

export function parse(str: string, style: number, unsigned: boolean, bitsize: number, radix?: number) {
  const res = isValid(str, style, radix);
  if (res != null) {
    let v = fromString(getPrefix(res.radix) + res.digits);
    if (res.sign === "-") {
      v = -v;
    }
    const [umin, umax] = getRange(true, bitsize);
    if (!unsigned && res.radix !== 10 && v >= umin && v <= umax) {
      v = BigInt.asIntN(bitsize, v);
    }
    const [min, max] = getRange(unsigned, bitsize);
    if (v >= min && v <= max) {
      return v;
    }
  }
  throw new Exception(`The input string ${str} was not in a correct format.`);
}

export function tryParse(str: string, style: number, unsigned: boolean, bitsize: number, defValue: FSharpRef<bigint>) {
  try {
    defValue.contents = parse(str, style, unsigned, bitsize);
    return true;
  } catch {
    return false;
  }
}
