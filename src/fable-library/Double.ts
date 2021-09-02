import { FSharpRef } from "./Types.js";
import { doubleToInt64Bits } from './BitConverter.js';
import * as LongLib from "./lib/long.js";

export function tryParse(str: string, defValue: FSharpRef<number>): boolean {
  // TODO: test if value is valid and in range
  if (str != null && /\S/.test(str)) {
    const v = +str.replace("_", "");
    if (!Number.isNaN(v)) {
      defValue.contents = v;
      return true;
    }
  }
  return false;
}

export function parse(str: string): number {
  const defValue = new FSharpRef(0);
  if (tryParse(str, defValue)) {
    return defValue.contents;
  } else {
    throw new Error("Input string was not in a correct format.");
  }
}

// JS Number.isFinite function evals false for NaN
export function isInfinity(x: number) {
  return x === Number.POSITIVE_INFINITY || x === Number.NEGATIVE_INFINITY;
}

// https://github.com/dotnet/runtime/blob/6ff32877f6ae0ddf48218477c7acc78de12f7fbf/src/libraries/System.Private.CoreLib/src/System/Double.cs#L122-L130
export function isNormal(d: number) {
  let bits = doubleToInt64Bits(d);
  bits = LongLib.and(bits, 0x7FFFFFFFFFFFFFFF);
  return LongLib.lessThan(bits, 0x7FF0000000000000) && (bits !== LongLib.ZERO) && (LongLib.and(bits, 0x7FF0000000000000) !== LongLib.ZERO);
}