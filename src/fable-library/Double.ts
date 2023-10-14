import { FSharpRef } from "./Types.js";

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
    throw new Error(`The input string ${str} was not in a correct format.`);
  }
}

// JS Number.isFinite function evals false for NaN
export function isPositiveInfinity(x: number) {
  return x === Number.POSITIVE_INFINITY;
}

export function isNegativeInfinity(x: number) {
  return x === Number.NEGATIVE_INFINITY;
}

export function isInfinity(x: number) {
  return x === Number.POSITIVE_INFINITY || x === Number.NEGATIVE_INFINITY;
}

export function max(x: number, y: number): number {
  return x > y ? x : y;
}

export function min(x: number, y: number): number {
  return x < y ? x : y;
}

export function maxMagnitude(x: number, y: number): number {
  return Math.abs(x) > Math.abs(y) ? x : y;
}

export function minMagnitude(x: number, y: number): number {
  return Math.abs(x) < Math.abs(y) ? x : y;
}

export function clamp(x: number, min: number, max: number): number {
  return x < min ? min : x > max ? max : x;
}
