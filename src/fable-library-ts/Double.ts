import { FSharpRef } from "./Types.ts";
import { Exception } from "./Util.ts";

// .NET's Float style: white space, a sign, digits with an optional fraction or a fraction alone, an exponent, then NULs
const floatRegex = /^[\t-\r ]*[+-]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eE][+-]?\d+)?[\t-\r ]*\0*$/;
// .NET's NaN and infinity symbols, ignoring case, allowing a sign and trimmed like String.Trim
const symbolRegex = /^[\t-\r \u0085\u00a0\u1680\u2000-\u200a\u2028\u2029\u202f\u205f\u3000]*(?:[+-]?nan|([+-]?)infinity)[\t-\r \u0085\u00a0\u1680\u2000-\u200a\u2028\u2029\u202f\u205f\u3000]*$/i;

export function tryParse(str: string, defValue: FSharpRef<number>): boolean {
  if (str != null) {
    if (floatRegex.test(str)) {
      defValue.contents = Number.parseFloat(str);
      return true;
    }
    const symbol = symbolRegex.exec(str);
    if (symbol != null) {
      defValue.contents = symbol[1] == null ? NaN : symbol[1] === "-" ? -Infinity : Infinity;
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
    throw new Exception(`The input string ${str} was not in a correct format.`);
  }
}

// FSharp.Core's float and float32 remove every underscore before parsing
export function parseIgnoringUnderscores(str: string): number {
  return parse(str == null ? str : str.replace(/_/g, ""));
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
  return Math.max(x, y);
}

export function min(x: number, y: number): number {
  return Math.min(x, y);
}

export function maxMagnitude(x: number, y: number): number {
  if (Number.isNaN(x) || Number.isNaN(y)) { return NaN; }
  return Math.abs(x) > Math.abs(y) ? x : y;
}

export function minMagnitude(x: number, y: number): number {
  if (Number.isNaN(x) || Number.isNaN(y)) { return NaN; }
  return Math.abs(x) < Math.abs(y) ? x : y;
}

export function clamp(x: number, min: number, max: number): number {
  return x < min ? min : x > max ? max : x;
}
