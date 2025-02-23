import Decimal, { BigSource } from "./lib/big.js";
import { Numeric, symbol } from "./Numeric.js";
import { FSharpRef } from "./Types.js";
import { combineHashCodes } from "./Util.js";
import { int8, uint8, int16, uint16, int32, uint32, float16, float32, float64 } from "./Int32.js";
import { fromDecimal, int64, uint64, int128, uint128, nativeint, unativeint } from "./BigInt.js";
import * as bigInt from "./BigInt.js";

Decimal.prototype.GetHashCode = function () {
  return combineHashCodes([this.s, this.e].concat(this.c))
}

Decimal.prototype.Equals = function (x: Decimal) {
  return !this.cmp(x)
}

Decimal.prototype.CompareTo = function (x: Decimal) {
  return this.cmp(x)
}

Decimal.prototype[symbol] = function() {
  const _this = this;
  return {
    multiply: (y: Numeric) => _this.mul(y as BigSource),
    toPrecision: (sd?: number) => _this.toPrecision(sd),
    toExponential: (dp?: number) => _this.toExponential(dp),
    toFixed: (dp?: number) => _this.toFixed(dp),
    toHex: () => (Number(_this) >>> 0).toString(16),
  }
}

export default Decimal;
export type decimal = Decimal;

export const get_Zero = new Decimal(0);
export const get_One = new Decimal(1);
export const get_MinusOne = new Decimal(-1);
export const get_MaxValue = new Decimal("79228162514264337593543950335");
export const get_MinValue = new Decimal("-79228162514264337593543950335");

export function compare(x: Decimal, y: Decimal) {
  return x.cmp(y);
}

export function equals(x: Decimal, y: Decimal) {
  return !x.cmp(y);
}

export function abs(x: Decimal) { return x.abs(); }
export function sign(x: Decimal): number { return x.lt(get_Zero) ? -1 : x.gt(get_Zero) ? 1 : 0; }

export function max(x: Decimal, y: Decimal): Decimal { return x.gt(y) ? x : y; }
export function min(x: Decimal, y: Decimal): Decimal { return x.lt(y) ? x : y; }

export function maxMagnitude(x: Decimal, y: Decimal): Decimal { return abs(x).gt(abs(y)) ? x : y; }
export function minMagnitude(x: Decimal, y: Decimal): Decimal { return abs(x).lt(abs(y)) ? x : y; }

export function clamp(x: Decimal, min: Decimal, max: Decimal): Decimal {
  return x.lt(min) ? min : x.gt(max) ? max : x;
}

export function round(x: Decimal, digits: number = 0) {
  return x.round(digits, 2 /* ROUND_HALF_EVEN */);
}

export function truncate(x: Decimal) {
  return x.round(0, 0 /* ROUND_DOWN */);
}

export function ceiling(x: Decimal) {
  return x.round(0, x.cmp(0) >= 0 ? 3 /* ROUND_UP */ : 0 /* ROUND_DOWN */);
}

export function floor(x: Decimal) {
  return x.round(0, x.cmp(0) >= 0 ? 0 /* ROUND_DOWN */ : 3 /* ROUND_UP */);
}

export function pow(x: Decimal, n: number) {
  return x.pow(n);
}

export function sqrt(x: Decimal) {
  return x.sqrt();
}

export function op_Addition(x: Decimal, y: Decimal) {
  return x.add(y);
}

export function op_Subtraction(x: Decimal, y: Decimal) {
  return x.sub(y);
}

export function op_Multiply(x: Decimal, y: Decimal) {
  return x.mul(y);
}

export function op_Division(x: Decimal, y: Decimal) {
  return x.div(y);
}

export function op_Modulus(x: Decimal, y: Decimal) {
  return x.mod(y);
}

export function op_UnaryNegation(x: Decimal) {
  const x2 = new Decimal(x);
  x2.s = -x2.s || 0;
  return x2;
}

export function op_UnaryPlus(x: Decimal) {
  return x;
}

export const add = op_Addition;
export const subtract = op_Subtraction;
export const multiply = op_Multiply;
export const divide = op_Division;
export const remainder = op_Modulus;
export const negate = op_UnaryNegation;

export function toString(x: Decimal) {
  return x.toString();
}

export function tryParse(str: string, defValue: FSharpRef<Decimal>): boolean {
  try {
    defValue.contents = new Decimal(str.trim());
    return true;
  } catch {
    return false;
  }
}

export function parse(str: string): Decimal {
  const defValue = new FSharpRef(get_Zero);
  if (tryParse(str, defValue)) {
    return defValue.contents;
  } else {
    throw new Error(`The input string ${str} was not in a correct format.`);
  }
}

export function toNumber(x: Decimal): number {
  return +x;
}

export function toChar(x: Decimal): string {
  const n = toNumber(x);
  if (n < 0 || n > 65535 || isNaN(n)) {
    throw new Error("Value was either too large or too small for a character.");
  }
  return String.fromCharCode(n);
}

export function toInt8(x: Decimal): int8 { return bigInt.toInt8(fromDecimal(x)); }
export function toUInt8(x: Decimal): uint8 { return bigInt.toUInt8(fromDecimal(x)); }
export function toInt16(x: Decimal): int16 { return bigInt.toInt16(fromDecimal(x)); }
export function toUInt16(x: Decimal): uint16 { return bigInt.toUInt16(fromDecimal(x)); }
export function toInt32(x: Decimal): int32 { return bigInt.toInt32(fromDecimal(x)); }
export function toUInt32(x: Decimal): uint32 { return bigInt.toUInt32(fromDecimal(x)); }
export function toInt64(x: Decimal): int64 { return bigInt.toInt64(fromDecimal(x)); }
export function toUInt64(x: Decimal): uint64 { return bigInt.toUInt64(fromDecimal(x)); }
export function toInt128(x: Decimal): int128 { return bigInt.toInt128(fromDecimal(x)); }
export function toUInt128(x: Decimal): uint128 { return bigInt.toUInt128(fromDecimal(x)); }
export function toNativeInt(x: Decimal): nativeint { return bigInt.toNativeInt(fromDecimal(x)); }
export function toUNativeInt(x: Decimal): unativeint { return bigInt.toUNativeInt(fromDecimal(x)); }

export function toFloat16(x: Decimal): float16 { return toNumber(x); }
export function toFloat32(x: Decimal): float32 { return toNumber(x); }
export function toFloat64(x: Decimal): float64 { return toNumber(x); }

function decimalToHex(dec: Uint8Array, bitSize: number) {
  const hex = new Uint8Array(bitSize / 4 | 0);
  let hexCount = 1;
  for (let d = 0; d < dec.length; d++) {
    let value = dec[d];
    for (let i = 0; i < hexCount; i++) {
      const digit = hex[i] * 10 + value | 0;
      hex[i] = digit & 0xF;
      value = digit >> 4;
    }
    if (value !== 0) {
      hex[hexCount++] = value;
    }
  }
  return hex.slice(0, hexCount); // digits in reverse order
}

function hexToDecimal(hex: Uint8Array, bitSize: number) {
  const dec = new Uint8Array(bitSize * 301 / 1000 + 1 | 0);
  let decCount = 1;
  for (let d = hex.length - 1; d >= 0; d--) {
    let carry = hex[d];
    for (let i = 0; i < decCount; i++) {
      const val = dec[i] * 16 + carry | 0;
      dec[i] = (val % 10) | 0;
      carry = (val / 10) | 0;
    }
    while (carry > 0) {
      dec[decCount++] = (carry % 10) | 0;
      carry = (carry / 10) | 0;
    }
  }
  return dec.slice(0, decCount); // digits in reverse order
}

function setInt32Bits(hexDigits: Uint8Array, bits: number, offset: number) {
  for (let i = 0; i < 8; i++) {
    hexDigits[offset + i] = (bits >> (i * 4)) & 0xF;
  }
}

function getInt32Bits(hexDigits: Uint8Array, offset: number) {
  let bits = 0;
  for (let i = 0; i < 8; i++) {
    bits = bits | (hexDigits[offset + i] << (i * 4));
  }
  return bits;
}

export function fromIntArray(bits: ArrayLike<number>) {
  return fromInts(bits[0], bits[1], bits[2], bits[3]);
}

export function fromInts(low: number, mid: number, high: number, signExp: number) {
  const isNegative = signExp < 0;
  const scale = (signExp >> 16) & 0x7F;
  return fromParts(low, mid, high, isNegative, scale);
}

export function fromParts(low: number, mid: number, high: number, isNegative: boolean, scale: number) {
  const bitSize = 96;
  const hexDigits = new Uint8Array(bitSize / 4);
  setInt32Bits(hexDigits, low, 0);
  setInt32Bits(hexDigits, mid, 8);
  setInt32Bits(hexDigits, high, 16);
  const decDigits = hexToDecimal(hexDigits, bitSize);
  scale = scale & 0x7F;
  const big = new Decimal(0);
  big.c = Array.from(decDigits.reverse());
  big.e = decDigits.length - scale - 1;
  big.s = isNegative ? -1 : 1;
  const d = new Decimal(big);
  return d;
}

export function getBits(d: Decimal) {
  const bitSize = 96;
  const decDigits = Uint8Array.from(d.c);
  const hexDigits = decimalToHex(decDigits, bitSize);
  const low = getInt32Bits(hexDigits, 0);
  const mid = getInt32Bits(hexDigits, 8);
  const high = getInt32Bits(hexDigits, 16);
  const decStr = d.toString();
  const dotPos = decStr.indexOf(".");
  const scale = dotPos < 0 ? 0 : decStr.length - dotPos - 1;
  const signExp = ((scale & 0x7F) << 16) | (d.s < 0 ? 0x80000000 : 0);
  return [low, mid, high, signExp];
}

// export function makeRangeStepFunction(step: Decimal, last: Decimal) {
//   const stepComparedWithZero = step.cmp(get_Zero);
//   if (stepComparedWithZero === 0) {
//     throw new Error("The step of a range cannot be zero");
//   }
//   const stepGreaterThanZero = stepComparedWithZero > 0;
//   return (x: Decimal) => {
//     const comparedWithLast = x.cmp(last);
//     if ((stepGreaterThanZero && comparedWithLast <= 0)
//       || (!stepGreaterThanZero && comparedWithLast >= 0)) {
//       return [x, op_Addition(x, step)];
//     } else {
//       return undefined;
//     }
//   };
// }
