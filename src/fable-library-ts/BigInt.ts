import { FSharpRef } from "./Types.js";
import { int8, uint8, int16, uint16, int32, uint32, float16, float32, float64 } from "./Int32.js";
import { decimal, fromParts, truncate } from "./Decimal.js";
import { bigintHash } from "./Util.js";

const isBigEndian = false;

export type int64 = bigint;
export type uint64 = bigint;
export type int128 = bigint;
export type uint128 = bigint;
export type nativeint = bigint;
export type unativeint = bigint;

(BigInt.prototype as any).toJSON = function () {
    return `${this.toString()}`;
};

const zero: bigint = 0n;
const one: bigint = 1n;
const two: bigint = 2n;
const minusOne: bigint = -1n;

export function isBigInt(x: any): boolean {
    return typeof x === "bigint";
}

export function hash(x: bigint): int32 {
    return bigintHash(x);
}

export function equals(x: bigint, y: bigint): boolean {
    return x === y;
}

export function compare(x: bigint, y: bigint): int32 {
    return x < y ? -1 : x > y ? 1 : 0;
}

export function abs(x: bigint): bigint { return x < zero ? -x : x; }
export function sign(x: bigint): int32 { return x < zero ? -1 : x > zero ? 1 : 0; }

export function max(x: bigint, y: bigint): bigint { return x > y ? x : y; }
export function min(x: bigint, y: bigint): bigint { return x < y ? x : y; }

export function maxMagnitude(x: bigint, y: bigint): bigint { return abs(x) > abs(y) ? x : y; }
export function minMagnitude(x: bigint, y: bigint): bigint { return abs(x) < abs(y) ? x : y; }

export function clamp(x: bigint, min: bigint, max: bigint): bigint {
    return x < min ? min : x > max ? max : x;
}

export function add(x: bigint, y: bigint): bigint { return x + y }
export function subtract(x: bigint, y: bigint): bigint { return x - y }
export function multiply(x: bigint, y: bigint): bigint { return x * y }
export function divide(x: bigint, y: bigint): bigint { return x / y }
export function remainder(x: bigint, y: bigint): bigint { return x % y }
export function negate(x: bigint): bigint { return -x }

export function op_UnaryNegation(x: bigint): bigint { return -x; }
export function op_LogicalNot(x: bigint): bigint { return ~x; }
export function op_UnaryPlus(x: bigint): bigint { return x; }

export function op_Addition(x: bigint, y: bigint): bigint { return x + y; }
export function op_Subtraction(x: bigint, y: bigint): bigint { return x - y; }
export function op_Multiply(x: bigint, y: bigint): bigint { return x * y; }
export function op_Division(x: bigint, y: bigint): bigint { return x / y; }
export function op_Modulus(x: bigint, y: bigint): bigint { return x % y; }

export function op_RightShift(x: bigint, n: int32): bigint { return x >> BigInt(n); }
export function op_LeftShift(x: bigint, n: int32): bigint { return x << BigInt(n); }
export function op_BitwiseAnd(x: bigint, y: bigint): bigint { return x & y; }
export function op_BitwiseOr(x: bigint, y: bigint): bigint { return x | y; }
export function op_ExclusiveOr(x: bigint, y: bigint): bigint { return x ^ y; }

export function op_LessThan(x: bigint, y: bigint): boolean { return x < y; }
export function op_LessThanOrEqual(x: bigint, y: bigint): boolean { return x <= y; }
export function op_GreaterThan(x: bigint, y: bigint): boolean { return x > y; }
export function op_GreaterThanOrEqual(x: bigint, y: bigint): boolean { return x >= y; }
export function op_Equality(x: bigint, y: bigint): boolean { return x === y; }
export function op_Inequality(x: bigint, y: bigint): boolean { return x !== y; }

export function get_Zero(): bigint { return zero; }
export function get_One(): bigint { return one; }
export function get_MinusOne(): bigint { return minusOne; }
export function get_IsZero(x: bigint): boolean { return x === zero; }
export function get_IsOne(x: bigint): boolean { return x === one; }
export function get_IsEven(x: bigint): boolean { return isEvenInteger(x); }
export function get_IsPowerOfTwo(x: bigint): boolean { return isPow2(x); }
export function get_Sign(x: bigint): int32 { return sign(x); }

export function isNegative(x: bigint): boolean { return x < zero; }
export function isPositive(x: bigint): boolean { return x > zero; }
export function isEvenInteger(x: bigint): boolean { return (x % two) === zero; }
export function isOddInteger(x: bigint): boolean { return (x % two) !== zero; }
export function isPow2(x: bigint): boolean { return (x & (x - one)) === zero }

export function fromZero(): bigint { return zero; }
export function fromOne(): bigint { return one; }

export function fromInt8(n: int8): bigint { return BigInt(n); }
export function fromUInt8(n: uint8): bigint { return BigInt(n); }
export function fromInt16(n: int16): bigint { return BigInt(n); }
export function fromUInt16(n: uint16): bigint { return BigInt(n); }
export function fromInt32(n: int32): bigint { return BigInt(n); }
export function fromUInt32(n: uint32): bigint { return BigInt(n); }
export function fromInt64(n: int64): bigint { return n; }
export function fromUInt64(n: uint64): bigint { return n; }
export function fromInt128(n: int128): bigint { return n; }
export function fromUInt128(n: uint128): bigint { return n; }
export function fromNativeInt(n: nativeint): bigint { return n; }
export function fromUNativeInt(n: unativeint): bigint { return n; }

export function fromFloat16(n: float16): bigint { return BigInt(Math.trunc(n)); }
export function fromFloat32(n: float32): bigint { return BigInt(Math.trunc(n)); }
export function fromFloat64(n: float64): bigint { return BigInt(Math.trunc(n)); }

export function fromDecimal(d: decimal): bigint { return BigInt(truncate(d).toString()); }

export function fromBigInt(x: bigint): bigint { return x }
export function fromBoolean(b: boolean): bigint { return BigInt(b); }
export function fromChar(c: string): bigint { return BigInt(c.charCodeAt(0)); }
export function fromString(s: string): bigint { return BigInt(s); }

export function fromByteArray(bytes: ArrayLike<uint8>): bigint {
    return fromSignedBytes(bytes, isBigEndian);
}

export function toByteArray(value: bigint): number[] {
    return toSignedBytes(value, isBigEndian) as any as number[];
}

export function toInt8(x: bigint): int8 { return Number(BigInt.asIntN(8, x)); }
export function toUInt8(x: bigint): uint8 { return Number(BigInt.asUintN(8, x)); }
export function toInt16(x: bigint): int16 { return Number(BigInt.asIntN(16, x)); }
export function toUInt16(x: bigint): uint16 { return Number(BigInt.asUintN(16, x)); }
export function toInt32(x: bigint): int32 { return Number(BigInt.asIntN(32, x)); }
export function toUInt32(x: bigint): uint32 { return Number(BigInt.asUintN(32, x)); }
export function toInt64(x: bigint): int64 { return BigInt.asIntN(64, x); }
export function toUInt64(x: bigint): uint64 { return BigInt.asUintN(64, x); }
export function toInt128(x: bigint): int128 { return BigInt.asIntN(128, x); }
export function toUInt128(x: bigint): uint128 { return BigInt.asUintN(128, x); }
export function toNativeInt(x: bigint): nativeint { return BigInt.asIntN(64, x); }
export function toUNativeInt(x: bigint): unativeint { return BigInt.asUintN(64, x); }

export function toFloat16(x: bigint): float32 { return Number(x); }
export function toFloat32(x: bigint): float32 { return Number(x); }
export function toFloat64(x: bigint): float64 { return Number(x); }

export function toDecimal(x: bigint): decimal {
    const isNegative = x < zero;
    const bits = abs(x);
    const low = Number(BigInt.asUintN(32, bits));
    const mid = Number(BigInt.asUintN(32, bits >> 32n));
    const high = Number(BigInt.asUintN(32, bits >> 64n));
    const scale = 0;
    return fromParts(low, mid, high, isNegative, scale)
}

export function toBigInt(x: bigint): bigint { return x; }
export function toBoolean(x: bigint): boolean { return x !== zero; }

export function toChar(x: bigint): string {
    return String.fromCharCode(toUInt16(x))
}

export function toString(x: bigint): string { return x.toString(); }

export function tryParse(s: string, res: FSharpRef<bigint>): boolean {
    try {
        res.contents = BigInt(s);
        return true;
    }
    catch (err: any) {
        return false;
    }
}

export function parse(s: string): bigint {
    return BigInt(s);
}

export function pow(x: bigint, n: int32): bigint {
    return x ** BigInt(n);
}

export function modPow(x: bigint, e: bigint, m: bigint): bigint {
    return (x ** e) % m;
}

export function divRem(x: bigint, y: bigint): [bigint, bigint];
export function divRem(x: bigint, y: bigint, out: FSharpRef<bigint>): bigint;
export function divRem(x: bigint, y: bigint, out?: FSharpRef<bigint>): bigint | [bigint, bigint] {
  const div = x / y;
  const rem = x % y;
  if (out === void 0) {
    return [div, rem];
  } else {
    out.contents = rem;
    return div;
  }
}

export function greatestCommonDivisor(x: bigint, y: bigint): bigint {
    while (y > zero) {
        const q = x / y;
        const r = x - q * y;
        x = y;
        y = r;
    }
    return x;
}

export function getBitLength(x: bigint): int64 {
    return fromFloat64(x === zero ? 1 : log2(abs(x)) + 1);
}

export function log2(x: bigint): float64 {
    const n = Number(x);
    if (Number.isFinite(n))
        return Math.log2(n); // fast path
    if (x < zero) return Number.NaN;
    let shift = one;
    while (x >= (one << shift)) {
        shift = shift << one;
    }
    let log = zero;
    while (shift > one) {
        shift = shift >> one;
        if (x >= (one << shift)) {
            log = log + shift;
            x = x >> shift;
        }
    }
    return Number(log);
}

export function log10(x: bigint): float64 {
    return log2(x) * Math.log10(2);
}

export function ln(x: bigint): float64 {
    return log2(x) * Math.log(2);
}

export function log(x: bigint, base: float64): float64 {
    return log2(x) / Math.log2(base);
}

export function ilog2(x: bigint): bigint {
    return BigInt(log2(x));
}

// export function copySign
// export function createChecked
// export function createSaturating
// export function createTruncating
// export function getByteCount
// export function leadingZeroCount
// export function popCount
// export function rotateLeft
// export function rotateRight
// export function trailingZeroCount
// export function tryFormat
// export function tryWriteBytes

// -------------------------------------------------
// Binary serialization
// -------------------------------------------------

const hexCodes = new Uint8Array([48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 97, 98, 99, 100, 101, 102]);

function fromHexCode(code: number): number {
    if (48 <= code && code <= 57) return code - 48;
    if (97 <= code && code <= 102) return code - 97 + 10;
    if (65 <= code && code <= 70) return code - 65 + 10;
    throw Error(`Invalid hex code: ${code}`);
}

function toSignedBytes(x: bigint, isBigEndian: boolean): Uint8Array {
    const isNeg = x < 0n;
    if (isNeg) {
        const len = log2(-x);
        const bits = len + (8 - len % 8);
        const pow2 = (1n << BigInt(bits));
        x = x + pow2; // two's complement
    }
    const hex = x.toString(16);
    const len = hex.length;
    const odd = len % 2;
    const first = hex.charCodeAt(0);
    const isLow = 48 <= first && first <= 55; // 0..7
    const start = (isNeg && isLow) || (!isNeg && !isLow) ? 1 : 0;
    const bytes = new Uint8Array(start + (len + odd) / 2);
    const inc = isBigEndian ? 1 : -1;
    let pos = isBigEndian ? 0 : bytes.length - 1;
    if (start > 0) {
        bytes[pos] = isNeg ? 255 : 0;
        pos += inc;
    }
    if (odd > 0) {
        bytes[pos] = fromHexCode(first);
        pos += inc;
    }
    for (let i = odd; i < len; i += 2, pos += inc) {
        const a = fromHexCode(hex.charCodeAt(i));
        const b = fromHexCode(hex.charCodeAt(i + 1));
        bytes[pos] = (a << 4) | b;
    }
    return bytes;
}

function fromSignedBytes(bytes: ArrayLike<uint8>, isBigEndian: boolean) {
    if (bytes == null) {
        throw new Error("bytes is null");
    }
    const len = bytes.length;
    const first = isBigEndian ? 0 : len - 1;
    const isNeg = bytes[first] > 127;
    const codes = new Uint16Array(len * 2 + 2);
    codes[0] = 48;  // 0
    codes[1] = 120; // x
    const inc = isBigEndian ? 1 : -1;
    let pos = isBigEndian ? 0 : len - 1;
    for (let i = 0; i < bytes.length; i++, pos += inc) {
        const byte = bytes[pos];
        codes[2 * i + 2] = hexCodes[byte >> 4];
        codes[2 * i + 3] = hexCodes[byte & 15];
    }
    const str = String.fromCharCode.apply(null, codes as any as number[]);
    let x = BigInt(str);
    if (isNeg) {
        const bits = len * 8;
        const pow2 = (1n << BigInt(bits));
        x = x - pow2; // two's complement
    }
    return x;
}
