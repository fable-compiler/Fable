import { fromParts, truncate } from "./Decimal.js";
import { bigintHash } from "./Util.js";
const isBigEndian = false;
BigInt.prototype.toJSON = function () {
    return `${this.toString()}`;
};
const zero = 0n;
const one = 1n;
const two = 2n;
const minusOne = -1n;
export function isBigInt(x) {
    return typeof x === "bigint";
}
export function hash(x) {
    return bigintHash(x);
}
export function equals(x, y) {
    return x === y;
}
export function compare(x, y) {
    return x < y ? -1 : x > y ? 1 : 0;
}
export function abs(x) { return x < zero ? -x : x; }
export function sign(x) { return x < zero ? -1 : x > zero ? 1 : 0; }
export function max(x, y) { return x > y ? x : y; }
export function min(x, y) { return x < y ? x : y; }
export function maxMagnitude(x, y) { return abs(x) > abs(y) ? x : y; }
export function minMagnitude(x, y) { return abs(x) < abs(y) ? x : y; }
export function clamp(x, min, max) {
    return x < min ? min : x > max ? max : x;
}
export function add(x, y) { return x + y; }
export function subtract(x, y) { return x - y; }
export function multiply(x, y) { return x * y; }
export function divide(x, y) { return x / y; }
export function remainder(x, y) { return x % y; }
export function negate(x) { return -x; }
export function op_UnaryNegation(x) { return -x; }
export function op_LogicalNot(x) { return ~x; }
export function op_UnaryPlus(x) { return x; }
export function op_Addition(x, y) { return x + y; }
export function op_Subtraction(x, y) { return x - y; }
export function op_Multiply(x, y) { return x * y; }
export function op_Division(x, y) { return x / y; }
export function op_Modulus(x, y) { return x % y; }
export function op_RightShift(x, n) { return x >> BigInt(n); }
export function op_LeftShift(x, n) { return x << BigInt(n); }
export function op_BitwiseAnd(x, y) { return x & y; }
export function op_BitwiseOr(x, y) { return x | y; }
export function op_ExclusiveOr(x, y) { return x ^ y; }
export function op_LessThan(x, y) { return x < y; }
export function op_LessThanOrEqual(x, y) { return x <= y; }
export function op_GreaterThan(x, y) { return x > y; }
export function op_GreaterThanOrEqual(x, y) { return x >= y; }
export function op_Equality(x, y) { return x === y; }
export function op_Inequality(x, y) { return x !== y; }
export function get_Zero() { return zero; }
export function get_One() { return one; }
export function get_MinusOne() { return minusOne; }
export function get_IsZero(x) { return x === zero; }
export function get_IsOne(x) { return x === one; }
export function get_IsEven(x) { return isEvenInteger(x); }
export function get_IsPowerOfTwo(x) { return isPow2(x); }
export function get_Sign(x) { return sign(x); }
export function isNegative(x) { return x < zero; }
export function isPositive(x) { return x > zero; }
export function isEvenInteger(x) { return (x % two) === zero; }
export function isOddInteger(x) { return (x % two) !== zero; }
export function isPow2(x) { return (x & (x - one)) === zero; }
export function fromZero() { return zero; }
export function fromOne() { return one; }
export function fromInt8(n) { return BigInt(n); }
export function fromUInt8(n) { return BigInt(n); }
export function fromInt16(n) { return BigInt(n); }
export function fromUInt16(n) { return BigInt(n); }
export function fromInt32(n) { return BigInt(n); }
export function fromUInt32(n) { return BigInt(n); }
export function fromInt64(n) { return n; }
export function fromUInt64(n) { return n; }
export function fromInt128(n) { return n; }
export function fromUInt128(n) { return n; }
export function fromNativeInt(n) { return n; }
export function fromUNativeInt(n) { return n; }
export function fromFloat16(n) { return BigInt(Math.trunc(n)); }
export function fromFloat32(n) { return BigInt(Math.trunc(n)); }
export function fromFloat64(n) { return BigInt(Math.trunc(n)); }
export function fromDecimal(d) { return BigInt(truncate(d).toString()); }
export function fromBigInt(x) { return x; }
export function fromBoolean(b) { return BigInt(b); }
export function fromChar(c) { return BigInt(c.charCodeAt(0)); }
export function fromString(s) { return BigInt(s); }
export function fromByteArray(bytes) {
    return fromSignedBytes(bytes, isBigEndian);
}
export function toByteArray(value) {
    return toSignedBytes(value, isBigEndian);
}
export function toInt8(x) { return Number(BigInt.asIntN(8, x)); }
export function toUInt8(x) { return Number(BigInt.asUintN(8, x)); }
export function toInt16(x) { return Number(BigInt.asIntN(16, x)); }
export function toUInt16(x) { return Number(BigInt.asUintN(16, x)); }
export function toInt32(x) { return Number(BigInt.asIntN(32, x)); }
export function toUInt32(x) { return Number(BigInt.asUintN(32, x)); }
export function toInt64(x) { return BigInt.asIntN(64, x); }
export function toUInt64(x) { return BigInt.asUintN(64, x); }
export function toInt128(x) { return BigInt.asIntN(128, x); }
export function toUInt128(x) { return BigInt.asUintN(128, x); }
export function toNativeInt(x) { return BigInt.asIntN(64, x); }
export function toUNativeInt(x) { return BigInt.asUintN(64, x); }
export function toFloat16(x) { return Number(x); }
export function toFloat32(x) { return Number(x); }
export function toFloat64(x) { return Number(x); }
export function toDecimal(x) {
    const low = Number(BigInt.asUintN(32, x));
    const mid = Number(BigInt.asUintN(32, x >> 32n));
    const high = Number(BigInt.asUintN(32, x >> 64n));
    const isNegative = x < zero;
    const scale = 0;
    return fromParts(low, mid, high, isNegative, scale);
}
export function toBigInt(x) { return x; }
export function toBoolean(x) { return x !== zero; }
export function toChar(x) {
    return String.fromCharCode(toUInt16(x));
}
export function toString(x) { return x.toString(); }
export function tryParse(s, res) {
    try {
        res.contents = BigInt(s);
        return true;
    }
    catch (err) {
        return false;
    }
}
export function parse(s) {
    return BigInt(s);
}
export function pow(x, n) {
    return x ** BigInt(n);
}
export function modPow(x, e, m) {
    return (x ** e) % m;
}
export function divRem(x, y, out) {
    const div = x / y;
    const rem = x % y;
    if (out === void 0) {
        return [div, rem];
    }
    else {
        out.contents = rem;
        return div;
    }
}
export function greatestCommonDivisor(x, y) {
    while (y > zero) {
        const q = x / y;
        const r = x - q * y;
        x = y;
        y = r;
    }
    return x;
}
export function getBitLength(x) {
    return fromFloat64(x === zero ? 1 : log2(abs(x)) + 1);
}
export function log2(x) {
    const n = Number(x);
    if (Number.isFinite(n))
        return Math.log2(n); // fast path
    if (x < zero)
        return Number.NaN;
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
export function log10(x) {
    return log2(x) * Math.log10(2);
}
export function ln(x) {
    return log2(x) * Math.log(2);
}
export function log(x, base) {
    return log2(x) / Math.log2(base);
}
export function ilog2(x) {
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
function fromHexCode(code) {
    if (48 <= code && code <= 57)
        return code - 48;
    if (97 <= code && code <= 102)
        return code - 97 + 10;
    if (65 <= code && code <= 70)
        return code - 65 + 10;
    throw Error(`Invalid hex code: ${code}`);
}
function toSignedBytes(x, isBigEndian) {
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
function fromSignedBytes(bytes, isBigEndian) {
    if (bytes == null) {
        throw new Error("bytes is null");
    }
    const len = bytes.length;
    const first = isBigEndian ? 0 : len - 1;
    const isNeg = bytes[first] > 127;
    const codes = new Uint16Array(len * 2 + 2);
    codes[0] = 48; // 0
    codes[1] = 120; // x
    const inc = isBigEndian ? 1 : -1;
    let pos = isBigEndian ? 0 : len - 1;
    for (let i = 0; i < bytes.length; i++, pos += inc) {
        const byte = bytes[pos];
        codes[2 * i + 2] = hexCodes[byte >> 4];
        codes[2 * i + 3] = hexCodes[byte & 15];
    }
    const str = String.fromCharCode.apply(null, codes);
    let x = BigInt(str);
    if (isNeg) {
        const bits = len * 8;
        const pow2 = (1n << BigInt(bits));
        x = x - pow2; // two's complement
    }
    return x;
}
