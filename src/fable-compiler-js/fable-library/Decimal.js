import Decimal from "./lib/big.js";
import { symbol } from "./Numeric.js";
import { FSharpRef } from "./Types.js";
import { combineHashCodes } from "./Util.js";
Decimal.prototype.GetHashCode = function () {
    return combineHashCodes([this.s, this.e].concat(this.c));
};
Decimal.prototype.Equals = function (x) {
    return !this.cmp(x);
};
Decimal.prototype.CompareTo = function (x) {
    return this.cmp(x);
};
Decimal.prototype[symbol] = function () {
    const _this = this;
    return {
        multiply: (y) => _this.mul(y),
        toPrecision: (sd) => _this.toPrecision(sd),
        toExponential: (dp) => _this.toExponential(dp),
        toFixed: (dp) => _this.toFixed(dp),
        toHex: () => (Number(_this) >>> 0).toString(16),
    };
};
export default Decimal;
export const get_Zero = new Decimal(0);
export const get_One = new Decimal(1);
export const get_MinusOne = new Decimal(-1);
export const get_MaxValue = new Decimal("79228162514264337593543950335");
export const get_MinValue = new Decimal("-79228162514264337593543950335");
export function compare(x, y) {
    return x.cmp(y);
}
export function equals(x, y) {
    return !x.cmp(y);
}
export function abs(x) { return x.abs(); }
export function sign(x) { return x < get_Zero ? -1 : x > get_Zero ? 1 : 0; }
export function max(x, y) { return x > y ? x : y; }
export function min(x, y) { return x < y ? x : y; }
export function maxMagnitude(x, y) { return abs(x) > abs(y) ? x : y; }
export function minMagnitude(x, y) { return abs(x) < abs(y) ? x : y; }
export function clamp(x, min, max) {
    return x < min ? min : x > max ? max : x;
}
export function round(x, digits = 0) {
    return x.round(digits, 2 /* ROUND_HALF_EVEN */);
}
export function truncate(x) {
    return x.round(0, 0 /* ROUND_DOWN */);
}
export function ceiling(x) {
    return x.round(0, x.cmp(0) >= 0 ? 3 /* ROUND_UP */ : 0 /* ROUND_DOWN */);
}
export function floor(x) {
    return x.round(0, x.cmp(0) >= 0 ? 0 /* ROUND_DOWN */ : 3 /* ROUND_UP */);
}
export function pow(x, n) {
    return x.pow(n);
}
export function sqrt(x) {
    return x.sqrt();
}
export function op_Addition(x, y) {
    return x.add(y);
}
export function op_Subtraction(x, y) {
    return x.sub(y);
}
export function op_Multiply(x, y) {
    return x.mul(y);
}
export function op_Division(x, y) {
    return x.div(y);
}
export function op_Modulus(x, y) {
    return x.mod(y);
}
export function op_UnaryNegation(x) {
    const x2 = new Decimal(x);
    x2.s = -x2.s || 0;
    return x2;
}
export function op_UnaryPlus(x) {
    return x;
}
export const add = op_Addition;
export const subtract = op_Subtraction;
export const multiply = op_Multiply;
export const divide = op_Division;
export const remainder = op_Modulus;
export const negate = op_UnaryNegation;
export function toString(x) {
    return x.toString();
}
export function tryParse(str, defValue) {
    try {
        defValue.contents = new Decimal(str.trim());
        return true;
    }
    catch {
        return false;
    }
}
export function parse(str) {
    const defValue = new FSharpRef(get_Zero);
    if (tryParse(str, defValue)) {
        return defValue.contents;
    }
    else {
        throw new Error(`The input string ${str} was not in a correct format.`);
    }
}
export function toNumber(x) {
    return +x;
}
function decimalToHex(dec, bitSize) {
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
function hexToDecimal(hex, bitSize) {
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
function setInt32Bits(hexDigits, bits, offset) {
    for (let i = 0; i < 8; i++) {
        hexDigits[offset + i] = (bits >> (i * 4)) & 0xF;
    }
}
function getInt32Bits(hexDigits, offset) {
    let bits = 0;
    for (let i = 0; i < 8; i++) {
        bits = bits | (hexDigits[offset + i] << (i * 4));
    }
    return bits;
}
export function fromIntArray(bits) {
    return fromInts(bits[0], bits[1], bits[2], bits[3]);
}
export function fromInts(low, mid, high, signExp) {
    const isNegative = signExp < 0;
    const scale = (signExp >> 16) & 0x7F;
    return fromParts(low, mid, high, isNegative, scale);
}
export function fromParts(low, mid, high, isNegative, scale) {
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
export function getBits(d) {
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
