import { isValid } from "./Int32";
import * as LongLib from "./lib/long";

export default LongLib.Long;
export type Long = LongLib.Long;
export type int64 = Long;
export type uint64 = Long;

export const get_Zero = LongLib.ZERO;
export const get_One = LongLib.ONE;

export const op_Addition = LongLib.add;
export const op_Subtraction = LongLib.subtract;
export const op_Multiply = LongLib.multiply;
export const op_Division = LongLib.divide;
export const op_Modulus = LongLib.modulo;
export const op_UnaryNegation = LongLib.negate;

export const op_LeftShift = LongLib.shiftLeft;
export const op_RightShift = LongLib.shiftRight;
export const op_RightShiftUnsigned = LongLib.shiftRightUnsigned;
export const op_BitwiseAnd = LongLib.and;
export const op_BitwiseOr = LongLib.or;
export const op_ExclusiveOr = LongLib.xor;
export const op_LogicalNot = LongLib.not;

export const op_LessThan = LongLib.lessThan;
export const op_LessThanOrEqual = LongLib.lessThanOrEqual;
export const op_GreaterThan = LongLib.greaterThan;
export const op_GreaterThanOrEqual = LongLib.greaterThanOrEqual;
export const op_Equality = LongLib.equals;
export const op_Inequality = LongLib.notEquals;

export const equals = LongLib.equals;
export const compare = LongLib.compare;

export const fromInt = LongLib.fromInt;
export const fromBits = LongLib.fromBits;
export const fromBytes = LongLib.fromBytes;
export const fromNumber = LongLib.fromNumber;
export const fromString = LongLib.fromString;
export const fromValue = LongLib.fromValue;

export const toInt = LongLib.toInt;
export const toBytes = LongLib.toBytes;
export const toNumber = LongLib.toNumber;
export const toString = LongLib.toString;

export const getLowBits = LongLib.getLowBits;
export const getHighBits = LongLib.getHighBits;
export const getLowBitsUnsigned = LongLib.getLowBitsUnsigned;
export const getHighBitsUnsigned = LongLib.getHighBitsUnsigned;

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

export function abs(x: Long) {
  if (!x.unsigned && LongLib.isNegative(x)) {
    return op_UnaryNegation(x);
  } else {
    return x;
  }
}

export function fromInteger(value: number, unsigned?: boolean, kind?: number) {
  let x = value;
  let xh = 0;
  switch (kind) {
    case 0: x = value << 24 >> 24; xh = x; break;
    case 4: x = value << 24 >>> 24; break;
    case 1: x = value << 16 >> 16; xh = x; break;
    case 5: x = value << 16 >>> 16; break;
    case 2: x = value >> 0; xh = x; break;
    case 6: x = value >>> 0; break;
  }
  return LongLib.fromBits(x, xh >> 31, unsigned);
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
      str = isNegative ? res.sign + res.digits : res.digits;
      return LongLib.fromString(str, unsigned, res.radix);
    }
  }
  throw new Error("Input string was not in a correct format.");
}

export function tryParse(str: string, style: number, unsigned: boolean, bitsize: number) {
  try {
    const v = parse(str, style, unsigned, bitsize);
    return [true, v];
  } catch {
    // supress error
  }
  return [false, LongLib.ZERO];
}

export function unixEpochMillisecondsToTicks(ms: number, offset: number) {
  return op_Multiply(op_Addition(op_Addition(LongLib.fromNumber(ms), 62135596800000), offset), 10000);
}

export function ticksToUnixEpochMilliseconds(ticks: Long) {
  return LongLib.toNumber(op_Subtraction(op_Division(ticks, 10000), 62135596800000));
}

export function makeRangeStepFunction(step: Long, last: Long, unsigned: boolean) {
  const stepComparedWithZero = LongLib.compare(step, unsigned ? LongLib.UZERO : LongLib.ZERO);
  if (stepComparedWithZero === 0) {
    throw new Error("The step of a range cannot be zero");
  }
  const stepGreaterThanZero = stepComparedWithZero > 0;
  return (x: Long) => {
    const comparedWithLast = LongLib.compare(x, last);
    if ((stepGreaterThanZero && comparedWithLast <= 0)
      || (!stepGreaterThanZero && comparedWithLast >= 0)) {
      return [x, op_Addition(x, step)];
    } else {
      return undefined;
    }
  };
}
