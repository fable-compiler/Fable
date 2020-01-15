import { isValid } from "./Int32";
import * as Long from "./lib/long";

export default Long.Long;
export type Long = Long.Long;

export const get_Zero = Long.ZERO;
export const get_One = Long.ONE;

export const op_Addition = Long.add;
export const op_Subtraction = Long.subtract;
export const op_Multiply = Long.multiply;
export const op_Division = Long.divide;
export const op_Modulus = Long.modulo;
export const op_UnaryNegation = Long.negate;

export const op_LeftShift = Long.shiftLeft;
export const op_RightShift = Long.shiftRight;
export const op_RightShiftUnsigned = Long.shiftRightUnsigned;
export const op_BitwiseAnd = Long.and;
export const op_BitwiseOr = Long.or;
export const op_ExclusiveOr = Long.xor;
export const op_LogicalNot = Long.not;

export const op_LessThan = Long.lessThan;
export const op_LessThanOrEqual = Long.lessThanOrEqual;
export const op_GreaterThan = Long.greaterThan;
export const op_GreaterThanOrEqual = Long.greaterThanOrEqual;
export const op_Equality = Long.equals;
export const op_Inequality = Long.notEquals;

export const equals = Long.equals;
export const compare = Long.compare;

export const fromInt = Long.fromInt;
export const fromBits = Long.fromBits;
export const fromBytes = Long.fromBytes;
export const fromNumber = Long.fromNumber;
export const fromString = Long.fromString;
export const fromValue = Long.fromValue;

export const toInt = Long.toInt;
export const toBytes = Long.toBytes;
export const toNumber = Long.toNumber;
export const toString = Long.toString;

export const getLowBits = Long.getLowBits;
export const getHighBits = Long.getHighBits;
export const getLowBitsUnsigned = Long.getLowBitsUnsigned;
export const getHighBitsUnsigned = Long.getHighBitsUnsigned;

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
  if (!x.unsigned && Long.isNegative(x)) {
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
  return Long.fromBits(x, xh >> 31, unsigned);
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
      return Long.fromString(str, unsigned, res.radix);
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
  return [false, Long.ZERO];
}

export function unixEpochMillisecondsToTicks(ms: number, offset: number) {
  return op_Multiply(op_Addition(op_Addition(Long.fromNumber(ms), 62135596800000), offset), 10000);
}

export function ticksToUnixEpochMilliseconds(ticks: Long) {
  return Long.toNumber(op_Subtraction(op_Division(ticks, 10000), 62135596800000));
}

export function makeRangeStepFunction(step: Long, last: Long, unsigned: boolean) {
  const stepComparedWithZero = Long.compare(step, unsigned ? Long.UZERO : Long.ZERO);
  if (stepComparedWithZero === 0) {
    throw new Error("The step of a range cannot be zero");
  }
  const stepGreaterThanZero = stepComparedWithZero > 0;
  return (x: Long) => {
    const comparedWithLast = Long.compare(x, last);
    if ((stepGreaterThanZero && comparedWithLast <= 0)
      || (!stepGreaterThanZero && comparedWithLast >= 0)) {
      return [x, op_Addition(x, step)];
    } else {
      return null;
    }
  };
}
