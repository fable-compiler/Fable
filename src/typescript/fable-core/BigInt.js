import { setType } from "./Symbol";
import _Symbol from "./Symbol";
import { toString as toString_1, Array as _Array } from "./Util";
import { factorial as bigNatFactorial, ofString, toFloat, toUInt64 as bigNattoUInt64, toUInt32 as bigNattoUInt32, pow as bigNatPow, two as bigNatTwo, rem, lte, hcf, bitOr, bitAnd, divmod, mul, isOne, sub, gte, scale as bigNatScale, one as bigNatOne, add, ofInt64, toString, hash as bigNatHash, gt, lt, isZero, equal, getSmall, isSmall, ofInt32 } from "./BigInt/BigNat";
import BigNat from "./BigInt/BigNat";
import { initialize } from "./Seq";
import { fromBits, fromNumber } from "./Long";
import { trim } from "./String";

export default class BigInteger {
  [_Symbol.reflection]() {
    return {
      type: "System.Numerics.BigInteger",
      interfaces: ["FSharpRecord", "System.IComparable"],
      properties: {
        signInt: "number",
        v: BigNat
      }
    };
  }

  constructor(signInt, v) {
    this.signInt = signInt;
    this.v = v;
  }

  get Sign() {
    if (this.IsZero) {
      return 0;
    } else {
      return this.signInt;
    }
  }

  get SignInt() {
    return this.signInt;
  }

  get V() {
    return this.v;
  }

  get IsZero() {
    if (this.SignInt === 0) {
      return true;
    } else {
      return isZero(this.V);
    }
  }

  get IsOne() {
    if (this.SignInt === 1) {
      return isOne(this.V);
    } else {
      return false;
    }
  }

  get StructuredDisplayString() {
    return toString_1(this);
  }

  get IsSmall() {
    if (this.IsZero) {
      return true;
    } else {
      return isSmall(this.V);
    }
  }

  get IsNegative() {
    if (this.SignInt === -1) {
      return !this.IsZero;
    } else {
      return false;
    }
  }

  get IsPositive() {
    if (this.SignInt === 1) {
      return !this.IsZero;
    } else {
      return false;
    }
  }

  CompareTo(obj) {
    if (obj instanceof BigInteger) {
      const that = obj;
      return compare(this, that);
    } else {
      throw new Error("the objects are not comparable" + '\nParameter name: ' + "obj");
    }
  }

  ToString() {
    const matchValue = this.SignInt;
    let $var19 = null;

    switch (matchValue) {
      case 1:
        $var19 = toString(this.V);
        break;

      case -1:
        if (isZero(this.V)) {
          $var19 = "0";
        } else {
          $var19 = "-" + toString(this.V);
        }
        break;

      case 0:
        $var19 = "0";
        break;

      default:
        throw new Error("signs should be +/- 1 or 0");
    }

    return $var19;
  }

  Equals(obj) {
    if (obj instanceof BigInteger) {
      const that = obj;
      return op_Equality(this, that);
    } else {
      return false;
    }
  }

  GetHashCode() {
    return hash(this);
  }
}
setType("System.Numerics.BigInteger", BigInteger);

const smallLim = 4096;
const smallPosTab = Array.from(initialize(smallLim, n => ofInt32(n)));
export const one = fromInt32(1);
export const two = fromInt32(2);
export const zero = fromInt32(0);

export function fromInt32(n) {
  if (n >= 0) {
    return new BigInteger(1, nat(ofInt32(n)));
  } else if (n === -2147483648) {
    return new BigInteger(-1, nat(ofInt64(fromNumber(n, false).neg())));
  } else {
    return new BigInteger(-1, nat(ofInt32(-n)));
  }
}

export function fromInt64(n) {
  if (n.CompareTo(fromBits(0, 0, false)) >= 0) {
    return new BigInteger(1, nat(ofInt64(n)));
  } else if (n.Equals(fromBits(0, 2147483648, false))) {
    return new BigInteger(-1, nat(add(ofInt64(fromBits(4294967295, 2147483647, false)), bigNatOne)));
  } else {
    return new BigInteger(-1, nat(ofInt64(n.neg())));
  }
}

export function nat(n) {
  if (isSmall(n) ? getSmall(n) < smallLim : false) {
    return smallPosTab[getSmall(n)];
  } else {
    return n;
  }
}

export function create(s, n) {
  return new BigInteger(s, nat(n));
}

export function posn(n) {
  return new BigInteger(1, nat(n));
}

export function negn(n) {
  return new BigInteger(-1, nat(n));
}

export function op_Equality(x, y) {
  const matchValue = [x.SignInt, y.SignInt];

  if (matchValue[0] === -1) {
    if (matchValue[1] === -1) {
      return equal(x.V, y.V);
    } else if (matchValue[1] === 0) {
      return isZero(x.V);
    } else if (matchValue[1] === 1) {
      if (isZero(x.V)) {
        return isZero(y.V);
      } else {
        return false;
      }
    } else {
      throw new Error("signs should be +/- 1 or 0" + '\nParameter name: ' + "x");
    }
  } else if (matchValue[0] === 0) {
    if (matchValue[1] === -1) {
      return isZero(y.V);
    } else if (matchValue[1] === 0) {
      return true;
    } else if (matchValue[1] === 1) {
      return isZero(y.V);
    } else {
      throw new Error("signs should be +/- 1 or 0" + '\nParameter name: ' + "x");
    }
  } else if (matchValue[0] === 1) {
    if (matchValue[1] === -1) {
      if (isZero(x.V)) {
        return isZero(y.V);
      } else {
        return false;
      }
    } else if (matchValue[1] === 0) {
      return isZero(x.V);
    } else if (matchValue[1] === 1) {
      return equal(x.V, y.V);
    } else {
      throw new Error("signs should be +/- 1 or 0" + '\nParameter name: ' + "x");
    }
  } else {
    throw new Error("signs should be +/- 1 or 0" + '\nParameter name: ' + "x");
  }
}

export function op_Inequality(x, y) {
  return !op_Equality(x, y);
}

export function op_LessThan(x, y) {
  const matchValue = [x.SignInt, y.SignInt];

  if (matchValue[0] === -1) {
    if (matchValue[1] === -1) {
      return lt(y.V, x.V);
    } else if (matchValue[1] === 0) {
      return !isZero(x.V);
    } else if (matchValue[1] === 1) {
      if (!isZero(x.V)) {
        return true;
      } else {
        return !isZero(y.V);
      }
    } else {
      throw new Error("signs should be +/- 1 or 0" + '\nParameter name: ' + "x");
    }
  } else if (matchValue[0] === 0) {
    if (matchValue[1] === -1) {
      return false;
    } else if (matchValue[1] === 0) {
      return false;
    } else if (matchValue[1] === 1) {
      return !isZero(y.V);
    } else {
      throw new Error("signs should be +/- 1 or 0" + '\nParameter name: ' + "x");
    }
  } else if (matchValue[0] === 1) {
    if (matchValue[1] === -1) {
      return false;
    } else if (matchValue[1] === 0) {
      return false;
    } else if (matchValue[1] === 1) {
      return lt(x.V, y.V);
    } else {
      throw new Error("signs should be +/- 1 or 0" + '\nParameter name: ' + "x");
    }
  } else {
    throw new Error("signs should be +/- 1 or 0" + '\nParameter name: ' + "x");
  }
}

export function op_GreaterThan(x, y) {
  const matchValue = [x.SignInt, y.SignInt];

  if (matchValue[0] === -1) {
    if (matchValue[1] === -1) {
      return gt(y.V, x.V);
    } else if (matchValue[1] === 0) {
      return false;
    } else if (matchValue[1] === 1) {
      return false;
    } else {
      throw new Error("signs should be +/- 1 or 0" + '\nParameter name: ' + "x");
    }
  } else if (matchValue[0] === 0) {
    if (matchValue[1] === -1) {
      return !isZero(y.V);
    } else if (matchValue[1] === 0) {
      return false;
    } else if (matchValue[1] === 1) {
      return false;
    } else {
      throw new Error("signs should be +/- 1 or 0" + '\nParameter name: ' + "x");
    }
  } else if (matchValue[0] === 1) {
    if (matchValue[1] === -1) {
      if (!isZero(x.V)) {
        return true;
      } else {
        return !isZero(y.V);
      }
    } else if (matchValue[1] === 0) {
      return !isZero(x.V);
    } else if (matchValue[1] === 1) {
      return gt(x.V, y.V);
    } else {
      throw new Error("signs should be +/- 1 or 0" + '\nParameter name: ' + "x");
    }
  } else {
    throw new Error("signs should be +/- 1 or 0" + '\nParameter name: ' + "x");
  }
}

export function compare(n, nn) {
  if (op_LessThan(n, nn)) {
    return -1;
  } else if (op_Equality(n, nn)) {
    return 0;
  } else {
    return 1;
  }
}

export function hash(z) {
  if (z.SignInt === 0) {
    return 1;
  } else {
    return z.SignInt + hash(z.V);
  }
}

export function op_UnaryNegation(z) {
  const matchValue = z.SignInt;

  if (matchValue === 0) {
    return zero;
  } else {
    return create(-matchValue, z.V);
  }
}

export function scale(k, z) {
  if (z.SignInt === 0) {
    return zero;
  } else if (k < 0) {
    return create(-z.SignInt, bigNatScale(-k, z.V));
  } else {
    return create(z.SignInt, bigNatScale(k, z.V));
  }
}

export function subnn(nx, ny) {
  if (gte(nx, ny)) {
    return posn(sub(nx, ny));
  } else {
    return negn(sub(ny, nx));
  }
}

export function addnn(nx, ny) {
  return posn(add(nx, ny));
}

export function op_Addition(x, y) {
  if (y.IsZero) {
    return x;
  } else if (x.IsZero) {
    return y;
  } else {
    const matchValue = [x.SignInt, y.SignInt];

    if (matchValue[0] === -1) {
      if (matchValue[1] === -1) {
        return op_UnaryNegation(addnn(x.V, y.V));
      } else if (matchValue[1] === 1) {
        return subnn(y.V, x.V);
      } else {
        throw new Error("signs should be +/- 1" + '\nParameter name: ' + "x");
      }
    } else if (matchValue[0] === 1) {
      if (matchValue[1] === -1) {
        return subnn(x.V, y.V);
      } else if (matchValue[1] === 1) {
        return addnn(x.V, y.V);
      } else {
        throw new Error("signs should be +/- 1" + '\nParameter name: ' + "x");
      }
    } else {
      throw new Error("signs should be +/- 1" + '\nParameter name: ' + "x");
    }
  }
}

export function op_Subtraction(x, y) {
  if (y.IsZero) {
    return x;
  } else if (x.IsZero) {
    return op_UnaryNegation(y);
  } else {
    const matchValue = [x.SignInt, y.SignInt];

    if (matchValue[0] === -1) {
      if (matchValue[1] === -1) {
        return subnn(y.V, x.V);
      } else if (matchValue[1] === 1) {
        return op_UnaryNegation(addnn(x.V, y.V));
      } else {
        throw new Error("signs should be +/- 1" + '\nParameter name: ' + "x");
      }
    } else if (matchValue[0] === 1) {
      if (matchValue[1] === -1) {
        return addnn(x.V, y.V);
      } else if (matchValue[1] === 1) {
        return subnn(x.V, y.V);
      } else {
        throw new Error("signs should be +/- 1" + '\nParameter name: ' + "x");
      }
    } else {
      throw new Error("signs should be +/- 1" + '\nParameter name: ' + "x");
    }
  }
}

export function op_Multiply(x, y) {
  if (x.IsZero) {
    return x;
  } else if (y.IsZero) {
    return y;
  } else if (x.IsOne) {
    return y;
  } else if (y.IsOne) {
    return x;
  } else {
    const m = mul(x.V, y.V);
    return create(x.SignInt * y.SignInt, m);
  }
}

export function divRem(x, y) {
  if (y.IsZero) {
    throw new Error();
  }

  if (x.IsZero) {
    return [zero, zero];
  } else {
    const patternInput = divmod(x.V, y.V);
    const matchValue = [x.SignInt, y.SignInt];

    if (matchValue[0] === -1) {
      if (matchValue[1] === -1) {
        return [posn(patternInput[0]), negn(patternInput[1])];
      } else if (matchValue[1] === 1) {
        return [negn(patternInput[0]), negn(patternInput[1])];
      } else {
        throw new Error("signs should be +/- 1" + '\nParameter name: ' + "x");
      }
    } else if (matchValue[0] === 1) {
      if (matchValue[1] === -1) {
        return [negn(patternInput[0]), posn(patternInput[1])];
      } else if (matchValue[1] === 1) {
        return [posn(patternInput[0]), posn(patternInput[1])];
      } else {
        throw new Error("signs should be +/- 1" + '\nParameter name: ' + "x");
      }
    } else {
      throw new Error("signs should be +/- 1" + '\nParameter name: ' + "x");
    }
  }
}

export function op_Division(x, y) {
  return divRem(x, y)[0];
}

export function op_Modulus(x, y) {
  return divRem(x, y)[1];
}

export function op_RightShift(x, y) {
  return op_Division(x, pow(two, y));
}

export function op_LeftShift(x, y) {
  return op_Multiply(x, pow(two, y));
}

export function op_BitwiseAnd(x, y) {
  return posn(bitAnd(x.V, y.V));
}

export function op_BitwiseOr(x, y) {
  return posn(bitOr(x.V, y.V));
}

export function greatestCommonDivisor(x, y) {
  const matchValue = [x.SignInt, y.SignInt];

  if (matchValue[0] === 0) {
    if (matchValue[1] === 0) {
      return zero;
    } else {
      return posn(y.V);
    }
  } else if (matchValue[1] === 0) {
    return posn(x.V);
  } else {
    return posn(hcf(x.V, y.V));
  }
}

export function abs(x) {
  if (x.SignInt === -1) {
    return op_UnaryNegation(x);
  } else {
    return x;
  }
}

export function op_LessThanOrEqual(x, y) {
  const matchValue = [x.SignInt, y.SignInt];

  if (matchValue[0] === -1) {
    if (matchValue[1] === -1) {
      return lte(y.V, x.V);
    } else if (matchValue[1] === 0) {
      return true;
    } else if (matchValue[1] === 1) {
      return true;
    } else {
      throw new Error("signs should be +/- 1 or 0" + '\nParameter name: ' + "x");
    }
  } else if (matchValue[0] === 0) {
    if (matchValue[1] === -1) {
      return isZero(y.V);
    } else if (matchValue[1] === 0) {
      return true;
    } else if (matchValue[1] === 1) {
      return true;
    } else {
      throw new Error("signs should be +/- 1 or 0" + '\nParameter name: ' + "x");
    }
  } else if (matchValue[0] === 1) {
    if (matchValue[1] === -1) {
      if (isZero(x.V)) {
        return isZero(y.V);
      } else {
        return false;
      }
    } else if (matchValue[1] === 0) {
      return isZero(x.V);
    } else if (matchValue[1] === 1) {
      return lte(x.V, y.V);
    } else {
      throw new Error("signs should be +/- 1 or 0" + '\nParameter name: ' + "x");
    }
  } else {
    throw new Error("signs should be +/- 1 or 0" + '\nParameter name: ' + "x");
  }
}

export function op_GreaterThanOrEqual(x, y) {
  const matchValue = [x.SignInt, y.SignInt];

  if (matchValue[0] === -1) {
    if (matchValue[1] === -1) {
      return gte(y.V, x.V);
    } else if (matchValue[1] === 0) {
      return isZero(x.V);
    } else if (matchValue[1] === 1) {
      if (isZero(x.V)) {
        return isZero(y.V);
      } else {
        return false;
      }
    } else {
      throw new Error("signs should be +/- 1 or 0" + '\nParameter name: ' + "x");
    }
  } else if (matchValue[0] === 0) {
    if (matchValue[1] === -1) {
      return true;
    } else if (matchValue[1] === 0) {
      return true;
    } else if (matchValue[1] === 1) {
      return isZero(y.V);
    } else {
      throw new Error("signs should be +/- 1 or 0" + '\nParameter name: ' + "x");
    }
  } else if (matchValue[0] === 1) {
    if (matchValue[1] === -1) {
      return true;
    } else if (matchValue[1] === 0) {
      return true;
    } else if (matchValue[1] === 1) {
      return gte(x.V, y.V);
    } else {
      throw new Error("signs should be +/- 1 or 0" + '\nParameter name: ' + "x");
    }
  } else {
    throw new Error("signs should be +/- 1 or 0" + '\nParameter name: ' + "x");
  }
}

export function toSByte(x) {
  return (toInt32(x) + 0x80 & 0xFF) - 0x80;
}

export function toByte(x) {
  return toUInt32(x) & 0xFF;
}

export function toInt16(x) {
  return (toInt32(x) + 0x8000 & 0xFFFF) - 0x8000;
}

export function toUInt16(x) {
  return toUInt32(x) & 0xFFFF;
}

export function toInt32(x) {
  if (x.IsZero) {
    return 0;
  } else {
    const u = bigNattoUInt32(x.V);

    if (u <= 2147483647 >>> 0) {
      return x.SignInt * ~~u;
    } else if (x.SignInt === -1 ? u === 2147483647 + 1 >>> 0 : false) {
      return -2147483648;
    } else {
      throw new Error();
    }
  }
}

export function toUInt32(x) {
  if (x.IsZero) {
    return 0;
  } else {
    return bigNattoUInt32(x.V);
  }
}

export function toInt64(x) {
  if (x.IsZero) {
    return fromBits(0, 0, false);
  } else {
    const u = bigNattoUInt64(x.V);

    if (u.CompareTo(fromBits(4294967295, 2147483647, false)) <= 0) {
      return fromNumber(x.SignInt, false).mul(u);
    } else if (x.SignInt === -1 ? u.Equals(fromBits(4294967295, 2147483647, false).add(fromBits(1, 0, false))) : false) {
      return fromBits(0, 2147483648, false);
    } else {
      throw new Error();
    }
  }
}

export function toUInt64(x) {
  if (x.IsZero) {
    return fromBits(0, 0, true);
  } else {
    return bigNattoUInt64(x.V);
  }
}

export function toDouble(x) {
  const matchValue = x.SignInt;
  let $var20 = null;

  switch (matchValue) {
    case 1:
      $var20 = toFloat(x.V);
      break;

    case -1:
      $var20 = -toFloat(x.V);
      break;

    case 0:
      $var20 = 0;
      break;

    default:
      throw new Error("signs should be +/- 1 or 0" + '\nParameter name: ' + "x");
  }

  return $var20;
}

export function toSingle(x) {
  return Math.fround(toDouble(x));
}

export function toDecimal(x) {
  return toDouble(x); //TODO: fix when decimal is implemented
}

export function parse(text) {
  if (text == null) {
    throw new Error("text");
  }

  const text_1 = trim(text, "both");
  const len = text_1.length;

  if (len === 0) {
    throw new Error();
  }

  const matchValue = [text_1[0], len];

  if (matchValue[0] === "+") {
    if (matchValue[1] === 1) {
      throw new Error();
    } else {
      return posn(ofString(text_1.slice(1, len - 1 + 1)));
    }
  } else if (matchValue[0] === "-") {
    if (matchValue[1] === 1) {
      throw new Error();
    } else {
      return negn(ofString(text_1.slice(1, len - 1 + 1)));
    }
  } else {
    return posn(ofString(text_1));
  }
}

export function factorial(x) {
  if (x.IsNegative) {
    throw new Error("mustBeNonNegative" + '\nParameter name: ' + "x");
  }

  if (x.IsPositive) {
    return posn(bigNatFactorial(x.V));
  } else {
    return one;
  }
}

export function op_UnaryPlus(n1) {
  return n1;
}

export function pow(x, y) {
  if (y < 0) {
    throw new Error("y");
  }

  const matchValue = [x.IsZero, y];

  if (matchValue[0]) {
    if (matchValue[1] === 0) {
      return one;
    } else {
      return zero;
    }
  } else {
    const yval = fromInt32(y);
    return create(isZero(rem(yval.V, bigNatTwo)) ? 1 : x.SignInt, bigNatPow(x.V, yval.V));
  }
}
