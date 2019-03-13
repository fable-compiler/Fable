import Decimal from "./lib/big";

export default Decimal;

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

export function abs(x: Decimal) {
  return x.abs();
}

export function round(x: Decimal, digits: number = 0) {
  return x.round(digits, x.cmp(0) >= 0 ? 1 /* ROUND_HALF_UP */ : 2 /* ROUND_HALF_EVEN */);
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

export function op_Subtraction(x: Decimal, y: Decimal) {
  return x.sub(y);
}

export function op_Modulus(x: Decimal, y: Decimal) {
  return x.mod(y);
}

export function op_Addition(x: Decimal, y: Decimal) {
  return x.add(y);
}

export function op_Division(x: Decimal, y: Decimal) {
  return x.div(y);
}

export function op_Multiply(x: Decimal, y: Decimal) {
  return x.mul(y);
}

export function op_UnaryNegation(x: Decimal) {
  const x2 = new Decimal(x);
  x2.s = -x2.s || 0;
  return x2;
}

export function toString(x: Decimal) {
  return x.toString();
}

export function tryParse(str: string): [boolean, Decimal] {
  try {
    return [true, new Decimal(str.trim())];
  } catch {
    return [false, get_Zero];
  }
}

export function parse(str: string): Decimal {
  const [ok, value] = tryParse(str);
  if (ok) {
    return value;
  } else {
    throw new Error("Input string was not in a correct format.");
  }
}

export function toNumber(x: Decimal) {
  return +x;
}

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

export function fromIntArray(bits: number[]) {
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
  const pos = decStr.indexOf(".");
  const scale = (pos < 0) ? 0 : decStr.length - pos - 1;
  const signExp = ((scale & 0x7F) << 16) * d.s;
  return [low, mid, high, signExp];
}
