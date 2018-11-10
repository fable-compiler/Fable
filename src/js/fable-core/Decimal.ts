import Decimal from "./lib/decimal";

export default Decimal;

export function abs(x: Decimal) {
  return x.abs();
}

export function compare(x: Decimal, y: Decimal) {
  return x.cmp(y);
}

export function op_Division(x: Decimal, y: Decimal) {
  return x.div(y);
}

export function equals(x: Decimal, y: Decimal) {
  return !x.cmp(y);
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

export function pow(x: Decimal, n: number) {
  return x.pow(n);
}

export function sqrt(x: Decimal) {
  return x.sqrt();
}

export function op_Multiply(x: Decimal, y: Decimal) {
  return x.mul(y);
}

export function toString(x: Decimal) {
  return x.toString();
}

// tslint:disable
// From https://github.com/bridgedotnet/Bridge/blob/e99e7eab5eda0f9ef74e11fbc3aebd3c24e8c0b1/Bridge/Resources/Decimal.js#L516
// https://github.com/bridgedotnet/Bridge/blob/master/LICENSE.md
// tslint:enable
export function getBytes(x: Decimal) {
  const s = (x as any).s;
  const e = (x as any).e;
  const d = (x as any).d;
  const bytes = new Uint8Array(23);

  bytes[0] = s & 255;
  bytes[1] = e;

  if (d && d.length > 0) {
    bytes[2] = d.length * 4;

    for (let i = 0; i < d.length; i++) {
      bytes[i * 4 + 3] = d[i] & 255;
      bytes[i * 4 + 4] = (d[i] >> 8) & 255;
      bytes[i * 4 + 5] = (d[i] >> 16) & 255;
      bytes[i * 4 + 6] = (d[i] >> 24) & 255;
    }
  } else {
    bytes[2] = 0;
  }

  return bytes;
}

export function fromBytes(bytes: number[]) {
  const value = new Decimal(0);
  const s = bytes[0] & 255;
  const e = bytes[1];
  const ln = bytes[2];
  const d = [];

  (value as any).s = s;
  (value as any).e = e;

  if (ln > 0) {
    for (let i = 3; i < (ln + 3);) {
      d.push(bytes[i] | bytes[i + 1] << 8 | bytes[i + 2] << 16 | bytes[i + 3] << 24);
      i = i + 4;
    }
  }

  (value as any).d = d;
  return value;
}
