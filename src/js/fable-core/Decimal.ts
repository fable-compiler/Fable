import Decimal from "./lib/Big";

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

export function round(x: Decimal, dp?: number, rm?: number) {
  return x.round(dp, rm);
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
