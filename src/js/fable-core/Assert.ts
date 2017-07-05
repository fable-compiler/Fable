import { equals } from "./Util";

export class AssertionError<T> extends Error {
  public actual: T;
  public expected: T;
  constructor(msg: string, actual: T, expected: T) {
    super(msg);
    this.actual = actual;
    this.expected = expected;
  }
}

export function equal<T>(actual: T, expected: T, msg?: string): void {
  if (!equals(actual, expected)) {
    throw new AssertionError(msg || `Expected: ${expected} - Actual: ${actual}`, actual, expected);
  }
}
