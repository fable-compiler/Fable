import { FSharpRef } from "./Types";

export function tryParse(str: string, defValue: FSharpRef<number>): boolean {
  // TODO: test if value is valid and in range
  if (str != null && /\S/.test(str)) {
    const v = +str.replace("_", "");
    if (!Number.isNaN(v)) {
      defValue.contents = v;
      return true;
    }
  }
  return false;
}

export function parse(str: string): number {
  const defValue = new FSharpRef(0);
  if (tryParse(str, defValue)) {
    return defValue.contents;
  } else {
    throw new Error("Input string was not in a correct format.");
  }
}

// JS Number.isFinite function evals false for NaN
export function isInfinity(x: number) {
  return x === Number.POSITIVE_INFINITY || x === Number.NEGATIVE_INFINITY;
}
