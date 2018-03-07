import { compare, IComparer, IEqualityComparer } from "./Util";

export default class Comparer<T> implements IComparer<T> {
  public Compare: (x: T, y: T) => number;

  constructor(f?: (x: T, y: T) => number) {
    this.Compare = f || compare;
  }
}

export function fromEqualityComparer<T>(comparer: IEqualityComparer<T>) {
  // Sometimes IEqualityComparer also implements IComparer
  if (typeof (comparer as any).Compare === "function") {
    return new Comparer<T>((comparer as any).Compare);
  } else {
    return new Comparer<T>((x: T, y: T) => {
      const xhash = comparer.GetHashCode(x);
      const yhash = comparer.GetHashCode(y);
      if (xhash === yhash) {
        return comparer.Equals(x, y) ? 0 : -1;
      } else {
        return xhash < yhash ? -1 : 1;
      }
    });
  }
}
