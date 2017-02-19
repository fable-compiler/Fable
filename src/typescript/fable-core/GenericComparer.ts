import { IComparer, IEqualityComparer, compare } from "./Util"
import FSymbol from "./Symbol"

export default class GenericComparer<T> implements IComparer<T> {
  Compare: (x:T, y:T) => number;

  constructor(f?: (x:T, y:T) => number) {
    this.Compare = f || compare;
  }

  [FSymbol.reflection]() {
    return { interfaces: ["System.IComparer"] }
  }
}

export function fromEqualityComparer<T>(comparer: IEqualityComparer<T>) {
  // Sometimes IEqualityComparer also implements IComparer
  if (typeof (comparer as any).Compare === "function") {
    return new GenericComparer<T>((comparer as any).Compare);
  }
  else {
    return new GenericComparer<T>(function (x: T, y: T) {
      var xhash = comparer.GetHashCode(x), yhash = comparer.GetHashCode(y);
      if (xhash === yhash) {
        return comparer.Equals(x, y) ? 0 : 1;
      }
      else {
        return xhash < yhash ? 1 : -1;
      }
    });
  }
}