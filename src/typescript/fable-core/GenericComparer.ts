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

export function fromEqualityComparer<T>(eqComparer: IEqualityComparer<T>) {
  return new GenericComparer<T>((x, y) => eqComparer.Equals(x, y) ? 0 : 1);
}