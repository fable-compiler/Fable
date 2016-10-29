import { IComparer } from "./Util.js"
import { compare } from "./Util.js"
import FSymbol from "./Symbol.js"

export default class GenericComparer<T> implements IComparer<T> {
  Compare: (x:T, y:T) => number;

  constructor(f?: (x:T, y:T) => number) {
    this.Compare = f || compare;
  }

  [FSymbol.interfaces]() {
    return ["System.IComparer"];
  }
}