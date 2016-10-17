import { IComparer } from "./Util"
import { compare } from "./Util"
import FSymbol from "./Symbol"

export default class GenericComparer<T> implements IComparer<T> {
  Compare: (x:T, y:T) => number;
  
  constructor(f?: (x:T, y:T) => number) {
    this.Compare = f || compare;
  }

  [FSymbol.interfaces]() {
    return ["System.IComparer"];
  }
}