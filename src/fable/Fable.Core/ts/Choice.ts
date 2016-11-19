import FSymbol from "./Symbol"
import { IEquatable } from "./Util"
import { IComparable } from "./Util"
import { equalsUnions } from "./Util"
import { compareUnions } from "./Util"

export function choice1Of2<T1, T2>(v: T1) {
    return new Choice<T1, T2>("Choice1Of2", [v]);
}

export function choice2Of2<T1, T2>(v: T2) {
    return new Choice<T1, T2>("Choice2Of2", [v]);
}

export default class Choice<T1, T2> implements IEquatable<Choice<T1, T2>>, IComparable<Choice<T1, T2>> {
  public Case: "Choice1Of2" | "Choice2Of2";
  public Fields: Array<T1 | T2>;

  constructor(t: "Choice1Of2" | "Choice2Of2", d: T1[] | T2[]) {
    this.Case = t;
    this.Fields = d;
  }

  get valueIfChoice1() {
    return this.Case === "Choice1Of2" ? <T1>this.Fields[0] : null;
  }

  get valueIfChoice2() {
    return this.Case === "Choice2Of2" ? <T2>this.Fields[0] : null;
  }

  Equals(other: Choice<T1,T2>) {
    return equalsUnions(this, other);
  }

  CompareTo(other: Choice<T1,T2>) {
    return compareUnions(this, other);
  }

  [FSymbol.reflection]() {
    return {
      type: "Microsoft.FSharp.Core.FSharpChoice",
      interfaces: ["FSharpUnion", "System.IEquatable", "System.IComparable"]
    }
  }
}
