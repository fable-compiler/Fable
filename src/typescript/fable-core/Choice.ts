import FSymbol from "./Symbol";
import { Any, IComparable, IEquatable } from "./Util";
import { compareUnions, equalsUnions } from "./Util";

export function choice1Of2<T1, T2>(v: T1) {
  return new Choice<T1, T2>(0, v);
}

export function choice2Of2<T1, T2>(v: T2) {
  return new Choice<T1, T2>(1, v);
}

export default class Choice<T1, T2> implements IEquatable<Choice<T1, T2>>, IComparable<Choice<T1, T2>> {
  public tag: number;
  public data: T1 | T2;

  constructor(tag: number, data: T1 | T2) {
    this.tag = tag | 0;
    this.data = data;
  }

  get valueIfChoice1() {
    return this.tag === 0 ? this.data as T1 : null;
  }

  get valueIfChoice2() {
    return this.tag === 1 ? this.data as T2 : null;
  }

  public Equals(other: Choice<T1, T2>) {
    return equalsUnions(this, other);
  }

  public CompareTo(other: Choice<T1, T2>) {
    return compareUnions(this, other);
  }

  public [FSymbol.reflection]() {
    return {
      type: "Microsoft.FSharp.Core.FSharpChoice",
      interfaces: ["FSharpUnion", "System.IEquatable", "System.IComparable"],
      cases: [["Choice1Of2", Any], ["Choice2Of2", Any]],
    };
  }
}
