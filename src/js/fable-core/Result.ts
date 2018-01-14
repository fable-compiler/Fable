import FSymbol from "./Symbol";
import { GenericParam, IComparable, IEquatable } from "./Util";
import { compareUnions, equalsUnions } from "./Util";

export default class Result<T1, T2> implements IEquatable<Result<T1, T2>>, IComparable<Result<T1, T2>> {
  public tag: number;
  public data: T1 | T2;

  constructor(tag: number, data: T1 | T2) {
    this.tag = tag | 0;
    this.data = data;
  }

  public Equals(other: Result<T1, T2>) {
    return equalsUnions(this, other);
  }

  public CompareTo(other: Result<T1, T2>) {
    return compareUnions(this, other);
  }

  public [FSymbol.reflection]() {
    return {
      type: "Microsoft.FSharp.Core.FSharpResult",
      interfaces: ["FSharpUnion", "System.IEquatable", "System.IComparable"],
      cases: [["Ok", GenericParam("T")], ["Error", GenericParam("TError")]],
    };
  }
}

export function map<T1, T2, Error>(f: (x: T1) => T2, result: Result<T1, Error>) {
  return result.tag === 0 ? new Result<T2, Error>(0, f(result.data as T1)) : result;
}

export function mapError<T, Error1, Error2>(f: (x: Error1) => Error2, result: Result<T, Error1>) {
  return result.tag === 1 ? new Result<T, Error2>(1, f(result.data as Error1)) : result;
}

export function bind<T1, T2, Error>(f: (x: T1) => Result<T2, Error>, result: Result<T1, Error>) {
  return result.tag === 0 ? f(result.data as T1) : result;
}
