import { declare, Union } from "./Types";
import { compare, equals, structuralHash } from "./Util";

// Options are erased in runtime by Fable, but we have
// the `Some` type below to wrap values that would evaluate
// to null in runtime. These two rules must be followed:

// 1- None is always null in runtime, a non-strict null check
//    (`x == null`) is enough to check the case of an option.
// 2- To get the value of an option the `getValue` helper
//    below must **always** be used.

export type Option<T> = T | Some<T> | null;

// Using a class here for better compatibility with TS files importing Some
export class Some<T> {
  public value: T | null;

  constructor(value: T | null) {
    this.value = value;
  }

  // Don't add "Some" for consistency with erased options
  public toString() {
    return String(this.value);
  }

  public toJSON() {
    return this.value;
  }

  public GetHashCode() {
    return structuralHash(this.value);
  }

  public Equals(other: Option<T>): boolean {
    if (other == null) {
      return false;
    } else {
      return equals(this.value, other instanceof Some ? other.value : other);
    }
  }

  public CompareTo(other: Option<T>) {
    if (other == null) {
      return 1;
    } else {
      return compare(this.value, other instanceof Some ? other.value : other);
    }
  }
}

export function some<T>(x: T | null): Option<T> {
  x = (x === undefined) ? null : x;
  return x == null || x instanceof Some ? new Some(x) : x;
}

export function value<T>(x: Option<T>, acceptNull?: boolean) {
  if (x == null) {
    if (!acceptNull) {
      throw new Error("Option has no value");
    }
    return null;
  } else {
    return x instanceof Some ? x.value : x;
  }
}

export function defaultArg<T>(arg: Option<T>, defaultValue: T, f?: (arg0: T | null) => T) {
  return arg == null ? defaultValue : (f != null ? f(value(arg)) : value(arg));
}

export function defaultArgWith<T>(arg: Option<T>, defThunk: () => T) {
  return arg == null ? defThunk() : value(arg);
}

export function filter<T>(predicate: (arg0: T | null) => boolean, arg: Option<T>) {
  return arg != null ? (!predicate(value(arg)) ? null : arg) : arg;
}

export function map<T1, T2>(f: (arg0: T1 | null) => T2, arg: Option<T1>) {
  return arg == null ? arg : some(f(value(arg)));
}

export function mapMultiple<T>(predicate: (...args: T[]) => boolean, ...args: T[]) {
  return args.every((x) => x != null) ? predicate.apply(null, args) : null;
}

export function bind<T1, T2>(f: (arg0: T1 | null) => Option<T2>, arg: Option<T1>) {
  return arg == null ? arg : f(value(arg));
}

export function tryOp<T1, T2>(op: (x: T1) => T2, arg: T1): Option<T2> {
  try {
    return some(op(arg));
  } catch {
    return null;
  }
}

// CHOICE
export type Choice<T1, T2> = Union;

export const Choice = declare(function Choice<T1, T2>(this: Choice<T1, T2>, tag: number, name: string, field: T1 | T2) {
  Union.call(this, tag, name, field);
}, Union);

export function choice1<T1, T2>(x: T1 | T2): Choice<T1, T2> {
  return new Choice(0, "Choice1Of2", x);
}

export function choice2<T1, T2>(x: T1 | T2): Choice<T1, T2> {
  return new Choice(1, "Choice2Of2", x);
}

export function tryValueIfChoice1<T1, T2>(x: Choice<T1, T2>): Option<T1> {
  return x.tag === 0 ? some(x.fields[0]) : null;
}

export function tryValueIfChoice2<T1, T2>(x: Choice<T1, T2>): Option<T2> {
  return x.tag === 1 ? some(x.fields[0]) : null;
}

// RESULT
export type Result<T1, T2> = Union;

export const Result = declare(function Result<T1, T2>(this: Result<T1, T2>, tag: number, name: string, field: T1 | T2) {
  Union.call(this, tag, name, field);
}, Union);

export function ok<T1, T2>(x: T1 | T2): Result<T1, T2> {
  return new Result(0, "Ok", x);
}

export function error<T1, T2>(x: T1 | T2): Result<T1, T2> {
  return new Result(1, "Error", x);
}

export function mapOk<T1, T2>(f: (arg: T1) => T1, result: Result<T1, T2>) {
  return result.tag === 0 ? ok(f(result.fields[0])) : result;
}

export function mapError<T1, T2>(f: (arg: T2) => T2, result: Result<T1, T2>) {
  return result.tag === 1 ? error(f(result.fields[0])) : result;
}

export function bindOk<T1, T2>(f: (arg: T1) => Result<T1, T2>, result: Result<T1, T2>) {
  return result.tag === 0 ? f(result.fields[0]) : result;
}
