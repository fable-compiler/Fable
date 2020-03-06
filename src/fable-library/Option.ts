import { declare, Union } from "./Types";
import { compare, equals, structuralHash } from "./Util";

// Options are erased in runtime by Fable, but we have
// the `Some` type below to wrap values that would evaluate
// to null in runtime. These two rules must be followed:

// 1- None is always null in runtime, a non-strict null check
//    (`x == null`) is enough to check the case of an option.
// 2- To get the value of an option the `getValue` helper
//    below must **always** be used.

export type Option<T> = T | Some<T> | null | undefined;

// Using a class here for better compatibility with TS files importing Some
export class Some<T> {
  public value: T;

  constructor(value: T) {
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

export function some<T>(x: T): Option<T> {
  return x == null || x instanceof Some ? new Some(x) : x;
}

export function value<T>(x: Option<T>) {
  if (x == null) {
    throw new Error("Option has no value");
  } else {
    return x instanceof Some ? x.value : x;
  }
}

export function tryValue<T>(x: Option<T>) {
  return x instanceof Some ? x.value : x;
}

export function toArray<T>(opt: Option<T>) {
  return (opt == null) ? [] : [value(opt)];
}

export function defaultArg<T>(opt: Option<T>, defaultValue: T) {
  return (opt != null) ? value(opt) : defaultValue;
}

export function defaultArgWith<T>(opt: Option<T>, defThunk: () => T) {
  return (opt != null) ? value(opt) : defThunk();
}

export function filter<T>(predicate: (arg: T) => boolean, opt: Option<T>) {
  return (opt != null) ? (predicate(value(opt)) ? opt : null) : opt;
}

export function map<T, U>(mapping: (arg: T) => U, opt: Option<T>): Option<U> {
  return (opt != null) ? some(mapping(value(opt))) : null;
}

export function map2<T1, T2, U>(
  mapping: (arg1: T1, arg2: T2) => Option<U>,
  opt1: Option<T1>, opt2: Option<T2>) {
  return (opt1 != null && opt2 != null) ? mapping(value(opt1), value(opt2)) : null;
}

export function map3<T1, T2, T3, U>(
  mapping: (arg1: T1, arg2: T2, arg3: T3) => Option<U>,
  opt1: Option<T1>, opt2: Option<T2>, opt3: Option<T3>) {
  return (opt1 != null && opt2 != null && opt3 != null) ? mapping(value(opt1), value(opt2), value(opt3)) : null;
}

export function bind<T, U>(binder: (arg: T) => Option<U>, opt: Option<T>): Option<U> {
  return opt != null ? binder(value(opt)) : null;
}

export function tryOp<T, U>(op: (x: T) => U, arg: T): Option<U> {
  try {
    return some(op(arg));
  } catch {
    return null;
  }
}

// CHOICE
export type Choice<T, U> = Union;

export const Choice = declare(function Choice<T, U>(this: Choice<T, U>, tag: number, name: string, field: T | U) {
  Union.call(this, tag, name, field);
}, Union);

export function choice1<T, U>(x: T | U): Choice<T, U> {
  return new Choice(0, "Choice1Of2", x);
}

export function choice2<T, U>(x: T | U): Choice<T, U> {
  return new Choice(1, "Choice2Of2", x);
}

export function tryValueIfChoice1<T, U>(x: Choice<T, U>): Option<T> {
  return x.tag === 0 ? some(x.fields[0]) : null;
}

export function tryValueIfChoice2<T, U>(x: Choice<T, U>): Option<U> {
  return x.tag === 1 ? some(x.fields[0]) : null;
}

// RESULT
export type Result<T, U> = Union;

export const Result = declare(function Result<T, U>(this: Result<T, U>, tag: number, name: string, field: T | U) {
  Union.call(this, tag, name, field);
}, Union);

export function ok<T, U>(x: T | U): Result<T, U> {
  return new Result(0, "Ok", x);
}

export function error<T, U>(x: T | U): Result<T, U> {
  return new Result(1, "Error", x);
}

export function mapOk<T, U>(f: (arg: T) => T, result: Result<T, U>) {
  return result.tag === 0 ? ok(f(result.fields[0])) : result;
}

export function mapError<T, U>(f: (arg: U) => U, result: Result<T, U>) {
  return result.tag === 1 ? error(f(result.fields[0])) : result;
}

export function bindOk<T, U>(f: (arg: T) => Result<T, U>, result: Result<T, U>) {
  return result.tag === 0 ? f(result.fields[0]) : result;
}
