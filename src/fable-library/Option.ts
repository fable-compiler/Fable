import { Nullable, Option, Some, value } from "./Util.js";
export { Nullable, Option, Some, value };

// Options are erased in runtime by Fable, but we have
// the `Some` type below to wrap values that would evaluate
// to `undefined` in runtime. These two rules must be followed:

// 1- `None` is always `undefined` in runtime, a non-strict null check
//    (`x == null`) is enough to check the case of an option.
// 2- To get the value of an option the `value` helper
//    below must **always** be used.

// Note: We use non-strict null check for backwards compatibility with
// code that use F# options to represent values that could be null in JS


export function some<T>(x: T): Option<T> {
  return x == null || x instanceof Some ? new Some(x) : x;
}

export function ofNullable<T>(x: T | null): Option<T> {
  // This will fail with unit probably, an alternative would be:
  // return x === null ? undefined : (x === undefined ? new Some(x) : x);
  return x == null ? undefined : x;
}

export function toNullable<T>(x: Option<T>): T | null {
  return x == null ? null : value(x);
}

export function flatten<T>(x: Option<Option<T>>) {
  return x == null ? undefined : value(x);
}

export function toArray<T>(opt: Option<T>): T[] {
  return (opt == null) ? [] : [value(opt)];
}

export function defaultArg<T>(opt: Option<T>, defaultValue: T): T {
  return (opt != null) ? value(opt) : defaultValue;
}

export function defaultArgWith<T>(opt: Option<T>, defThunk: () => T): T {
  return (opt != null) ? value(opt) : defThunk();
}

export function orElse<T>(opt: Option<T>, ifNone: Option<T>): Option<T> {
  return opt == null ? ifNone : opt;
}

export function orElseWith<T>(opt: Option<T>, ifNoneThunk: () => Option<T>): Option<T> {
  return opt == null ? ifNoneThunk() : opt;
}

export function filter<T>(predicate: (arg: T) => boolean, opt: Option<T>): Option<T> {
  return (opt != null) ? (predicate(value(opt)) ? opt : undefined) : opt;
}

export function map<T, U>(mapping: (arg: T) => U, opt: Option<T>): Option<U> {
  return (opt != null) ? some(mapping(value(opt))) : undefined;
}

export function map2<T1, T2, U>(
  mapping: (arg1: T1, arg2: T2) => Option<U>,
  opt1: Option<T1>, opt2: Option<T2>): Option<U> {
  return (opt1 != null && opt2 != null) ? mapping(value(opt1), value(opt2)) : undefined;
}

export function map3<T1, T2, T3, U>(
  mapping: (arg1: T1, arg2: T2, arg3: T3) => Option<U>,
  opt1: Option<T1>, opt2: Option<T2>, opt3: Option<T3>): Option<U> {
  return (opt1 != null && opt2 != null && opt3 != null) ? mapping(value(opt1), value(opt2), value(opt3)) : undefined;
}

export function bind<T, U>(binder: (arg: T) => Option<U>, opt: Option<T>): Option<U> {
  return opt != null ? binder(value(opt)) : undefined;
}

export function tryOp<T, U>(op: (x: T) => U, arg: T): Option<U> {
  try {
    return some(op(arg));
  } catch {
    return undefined;
  }
}
