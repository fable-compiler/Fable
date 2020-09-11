import { Union } from "./Types";
import { compare, equals, structuralHash } from "./Util";

// Options are erased in runtime by Fable, but we have
// the `Some` type below to wrap values that would evaluate
// to `undefined` in runtime. These two rules must be followed:

// 1- `None` is always `undefined` in runtime, a non-strict null check
//    (`x == null`) is enough to check the case of an option.
// 2- To get the value of an option the `getValue` helper
//    below must **always** be used.

export type Option<T> = T | Some<T> | undefined;

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

export function toArray<T>(opt: Option<T>): T[] {
  return (opt == null) ? [] : [value(opt)];
}

export function defaultArg<T>(opt: Option<T>, defaultValue: T): T {
  return (opt != null) ? value(opt) : defaultValue;
}

export function defaultArgWith<T>(opt: Option<T>, defThunk: () => T): T {
  return (opt != null) ? value(opt) : defThunk();
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

// CHOICE

export class Choice<_T1, _T2> extends Union {
  constructor(tag: number, name: string, ...fields: any[]) {
    super();
    this.tag = tag | 0;
    this.name = name;
    this.fields = fields;
  }
}
export class Choice3<_T1, _T2, _T3> extends Union {
  constructor(tag: number, name: string, ...fields: any[]) {
    super();
    this.tag = tag | 0;
    this.name = name;
    this.fields = fields;
  }
}
export class Choice4<_T1, _T2, _T3, _T4> extends Union {
  constructor(tag: number, name: string, ...fields: any[]) {
    super();
    this.tag = tag | 0;
    this.name = name;
    this.fields = fields;
  }
}
export class Choice5<_T1, _T2, _T3, _T4, _T5> extends Union {
  constructor(tag: number, name: string, ...fields: any[]) {
    super();
    this.tag = tag | 0;
    this.name = name;
    this.fields = fields;
  }
}
export class Choice6<_T1, _T2, _T3, _T4, _T5, _T6> extends Union {
  constructor(tag: number, name: string, ...fields: any[]) {
    super();
    this.tag = tag | 0;
    this.name = name;
    this.fields = fields;
  }
}
export class Choice7<_T1, _T2, _T3, _T4, _T5, _T6, _T7> extends Union {
  constructor(tag: number, name: string, ...fields: any[]) {
    super();
    this.tag = tag | 0;
    this.name = name;
    this.fields = fields;
  }
}

export function choice1Of2<T1, T2>(x: T1 | T2): Choice<T1, T2> {
  return new Choice(0, "Choice1Of2", x);
}

export function choice2Of2<T1, T2>(x: T1 | T2): Choice<T1, T2> {
  return new Choice(1, "Choice2Of2", x);
}

export function tryValueIfChoice1Of2<T1, T2>(x: Choice<T1, T2>): Option<T1> {
  return x.tag === 0 ? some(x.fields[0]) : undefined;
}

export function tryValueIfChoice2Of2<T1, T2>(x: Choice<T1, T2>): Option<T2> {
  return x.tag === 1 ? some(x.fields[0]) : undefined;
}

// RESULT

export class Result<_T, _U> extends Union {
  constructor(tag: number, name: string, ...fields: any[]) {
    super();
    this.tag = tag | 0;
    this.name = name;
    this.fields = fields;
  }
}

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
