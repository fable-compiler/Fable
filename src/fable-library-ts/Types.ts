import { IComparable, IEquatable, IHashable, combineHashCodes, compare, compareArrays, equalArrays, equals, sameConstructor, numberHash, structuralHash } from "./Util.js";

// This type is only used internally for .ts files in the library
// F# Result type is in Choice.fs
export type Result<T> = { tag: "ok"; value: T } | { tag: "error"; error: string };

export function seqToString<T>(self: Iterable<T>): string {
  let count = 0;
  let str = "[";
  for (const x of self) {
    if (count === 0) {
      str += toString(x);
    } else if (count === 100) {
      str += "; ...";
      break;
    } else {
      str += "; " + toString(x);
    }
    count++;
  }
  return str + "]";
}

export function toString(x: any, callStack = 0): string {
  if (x != null && typeof x === "object") {
    if (typeof x.toString === "function") {
      return x.toString();
    } else if (Symbol.iterator in x) {
      return seqToString(x);
    } else { // TODO: Date?
      const cons = Object.getPrototypeOf(x)?.constructor;
      return cons === Object && callStack < 10
        // Same format as recordToString
        ? "{ " + Object.entries(x).map(([k, v]) => k + " = " + toString(v, callStack + 1)).join("\n  ") + " }"
        : cons?.name ?? "";
    }
  }
  return String(x);
}

export function unionToString(name: string, fields: any[]) {
  if (fields.length === 0) {
    return name;
  } else {
    let fieldStr;
    let withParens = true;
    if (fields.length === 1) {
      fieldStr = toString(fields[0]);
      withParens = fieldStr.indexOf(" ") >= 0;
    } else {
      fieldStr = fields.map((x: any) => toString(x)).join(", ");
    }
    return name + (withParens ? " (" : " ") + fieldStr + (withParens ? ")" : "");
  }
}

export abstract class Union<Tag extends number, Name extends string> implements IEquatable<Union<Tag, Name>>, IComparable<Union<Tag, Name>> {
  abstract readonly tag: Tag;
  abstract readonly fields: any[];
  abstract cases(): string[];

  public get name(): Name {
    return this.cases()[this.tag] as Name;
  }

  public toJSON() {
    return this.fields.length === 0 ? this.name : [this.name].concat(this.fields);
  }

  public toString() {
    return unionToString(this.name, this.fields);
  }

  public GetHashCode() {
    const hashes = this.fields.map((x: any) => structuralHash(x));
    hashes.splice(0, 0, numberHash(this.tag));
    return combineHashCodes(hashes);
  }

  public Equals(other: Union<Tag, Name>) {
    if (this === other) {
      return true;
    } else if (!sameConstructor(this, other)) {
      return false;
    } else if (this.tag === other.tag) {
      return equalArrays(this.fields, other.fields);
    } else {
      return false;
    }
  }

  public CompareTo(other: Union<Tag, Name>) {
    if (this === other) {
      return 0;
    } else if (!sameConstructor(this, other)) {
      return -1;
    } else if (this.tag === other.tag) {
      return compareArrays(this.fields, other.fields);
    } else {
      return this.tag < other.tag ? -1 : 1;
    }
  }
}

function recordToJSON<T>(self: T) {
  const o: any = {};
  const keys = Object.keys(self as any);
  for (let i = 0; i < keys.length; i++) {
    o[keys[i]] = (self as any)[keys[i]];
  }
  return o;
}

function recordToString<T>(self: T) {
  return "{ " + Object.entries(self as any).map(([k, v]) => k + " = " + toString(v)).join("\n  ") + " }";
}

function recordGetHashCode<T>(self: T) {
  const hashes = Object.values(self as any).map((v) => structuralHash(v));
  return combineHashCodes(hashes);
}

function recordEquals<T>(self: T, other: T) {
  if (self === other) {
    return true;
  } else if (!sameConstructor(self, other)) {
    return false;
  } else {
    const thisNames = Object.keys(self as any);
    for (let i = 0; i < thisNames.length; i++) {
      if (!equals((self as any)[thisNames[i]], (other as any)[thisNames[i]])) {
        return false;
      }
    }
    return true;
  }
}

function recordCompareTo<T>(self: T, other: T) {
  if (self === other) {
    return 0;
  } else if (!sameConstructor(self, other)) {
    return -1;
  } else {
    const thisNames = Object.keys(self as any);
    for (let i = 0; i < thisNames.length; i++) {
      const result = compare((self as any)[thisNames[i]], (other as any)[thisNames[i]]);
      if (result !== 0) {
        return result;
      }
    }
    return 0;
  }
}

export abstract class Record implements IEquatable<Record>, IComparable<Record>, IHashable {
  toJSON() { return recordToJSON(this); }
  toString() { return recordToString(this); }
  GetHashCode() { return recordGetHashCode(this); }
  Equals(other: Record) { return recordEquals(this, other); }
  CompareTo(other: Record) { return recordCompareTo(this, other); }
}

export class FSharpRef<T> {
  private readonly getter: () => T;
  private readonly setter: (v: T) => void;

  get contents() {
    return this.getter();
  }

  set contents(v) {
    this.setter(v)
  }

  constructor(contentsOrGetter: T | (() => T), setter?: (v: T) => void) {
    if (typeof setter === "function") {
      this.getter = contentsOrGetter as () => T;
      this.setter = setter
    } else {
      this.getter = () => contentsOrGetter as T;
      this.setter = (v) => { contentsOrGetter = v };
    }
  }
}

// EXCEPTIONS

// Exception is intentionally not derived from Error, for performance reasons (see #2160)
export class Exception {
  constructor(public message?: string) { }
}

export function isException(x: any) {
  return x instanceof Exception || x instanceof Error;
}

export function isPromise(x: any) {
  return x instanceof Promise;
}

export function ensureErrorOrException(e: any): any {
  // Exceptionally admitting promises as errors for compatibility with React.suspense (see #3298)
  return (isException(e) || isPromise(e)) ? e : new Error(String(e));
}

export abstract class FSharpException extends Exception
  implements IEquatable<FSharpException>, IComparable<FSharpException> {
  toJSON() { return recordToJSON(this); }
  toString() { return recordToString(this); }
  GetHashCode() { return recordGetHashCode(this); }
  Equals(other: FSharpException) { return recordEquals(this, other); }
  CompareTo(other: FSharpException) { return recordCompareTo(this, other); }
}

export class MatchFailureException extends FSharpException {
  public arg1: string;
  public arg2: number;
  public arg3: number;

  constructor(arg1: string, arg2: number, arg3: number) {
    super();
    this.arg1 = arg1;
    this.arg2 = arg2 | 0;
    this.arg3 = arg3 | 0;
    this.message = "The match cases were incomplete";
  }
}

export class Attribute {
}
