// tslint:disable: space-before-function-paren
import { IEquatable, IComparable, combineHashCodes, compare, compareArrays, equalArrays, equals, identityHash, numberHash, structuralHash } from "./Util";

function sameType(x: any, y: any) {
  return y != null && Object.getPrototypeOf(x).constructor === Object.getPrototypeOf(y).constructor;
}

// Taken from Babel helpers
function inherits(subClass: any, superClass: any) {
  // if (typeof superClass !== "function" && superClass !== null) {
  //   throw new TypeError(
  //     "Super expression must either be null or a function, not " +
  //       typeof superClass
  //   );
  // }
  subClass.prototype = Object.create(superClass && superClass.prototype, {
    constructor: {
      value: subClass,
      enumerable: false,
      writable: true,
      configurable: true,
    },
  });
  // if (superClass)
  //   Object.setPrototypeOf
  //     ? Object.setPrototypeOf(subClass, superClass)
  //     : (subClass.__proto__ = superClass);
}

export function declare(cons: any, superClass?: any) {
  inherits(cons, superClass || SystemObject);
  return cons;
}

export class SystemObject implements IEquatable<any> {

  public toString() {
    return "{" + Object.entries(this).map(([k, v]) => k + " = " + String(v)).join(";\n ") + "}";
  }

  public GetHashCode(x?: any) {
    return identityHash(x ?? this);
  }

  public Equals(x: any, y?: any) {
    return x === (y ?? this);
  }
}

function compareList<T>(self: List<T>, other: List<T>) {
  if (self === other) {
    return 0;
  } else {
    if (other == null) {
      return -1;
    }
    const selfLen = self.length;
    const otherLen = other.length;
    const minLen = Math.min(selfLen, otherLen);
    for (let i = 0; i < minLen; i++) {
      const res = compare(self.item(i), other.item(i));
      if (res !== 0) { return res; }
    }
    return selfLen > otherLen ? 1 : (selfLen < otherLen ? -1 : 0);
  }
}

/**
 * F# list is represented in runtime by an optimized type that uses a stack (a reverted JS array)
 * to store the values, so we can a have a big list represented by a single object (plus the stack).
 * It also allows for optimizations in the List module.
 */
export class List<T> implements IEquatable<List<T>>, IComparable<List<T>>, Iterable<T> {
  public vals: T[];
  public idx: number;
  public _tail: List<T> | undefined;

  constructor(vals?: T[], idx?: number) {
    this.vals = vals ?? [];
    this.idx = idx ?? this.vals.length - 1;
  }

  add(item: T): List<T> {
    // If this points to the last index of the stack, push the new value into it.
    // Otherwise, this becomes an "actual" tail.
    if (this.vals.length === this.idx + 1) {
      this.vals.push(item);
      return new List(this.vals);
    } else {
      const li = new List([item]);
      li._tail = this;
      return li;
    }
   }

  /** Unsafe, check length before calling it */
  public item(i: number): T | undefined {
    let rev_i = this.idx - i;
    if (rev_i >= 0) {
      return this.vals[rev_i];
    } else if (this._tail) {
      return this._tail.item(rev_i * -1 - 1);
    }
    return undefined;
  }

  /** Unsafe, check isEmpty before calling it */
  public get head(): T | undefined {
    return this.vals[this.idx];
  }

  public get tail(): List<T> | undefined {
    if (this.idx === 0 && this._tail) {
      return this._tail;
    } else if (this.idx >= 0) {
      return new List(this.vals, this.idx - 1);
    }
    return undefined;
  }

  public get isEmpty() {
    return this.idx < 0;
  }

  public get length(): number {
    return this.idx + 1 + (this._tail?.length ?? 0);
  }

  public toString() {
    return "[" + Array.from(this).join("; ") + "]";
  }

  public toJSON() {
    return Array.from(this);
  }

  public [Symbol.iterator](): Iterator<T> {
    let curIdx = this.idx;
    let li: List<T> = this;
    return {
      next: (): IteratorResult<T> => {
        if (curIdx < 0) {
          if (li._tail) {
            li = li._tail;
            curIdx = li.idx;
          } else {
            return { done: true, value: undefined };
          }
        }
        return { done: false, value: li.vals[curIdx--] };
      }
    };
  }

  public GetHashCode() {
    if (this.idx < 0) {
      return 0;
    } else {
      const hashes: number[] = new Array(this.idx + 1);
      for (let i = this.idx; i >= 0; i--) {
        hashes[i] = structuralHash(this.vals[i]);
      }
      return combineHashCodes(hashes);
    }
  }

  public Equals(other: List<T>): boolean {
    return compareList(this, other) === 0;
  }

  public CompareTo(other: List<T>): number {
    return compareList(this, other);
  }
}

export class Union extends SystemObject implements IComparable<any> {
  public tag: number;
  public name: string;
  public fields: any[];

  constructor(tag: number, name: string, ...fields: any[]) {
    super();
    this.tag = tag | 0;
    this.name = name;
    this.fields = fields;
  }

  public toString() {
    const len = this.fields.length;
    if (len === 0) {
      return this.name;
    } else if (len === 1) {
      return this.name + " " + String(this.fields[0]);
    } else {
      return this.name + " (" + this.fields.map((x: any) => String(x)).join(",") + ")";
    }
  }

  public toJSON() {
    return this.fields.length === 0
      ? this.name
      : [this.name].concat(this.fields);
  }

  public GetHashCode() {
    const hashes = this.fields.map((x: any) => structuralHash(x));
    hashes.splice(0, 0, numberHash(this.tag));
    return combineHashCodes(hashes);
  }

  public Equals(other: any) {
    return this === other
      || (sameType(this, other)
        && this.tag === other.tag
        && equalArrays(this.fields, other.fields));
  }

  public CompareTo(other: any) {
    if (this === other) {
      return 0;
    } else if (!sameType(this, other)) {
      return -1;
    } else if (this.tag === other.tag) {
      return compareArrays(this.fields, other.fields);
    } else {
      return this.tag < other.tag ? -1 : 1;
    }
  }
}

function recordToJson(record: any, getFieldNames?: (arg: any) => any) {
  const o: any = {};
  const keys = getFieldNames == null ? Object.keys(record) : getFieldNames(record);
  for (let i = 0; i < keys.length; i++) {
    o[keys[i]] = record[keys[i]];
  }
  return o;
}

function recordEquals(self: any, other: any, getFieldNames?: (arg: any) => any) {
  if (self === other) {
    return true;
  } else if (!sameType(self, other)) {
    return false;
  } else {
    const thisNames = getFieldNames == null ? Object.keys(self) : getFieldNames(self);
    for (let i = 0; i < thisNames.length; i++) {
      if (!equals(self[thisNames[i]], other[thisNames[i]])) {
        return false;
      }
    }
    return true;
  }
}

function recordCompare(self: any, other: any, getFieldNames?: (arg: any) => any) {
  if (self === other) {
    return 0;
  } else if (!sameType(self, other)) {
    return -1;
  } else {
    const thisNames = getFieldNames == null ? Object.keys(self) : getFieldNames(self);
    for (let i = 0; i < thisNames.length; i++) {
      const result = compare(self[thisNames[i]], other[thisNames[i]]);
      if (result !== 0) {
        return result;
      }
    }
    return 0;
  }
}

export class Record extends SystemObject implements IComparable<any> {

  public toString() {
    return "{" + Object.entries(this).map(([k, v]) => k + " = " + String(v)).join(";\n ") + "}";
  }

  public toJSON() {
    return recordToJson(this);
  }

  public GetHashCode() {
    const hashes = Object.values(this).map((v) => structuralHash(v));
    return combineHashCodes(hashes);
  }

  public Equals(other: any) {
    return recordEquals(this, other);
  }

  public CompareTo(other: any) {
    return recordCompare(this, other);
  }
}

export function anonRecord(o: any) {
  return Object.assign(Object.create(Record.prototype), o);
}

export class FSharpRef<T> extends Record {
  public contents: T;

  constructor(contents: T | null) {
    super();
    this.contents = contents as T;
  }
}

// EXCEPTIONS

// export class Exception extends SystemObject {
//   public stack?: string;
//   public message?: string;

//   constructor(message?: string) {
//     super();
//     this.stack = Error().stack;
//     this.message = message;
//   }
// }

export interface Exception extends SystemObject {
  stack?: string;
  message?: string;
}

export const Exception = declare(function Exception(this: Exception, message?: string) {
  this.stack = Error().stack;
  this.message = message;
}, SystemObject);

export function isException(x: any) {
  return x instanceof Error || x instanceof Exception;
}

function getFSharpExceptionFieldNames(self: any) {
  return Object.keys(self).filter((k) => k !== "message" && k !== "stack");
}

export class FSharpException extends Exception implements IComparable<any> {

  public toString() {
    // const fieldNames = getFSharpExceptionFieldNames(this);
    const fields = Object.entries(this).filter(([k, _]) => k !== "message" && k !== "stack");
    const len = fields.length;
    if (len === 0) {
      return this.message ?? "";
    } else if (len === 1) {
      return this.message + " " + String(fields[1]);
    } else {
      return this.message + " (" + fields.map(([_, v]) => String(v)).join(",") + ")";
    }
  }

  public toJSON() {
    return recordToJson(this, getFSharpExceptionFieldNames);
  }

  public GetHashCode() {
    const fields = Object.entries(this).filter(([k, _]) => k !== "message" && k !== "stack");
    const hashes = fields.map(([_, v]) => structuralHash(v));
    return combineHashCodes(hashes);
  }

  public Equals(other: any) {
    return recordEquals(this, other, getFSharpExceptionFieldNames);
  }

  public CompareTo(other: any) {
    return recordCompare(this, other, getFSharpExceptionFieldNames);
  }
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

export const Attribute = declare(function Attribute() { return; }, SystemObject);
