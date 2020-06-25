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
    return this.ToString();
  }

  public ToString() {
    return this.constructor.name;
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
    while (self.tail != null) {
      if (other.tail == null) { return 1; }
      const res = compare(self.head, other.head);
      if (res !== 0) { return res; }
      self = self.tail;
      other = other.tail;
    }
    return other.tail == null ? 0 : -1;
  }
}

export class List<T> implements IEquatable<List<T>>, IComparable<List<T>>, Iterable<T> {
  public head: T;
  public tail?: List<T>;

  constructor(head?: T, tail?: List<T>) {
    this.head = head as T;
    this.tail = tail;
  }

  public toString() {
    return this.ToString();
  }

  public toJSON() {
    return Array.from(this);
  }

  public [Symbol.iterator](): Iterator<T> {
    let cur: List<T> | undefined = this;
    return {
      next: (): IteratorResult<T> => {
        const value = cur?.head as T;
        const done = cur?.tail == null;
        cur = cur?.tail;
        return { done, value };
      },
    };
  }

  public ToString() {
    return "[" + Array.from(this).join("; ") + "]";
  }

  public GetHashCode() {
    const hashes = Array.from(this).map(structuralHash);
    return combineHashCodes(hashes);
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
  public fields: any[];

  public cases(): string[] {
      return [];
  }

  public get name(): string {
      return this.cases()[this.tag];
  }

  constructor(tag: number, ...fields: any[]) {
    super();
    this.tag = tag | 0;
    this.fields = fields;
  }

  public ToString() {
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

  public ToString() {
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
    return this.ToString();
  }

  public toJSON() {
    return recordToJson(this, getFSharpExceptionFieldNames);
  }

  public ToString() {
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
