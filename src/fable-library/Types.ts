import { IEquatable, IComparable, combineHashCodes, compare, compareArrays, equalArrays, equals, isComparable, isEquatable, isHashable, isSameType, isStringable, numberHash, structuralHash } from "./Util.js";

export function objectToString(self: any) {
  if (isStringable(self)) {
    return self.ToString();
  } else {
    return Object.getPrototypeOf(self).constructor.name;
  }
}

// export class SystemObject implements IEquatable<any> {

//   public toString() {
//     return this.ToString();
//   }

//   public ToString() {
//     return objectToString(this);
//   }

//   public GetHashCode(x?: any) {
//     return identityHash(x ?? this);
//   }

//   public Equals(x: any, y?: any) {
//     return x === (y ?? this);
//   }
// }

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

  public toJSON() {
    return Array.from(this);
  }

  public toString() {
    return this.ToString();
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

export function unionToString(self: any) {
  const name = self.cases()[self.tag];
  if (isStringable(self)) {
    return self.ToString();
  } else if (self.fields.length === 0) {
    return name;
  } else if (self.fields.length === 1) {
    return name + " " + String(self.fields[0]);
  } else {
    return name + " (" + self.fields.map((x: any) => String(x)).join(",") + ")";
  }
}

export function unionToJson(self: any) {
  const name = self.cases()[self.tag];
  return self.fields.length === 0 ? name : [name].concat(self.fields);
}

export function unionGetHashCode(self: any): number {
  if (isHashable(self)) { // && !(self instanceof Union)) {
    return self.GetHashCode();
  } else {
    const hashes = self.fields.map((x: any) => structuralHash(x));
    hashes.splice(0, 0, numberHash(self.tag));
    return combineHashCodes(hashes);
  }
}

export function unionEquals(self: any, other: any) {
  if (self === other) {
    return true;
  } else if (isEquatable(self)) { // && !(self instanceof Union)) {
    return self.Equals(other);
  } else if (!isSameType(self, other)) {
    return false;
  } else if (self.tag === other.tag) {
    return equalArrays(self.fields, other.fields);
  } else {
    return false;
  }
}

export function unionCompareTo(self: any, other: any) {
  if (self === other) {
    return 0;
  } else if (isComparable(self)) { // && !(self instanceof Union)) {
    return self.CompareTo(other);
  } else if (!isSameType(self, other)) {
    return -1;
  } else if (self.tag === other.tag) {
    return compareArrays(self.fields, other.fields);
  } else {
    return self.tag < other.tag ? -1 : 1;
  }
}

// export class Union implements IEquatable<any>, IComparable<any> {
//   public tag: number;
//   public fields: any[];

//   public cases(): string[] {
//       return [];
//   }

//   constructor(tag: number, ...fields: any[]) {
//     this.tag = tag | 0;
//     this.fields = fields;
//   }

//   public toJSON() {
//     return unionToJson(this);
//   }

//   public toString() {
//     return this.ToString();
//   }

//   public ToString() {
//     return unionToString(this);
//   }

//   public GetHashCode() {
//     return unionGetHashCode(this);
//   }

//   public Equals(other: any) {
//     return unionEquals(this, other);
//   }

//   public CompareTo(other: any) {
//     return unionCompareTo(this, other);
//   }
// }

export function recordToString(self: any) {
  if (isStringable(self)) {
    return self.ToString();
  } else {
    return "{" + Object.entries(self).map(([k, v]) => k + " = " + String(v)).join(";\n ") + "}";
  }
}

export function recordToJson(self: any, getFieldNames?: (arg: any) => any) {
  const o: any = {};
  const keys = getFieldNames == null ? Object.keys(self) : getFieldNames(self);
  for (let i = 0; i < keys.length; i++) {
    o[keys[i]] = self[keys[i]];
  }
  return o;
}

export function recordGetHashCode(self: any) {
  if (isHashable(self)) { // && !(self instanceof Record)) {
    return self.GetHashCode();
  } else {
    const hashes = Object.values(self).map((v) => structuralHash(v));
    return combineHashCodes(hashes);
  }
}

export function recordEquals(self: any, other: any) {
  if (self === other) {
    return true;
  } else if (isEquatable(self)) { // && !(self instanceof Record)) {
    return self.Equals(other);
  } else if (!isSameType(self, other)) {
    return false;
  } else {
    const thisNames = Object.keys(self);
    for (let i = 0; i < thisNames.length; i++) {
      if (!equals(self[thisNames[i]], other[thisNames[i]])) {
        return false;
      }
    }
    return true;
  }
}

export function recordCompareTo(self: any, other: any) {
  if (self === other) {
    return 0;
  } else if (isComparable(self)) { // && !(self instanceof Record)) {
    return self.CompareTo(other);
  } else if (!isSameType(self, other)) {
    return -1;
  } else {
    const thisNames = Object.keys(self);
    for (let i = 0; i < thisNames.length; i++) {
      const result = compare(self[thisNames[i]], other[thisNames[i]]);
      if (result !== 0) {
        return result;
      }
    }
    return 0;
  }
}

// export class Record implements IEquatable<any>, IComparable<any> {

//   public toJSON() {
//     return recordToJson(this);
//   }

//   public toString() {
//     return this.ToString();
//   }

//   public ToString() {
//     return recordToString(this);
//   }

//   public GetHashCode() {
//     return recordGetHashCode(this);
//   }

//   public Equals(other: any) {
//     return recordEquals(this, other);
//   }

//   public CompareTo(other: any) {
//     return recordCompareTo(this, other);
//   }
// }

export function anonRecord(o: any) {
  // return Object.assign(Object.create(Record.prototype), o);
  return o;
}

export class FSharpRef<T> {
  public contents: T;
  constructor(contents: T | null) {
    this.contents = contents as T;
  }
}

// EXCEPTIONS

// export class Exception extends Error implements IEquatable<any> {
//   constructor(message?: string) {
//     super(message);
//     if (Error.captureStackTrace) {
//         Error.captureStackTrace(this, Exception)
//     }
//   }

//   public toString() {
//     return this.ToString();
//   }

//   public ToString() {
//     return Object.getPrototypeOf(this).constructor.name;
//   }

//   public GetHashCode(x?: any) {
//     return identityHash(x ?? this);
//   }

//   public Equals(x: any, y?: any) {
//     return x === (y ?? this);
//   }
// }

// Exception is intentionally not derived from Error, for performance reasons (see #2160)
export class Exception {
  constructor(public message?: string) { }
}

export function isException(x: any) {
  return x instanceof Exception || x instanceof Error;
}

function getFSharpExceptionFieldNames(self: any) {
  return Object.keys(self).filter((k) => k !== "message" && k !== "stack");
}

export class FSharpException extends Exception implements IComparable<any> {

  public toJSON() {
    return recordToJson(this, getFSharpExceptionFieldNames);
  }

  public toString() {
    return this.ToString();
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
    const self: any = this;
    const thisNames = getFSharpExceptionFieldNames(self);
    for (let i = 0; i < thisNames.length; i++) {
      if (!equals(self[thisNames[i]], other[thisNames[i]])) {
        return false;
      }
    }
    return true;
  }

  public CompareTo(other: any) {
    const self: any = this;
    const thisNames = getFSharpExceptionFieldNames(self);
    for (let i = 0; i < thisNames.length; i++) {
      const result = compare(self[thisNames[i]], other[thisNames[i]]);
      if (result !== 0) {
        return result;
      }
    }
    return 0;
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

export class Attribute {
}
