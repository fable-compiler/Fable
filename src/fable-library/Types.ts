// tslint:disable: space-before-function-paren
import { combineHashCodes, compare, compareArrays, equalArrays, equals, identityHash, numberHash, structuralHash } from "./Util";

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

export function SystemObject() {
  return;
}

SystemObject.prototype.toString = function () {
  return "{" + Object.keys(this).map((k) => k + " = " + String(this[k])).join(";\n ") + "}";
};

SystemObject.prototype.GetHashCode = function () {
  return identityHash(this);
};

SystemObject.prototype.Equals = function (other: any) {
  return this === other;
};

function compareList(self: { tail: any; head: any; }, other: { tail: any; head: any; }) {
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

export interface List<T> {
  head?: T;
  tail?: List<T>;
}

export function List<T>(this: List<T>, head?: T, tail?: List<T>) {
  this.head = head;
  this.tail = tail;
}

List.prototype.toString = function () {
  return "[" + Array.from(this).map((x) => String(x)).join("; ") + "]";
};

List.prototype.toJSON = function () {
  return Array.from(this);
};

List.prototype[Symbol.iterator] = function () {
  let cur = this;
  return {
    next: () => {
      const tmp = cur;
      cur = cur.tail;
      return { done: tmp.tail == null, value: tmp.head };
    },
  };
};

List.prototype.GetHashCode = function () {
  const hashes = Array.from(this).map(structuralHash);
  return combineHashCodes(hashes);
};

List.prototype.Equals = function (other: any) {
  return compareList(this, other) === 0;
};

List.prototype.CompareTo = function (other: any) {
  return compareList(this, other);
};

export function Union(this: any, tag: number, name: any, ...fields: any[]) {
  this.tag = tag | 0;
  this.name = name;
  this.fields = fields;
}

Union.prototype.toString = function () {
  const len = this.fields.length;
  if (len === 0) {
    return this.name;
  } else if (len === 1) {
    return this.name + " " + String(this.fields[0]);
  } else {
    return this.name + " (" + this.fields.map((x: any) => String(x)).join(",") + ")";
  }
};

Union.prototype.toJSON = function () {
  return this.fields.length === 0
    ? this.name
    : [this.name].concat(this.fields);
};

Union.prototype.GetHashCode = function () {
  const hashes = this.fields.map((x: any) => structuralHash(x));
  hashes.splice(0, 0, numberHash(this.tag));
  return combineHashCodes(hashes);
};

Union.prototype.Equals = function (other: any) {
  return this === other
    || (sameType(this, other)
      && this.tag === other.tag
      && equalArrays(this.fields, other.fields));
};

Union.prototype.CompareTo = function (other: any) {
  if (this === other) {
    return 0;
  } else if (!sameType(this, other)) {
    return -1;
  } else if (this.tag === other.tag) {
    return compareArrays(this.fields, other.fields);
  } else {
    return this.tag < other.tag ? -1 : 1;
  }
};

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

export function Record() {
  return;
}

Record.prototype.toString = function () {
  return "{" + Object.keys(this).map((k) => k + " = " + String(this[k])).join(";\n ") + "}";
};

Record.prototype.toJSON = function () {
  return recordToJson(this);
};

Record.prototype.GetHashCode = function () {
  const hashes = Object.keys(this).map((k) => structuralHash(this[k]));
  return combineHashCodes(hashes);
};

Record.prototype.Equals = function (other: any) {
  return recordEquals(this, other);
};

Record.prototype.CompareTo = function (other: any) {
  return recordCompare(this, other);
};

export function anonRecord(o: any) {
  return Object.assign(Object.create(Record.prototype), o);
}

export const FSharpRef = declare(function FSharpRef(this: any, contents: any) {
  this.contents = contents;
}, Record);

// EXCEPTIONS

export const Exception = declare(function Exception(this: any, msg: any) {
  this.stack = Error().stack;
  this.message = msg;
});

export function isException(x: any) {
  return x instanceof Error || x instanceof Exception;
}

function getFSharpExceptionFieldNames(self: any) {
  return Object.keys(self).filter((k) => k !== "message" && k !== "stack");
}

export const FSharpException = declare(function FSharpException(this: any) {
  Exception.call(this);
}, Exception);

FSharpException.prototype.toString = function () {
  const fieldNames = getFSharpExceptionFieldNames(this);
  const len = fieldNames.length;
  if (len === 0) {
    return this.message;
  } else if (len === 1) {
    return this.message + " " + String(this[fieldNames[0]]);
  } else {
    return this.message + " (" + fieldNames.map((k) => String(this[k])).join(",") + ")";
  }
};

FSharpException.prototype.toJSON = function () {
  return recordToJson(this, getFSharpExceptionFieldNames);
};

FSharpException.prototype.GetHashCode = function () {
  const hashes = getFSharpExceptionFieldNames(this).map((k) => structuralHash(this[k]));
  return combineHashCodes(hashes);
};

FSharpException.prototype.Equals = function (other: any) {
  return recordEquals(this, other, getFSharpExceptionFieldNames);
};

FSharpException.prototype.CompareTo = function (other: any) {
  return recordCompare(this, other, getFSharpExceptionFieldNames);
};

export const MatchFailureException = declare(
  function MatchFailureException(this: any, arg1: any, arg2: number, arg3: number) {
    this.arg1 = arg1;
    this.arg2 = arg2 | 0;
    this.arg3 = arg3 | 0;
    this.message = "The match cases were incomplete";
  }, FSharpException);

export const Attribute = declare(function Attribute() { return; });
