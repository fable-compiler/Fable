import { createMutable as createMutableMap } from "./Map";
import { createMutable as createMutableSet } from "./Set";
import { combineHashCodes, compare, compareArrays, equals, equalArrays, identityHash, structuralHash, numberHash, toString } from "./Util";

function sameType(x, y) {
  return y != null && Object.getPrototypeOf(x).constructor === Object.getPrototypeOf(y).constructor;
}

// Taken from Babel helpers
export function inherits(subClass, superClass) {
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

export function SystemObject() {
}

SystemObject.prototype.toString = function() {
  return "{" + Object.keys(this).map((k) => k + " = " + toString(this[k])).join(";\n ") + "}";
};

SystemObject.prototype.GetHashCode = function() {
  return identityHash(this);
};

SystemObject.prototype.Equals = function(other) {
  return this === other;
};

function compareList(self, other) {
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

export function List(head, tail) {
  this.head = head;
  this.tail = tail;
}

List.prototype.toString = function() {
  return "[" + Array.from(this).map(x => toString(x)).join("; ") + "]";
};

List.prototype.toJSON = function() {
  return Array.from(this);
};

List.prototype[Symbol.iterator] = function() {
  let cur = this;
  return {
    next: () => {
      const tmp = cur;
      cur = cur.tail;
      return { done: tmp.tail == null, value: tmp.head };
    },
  };
};

List.prototype.GetHashCode = function() {
  const hashes = Array.from(this).map(structuralHash);
  return combineHashCodes(hashes);
};

List.prototype.Equals = function(other) {
  return compareList(this, other) === 0;
};

List.prototype.CompareTo = function(other) {
  return compareList(this, other);
};

export function L(h, t) {
  return new List(h, t);
}

export function Union(tag, name, ...fields) {
  this.tag = tag | 0;
  this.name = name;
  this.fields = fields;
}

Union.prototype.toString = function() {
  const len = this.fields.length;
  if (len === 0) {
    return this.name;
  } else if (len === 1) {
    return this.name + " " + toString(this.fields[0]);
  } else {
    return this.name + " (" + this.fields.map(toString).join(",") + ")";
  }
};

Union.prototype.toJSON = function() {
  return this.fields.length === 0
    ? this.name
    : [this.name].concat(this.fields);
};

Union.prototype.GetHashCode = function() {
  let hashes = this.fields.map(structuralHash);
  hashes.splice(0, 0, numberHash(this.tag));
  return combineHashCodes(hashes);
};

Union.prototype.Equals = function(other) {
  return this === other
    || (sameType(this, other)
        && this.tag === other.tag
        && equalArrays(this.fields, other.fields));
};

Union.prototype.CompareTo = function(other) {
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

function recordToJson(record, getFieldNames) {
  const o = {};
  const keys = getFieldNames == null ? Object.keys(record) : getFieldNames(record);
  for (let i = 0; i < keys.length; i++) {
    o[keys[i]] = record[keys[i]];
  }
  return o;
}

function recordEquals(self, other, getFieldNames) {
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

function recordCompare(self, other, getFieldNames) {
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
}

Record.prototype.toString = function() {
  return "{" + Object.keys(this).map((k) => k + " = " + toString(this[k])).join(";\n ") + "}";
};

Record.prototype.toJSON = function() {
  return recordToJson(this);
};

Record.prototype.GetHashCode = function() {
  const hashes = Object.keys(this).map(k => structuralHash(this[k]));
  return combineHashCodes(hashes);
};

Record.prototype.Equals = function(other) {
  return recordEquals(this, other);
};

Record.prototype.CompareTo = function(other) {
  return recordCompare(this, other);
};

export function FSharpRef(contents) {
  this.contents = contents;
}
inherits(FSharpRef, Record);

// EXCEPTIONS

// function MyException(x, y) {
//   function init(x, y) {
//     this.bar = x;
//     this.foo = y;
//   }
//   const _this = Error.call(this, "FATAL CRASH");
//   init.call(_this, x, y);
//   Object.setPrototypeOf(_this, MyException.prototype);
//   return _this;
// }
// inherits(MyException, Error);

// F# EXCEPTIONS

function getFSharpExceptionFieldNames(self) {
  return Object.keys(self).filter(k => k !== "message" && k !== "stack");
}

export function Exception(msg) {
  this.stack = Error().stack;
  this.message = msg;
}

export function isException(x) {
  return x instanceof Error || x instanceof Exception;
}

export function FSharpException() {
  Exception.call(this);
}
inherits(FSharpException, Exception);

FSharpException.prototype.toString = function() {
  const fieldNames = getFSharpExceptionFieldNames(this);
  const len = fieldNames.length;
  if (len === 0) {
    return this.message;
  } else if (len === 1) {
    return this.message + " " + String(this[fieldNames[0]]);
  } else {
    return this.message + " (" + fieldNames.map(k => String(this[k])).join(",") + ")";
  }
};

FSharpException.prototype.toJSON = function() {
  return recordToJson(this, getFSharpExceptionFieldNames);
};

FSharpException.prototype.GetHashCode = function() {
  const hashes = getFSharpExceptionFieldNames(this).map(k => structuralHash(this[k]));
  return combineHashCodes(hashes);
};

FSharpException.prototype.Equals = function(other) {
  return recordEquals(this, other, getFSharpExceptionFieldNames);
};

FSharpException.prototype.CompareTo = function(other) {
  return recordCompare(this, other, getFSharpExceptionFieldNames);
};

export function MatchFailureException(arg1, arg2, arg3) {
  this.arg1 = arg1;
  this.arg2 = arg2 | 0;
  this.arg3 = arg3 | 0;
}
inherits(MatchFailureException, FSharpException);

export function Dictionary(source, comparer) {
  this.__mutableMap = createMutableMap(source, comparer);
}
Object.defineProperty(Dictionary.prototype, "size", { get: function() {
  return this.__mutableMap.size;
}});
Dictionary.prototype.clear = function() { return this.__mutableMap.clear(); };
Dictionary.prototype.delete = function(k) { return this.__mutableMap.delete(k); };
Dictionary.prototype.entries = function() { return this.__mutableMap.entries(); };
Dictionary.prototype.get = function(k) { return this.__mutableMap.get(k); };
Dictionary.prototype.has = function(k) { return this.__mutableMap.has(k); };
Dictionary.prototype.keys = function() { return this.__mutableMap.keys(); };
Dictionary.prototype.set = function(k, v) { return this.__mutableMap.set(k, v); };
Dictionary.prototype.values = function() { return this.__mutableMap.values(); };
Dictionary.prototype[Symbol.iterator] = function() { return this.__mutableMap[Symbol.iterator](); };

export function HashSet(source, comparer) {
  this.__mutableSet = createMutableSet(source, comparer);
}
Object.defineProperty(HashSet.prototype, "size", { get: function() {
  return this.__mutableSet.size;
}});
HashSet.prototype.add = function(v) { return this.__mutableSet.add(v); };
HashSet.prototype.clear = function() { return this.__mutableSet.clear(); };
HashSet.prototype.delete = function(k) { return this.__mutableSet.delete(k); };
HashSet.prototype.has = function(k) { return this.__mutableSet.has(k); };
HashSet.prototype.values = function() { return this.__mutableSet.values(); };
HashSet.prototype[Symbol.iterator] = function() { return this.__mutableSet[Symbol.iterator](); };

export function Attribute() {
}
