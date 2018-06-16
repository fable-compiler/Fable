import { compare, compareArrays, equals, equalArrays, toString } from "./Util";

export function sameType(x, y) {
  return Object.getPrototypeOf(x).constructor === Object.getPrototypeOf(y).constructor;
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
      configurable: true
    }
  });
  // if (superClass)
  //   Object.setPrototypeOf
  //     ? Object.setPrototypeOf(subClass, superClass)
  //     : (subClass.__proto__ = superClass);
}

function compareList(self, other) {
  if (self === other) {
    return 0;
  } else {
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

List.prototype.toString = function () {
  return "[" + Array.from(this).map(x => toString(x)).join("; ") + "]";
}

List.prototype.toJSON = function () {
  return Array.from(this);
}

List.prototype[Symbol.iterator] = function () {
  let cur = this;
  return {
    next: () => {
      const tmp = cur;
      cur = cur.tail;
      return { done: tmp.tail == null, value: tmp.head };
    },
  };
}

List.prototype.Equals = function (other) {
  return compareList(this, other) === 0;
}

List.prototype.CompareTo = function (other) {
  return compareList(this, other);
}

export function L(h, t) {
  return new List(h, t);
}

export function Union(tag, name, ...fields) {
  this.tag = tag | 0;
  this.name = name;
  this.fields = fields;
}

Union.prototype.toString = function () {
  const len = this.fields.length;
  if (len === 0) {
    return this.name;
  } else if (len === 1) {
    return this.name + " " + toString(this.fields[0]);
  } else {
    return this.name + " (" + this.fields.map(toString).join(",") + ")";
  }
}

Union.prototype.toJSON = function () {
  return this.fields.length === 0
    ? this.name
    : [this.name].concat(this.fields);
}

Union.prototype.Equals = function (other) {
  return this === other
    || (sameType(this, other)
        && this.tag === other.tag
        && equalArrays(this.fields, other.fields));
}

Union.prototype.CompareTo = function (other) {
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

function recordToJson(record) {
  const o = {};
  const keys = Object.keys(record);
  for (let i = 0; i < keys.length; i++) {
    o[keys[i]] = record[keys[i]];
  }
  return o;
}

function recordEquals(self, other) {
  if (self === other) {
    return true;
  } else if (!sameType(self, other)) {
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

function recordCompare(self, other) {
  if (self === other) {
    return 0;
  } else if (!sameType(self, other)) {
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

export function Record() {
}

Record.prototype.toString = function () {
  return "{" + Object.keys(this).map((k) => k + " = " + toString(obj[k])).join(";\n ") + "}";
}

Record.prototype.toJSON = function () {
  return recordToJson(this);
}

Record.prototype.Equals = function (other) {
  return recordEquals(this, other);
}

Record.prototype.CompareTo = function (other) {
  return recordCompare(this, other);
}

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

export function FSharpException(name) {
  const _this = Error.call(this, name);
  Object.setPrototypeOf(_this, FSharpException.prototype);
  return _this;
}
inherits(FSharpException, Error);

FSharpException.prototype.toString = function() {
  const fieldNames = Object.keys(this).filter(k => k !== "message" && k !== "stack");
  const len = fieldNames.length;
  if (len === 0) {
    return this.message;
  } else if (len === 1) {
    return this.message + " " + String(this[fieldNames[0]]);
  } else {
    return this.message + " (" + fieldNames.map(k => String(this[k])).join(",") + ")";
  }
}

FSharpException.prototype.toJSON = function () {
  return recordToJson(this);
}

FSharpException.prototype.Equals = function (other) {
  return recordEquals(this, other);
}

FSharpException.prototype.CompareTo = function (other) {
  return recordCompare(this, other);
}

// function MyFSharpException(x, y) {
//   function init(x, y) {
//     this.x = x;
//     this.y = y;
//   }
//   var _this = FSharpException.call(this, "MyFSharpException");
//   _this.x = x;
//   _this.y = y;
//   Object.setPrototypeOf(_this, MyFSharpException.prototype);
//   return _this;
// }
// inherits(MyFSharpException, FSharpException);

