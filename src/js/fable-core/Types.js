import { compare, equalArrays, toString } from "./Util";

export function sameType(x, y) {
  return Object.getPrototypeOf(x).constructor === Object.getPrototypeOf(y).constructor;
}

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
  } else if (!sameType(x, y)) {
    return -1;
  } else if (this.tag === other.tag) {
    return compareArrays(this.fields, other.fields);
  } else {
    return this.tag < other.tag ? -1 : 1;
  }
}

// function Foo(tag, name, ...fields) {
//   Union.call(this, tag, name, ...fields);
// }
// inherits(Foo, Union);

// function MyRecord(a1, a2, a3, a4) {
//   this.foo = a1;
//   this.bar = a2;
// }
// inherits(MyRecord, Record);

// Foo.prototype.toString = function() { return "ajaja"}

// var c1a = new Foo(1, "Foo", 5, "bar");
// var c1b = new Foo(1, "Foo", 5, "bar");
// var c1c = new Foo(1, "Foo", 5, "barx");
// var c2 = new Bar(1, "Foo", 5, "bar");

// console.log(c1a.Equals(c1b));
// console.log(c1a.Equals(c1c));
// console.log(c1a.Equals(c2));

// console.log(String(c));

// class Record {
//   // TODO!!!
//   // public toJSON() {
//   // }

//   public Equals(other: Union) {
//     const thisNames = Object.getOwnPropertyNames(this);
//     const otherNames = Object.getOwnPropertyNames(other);
//     for (let i = 0; i < thisNames.length; i++) {
//       if (!equals((this as any)[thisNames[i]], (other as any)[otherNames[i]])) {
//         return false;
//       }
//     }
//     return true;
//   }

//   // TODO!!!
//   // public CompareTo(other: Union) {
//   //   if (this === other) {
//   //     return 0;
//   //   } else if (this.name === other.name) {
//   //     return compareArrays(this.fields, other.fields);
//   //   } else {
//   //     // TODO: We need the tag index to get the proper order
//   //     return this.name < other.name ? -1 : 1;
//   //   }
//   // }
// }

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

// var ex = new MyException(5, "$");

// F# EXCEPTIONS

export function FSharpException(name) {
  const _this = Error.call(this, name);
  Object.setPrototypeOf(_this, FSharpException.prototype);
  return _this;
}
inherits(FSharpException, Error);

FSharpException.prototype.toString = function() {
  const fieldNames =
    Object.getOwnPropertyNames(this)
      .filter(k => k !== "message" && k !== "stack");
  const len = fieldNames.length;
  if (len === 0) {
    return this.message;
  } else if (len === 1) {
    return this.message + " " + String(this[fieldNames[0]]);
  } else {
    return this.message + " (" + fieldNames.map(k => String(this[k])).join(",") + ")";
  }
}

// function MyFSharpException(x, y) {
//   function init(x, y) {
//     this.x = x;
//     this.y = y;
//   }
//   var _this = FSharpException.call(this, "MyFSharpException");
//   init.call(_this, x, y);
//   // Object.setPrototypeOf(_this, MyFSharpException.prototype);
//   return _this;
// }
// inherits(MyFSharpException, FSharpException);

// var ex2 = new MyFSharpException(1,2);
