import { compare, toString } from "./Util";

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

export default class List {
  constructor(head, tail) {
    this.head = head;
    this.tail = tail;
  }

  toString() {
    return "[" + Array.from(this).map(x => toString(x)).join("; ") + "]";
  }

  toJSON() {
      return Array.from(this);
  }

  [Symbol.iterator]() {
    let cur = this;
    return {
      next: () => {
        const tmp = cur;
        cur = cur.tail;
        return { done: tmp.tail == null, value: tmp.head };
      },
    };
  }

  Equals(other) {
    return compareList(this, other) === 0;
  }

  CompareTo(other) {
    return compareList(this, other);
  }
}

export function L(h, t) {
  return new List(h, t);
}
