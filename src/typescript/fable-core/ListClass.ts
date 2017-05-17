import FSymbol from "./Symbol";
import { IEquatable } from "./Util";
import { IComparable } from "./Util";
import { toString } from "./Util";
import { equals } from "./Util";
import { compare } from "./Util";

// This module is split from List.ts to prevent cyclic dependencies

export function ofArray<T>(args: T[], base?: List<T>) {
  let acc = base || new List<T>();
  for (let i = args.length - 1; i >= 0; i--) {
    acc = new List<T>(args[i], acc);
  }
  return acc;
}

export default class List<T> implements IEquatable<List<T>>, IComparable<List<T>>, Iterable<T> {
  public head: T;
  public tail: List<T>;

  constructor(head?: T, tail?: List<T>) {
    this.head = head;
    this.tail = tail;
  }

  public ToString() {
    return "[" + Array.from(this).map((x) => toString(x)).join("; ") + "]";
  }

  public Equals(x: List<T>) {
    // Optimization if they are referencially equal
    if (this === x) {
      return true;
    } else {
      const iter1 = this[Symbol.iterator]();
      const iter2 = x[Symbol.iterator]();
      while (true) {
        const cur1 = iter1.next();
        const cur2 = iter2.next();
        if (cur1.done) {
          return cur2.done ? true : false;
        } else if (cur2.done) {
          return false;
        } else if (!equals(cur1.value, cur2.value)) {
          return false;
        }
      }
    }
  }

  public CompareTo(x: List<T>) {
    // Optimization if they are referencially equal
    if (this === x) {
      return 0;
    } else {
      let acc = 0;
      const iter1 = this[Symbol.iterator]();
      const iter2 = x[Symbol.iterator]();
      while (true) {
        const cur1 = iter1.next();
        const cur2 = iter2.next();
        if (cur1.done) {
          return cur2.done ? acc : -1;
        } else if (cur2.done) {
          return 1;
        } else {
          acc = compare(cur1.value, cur2.value);
          if (acc !== 0) { return acc; }
        }
      }
    }
  }

  get length() {
    let cur: List<T> = this;
    let acc: number = 0;
    while (cur.tail != null) {
      cur = cur.tail;
      acc++;
    }
    return acc;
  }

  public [Symbol.iterator]() {
    let cur: List<T> = this;
    return {
      next: () => {
        const tmp = cur;
        cur = cur.tail;
        return { done: tmp.tail == null, value: tmp.head };
      },
    } as Iterator<T>;
  }

  //   append(ys: List<T>): List<T> {
  //     return append(this, ys);
  //   }

  //   choose<U>(f: (x: T) => U, xs: List<T>): List<U> {
  //     return choose(f, this);
  //   }

  //   collect<U>(f: (x: T) => List<U>): List<U> {
  //     return collect(f, this);
  //   }

  //   filter(f: (x: T) => boolean): List<T> {
  //     return filter(f, this);
  //   }

  //   where(f: (x: T) => boolean): List<T> {
  //     return filter(f, this);
  //   }

  //   map<U>(f: (x: T) => U): List<U> {
  //     return map(f, this);
  //   }

  //   mapIndexed<U>(f: (i: number, x: T) => U): List<U> {
  //     return mapIndexed(f, this);
  //   }

  //   partition(f: (x: T) => boolean): [List<T>, List<T>] {
  //     return partition(f, this) as [List<T>, List<T>];
  //   }

  //   reverse(): List<T> {
  //     return reverse(this);
  //   }

  //   slice(lower: number, upper: number): List<T> {
  //     return slice(lower, upper, this);
  //   }

  public [FSymbol.reflection]() {
    return {
      type: "Microsoft.FSharp.Collections.FSharpList",
      interfaces: ["System.IEquatable", "System.IComparable"],
    };
  }
}
