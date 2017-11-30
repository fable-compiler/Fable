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

  public Equals(other: List<T>) {
    // Optimization if they are referencially equal
    if (this === other) {
      return true;
    } else {
      let cur1: List<T> = this;
      let cur2 = other;
      while (equals(cur1.head, cur2.head)) {
        cur1 = cur1.tail;
        cur2 = cur2.tail;
        if (cur1 == null) {
          return cur2 == null;
        }
      }
      return false;
    }
  }

  public CompareTo(other: List<T>) {
    // Optimization if they are referencially equal
    if (this === other) {
      return 0;
    } else {
      let cur1: List<T> = this;
      let cur2 = other;
      let res = compare(cur1.head, cur2.head);
      while (res === 0) {
        cur1 = cur1.tail;
        cur2 = cur2.tail;
        if (cur1 == null) {
          return cur2 == null ? 0 : -1;
        }
        res = compare(cur1.head, cur2.head);
      }
      return res;
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
