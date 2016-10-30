import FSymbol from "./Symbol"
import { IEquatable } from "./Util"
import { IComparable } from "./Util"
import { toString } from "./Util"
import { equals } from "./Util"
import { compare } from "./Util"
import { map as seqMap } from "./Seq"
import { fold as seqFold } from "./Seq"
import { foldBack as seqFoldBack } from "./Seq"
import { toList as seqToList } from "./Seq"
import { groupBy as seqGroupBy } from "./Seq"

export function ofArray<T>(args: Array<T>, base?: List<T>) {
  let acc = base || new List<T>();
  for (let i = args.length - 1; i >= 0; i--) {
    acc = new List<T>(args[i], acc);
  }
  return acc;
}

export function append<T>(xs: List<T>, ys: List<T>) {
  return seqFold((acc, x) => new List<T>(x, acc), ys, reverse(xs));
}

export function choose<T, U>(f: (x: T) => U, xs: List<T>) {
  const r = seqFold((acc, x) => {
    const y = f(x);
    return y != null ? new List<U>(y, acc) : acc;
  }, new List<U>(), xs);
  return reverse(r);
}

export function collect<T, U>(f: (x: T) => List<U>, xs: List<T>) {
  return seqFold((acc, x) => append(acc, f(x)), new List<U>(), xs);
}
// TODO: should be xs: Iterable<List<T>>

export function concat<T>(xs: List<List<T>>) {
  return collect(x => x, xs);
}

export function filter<T>(f: (x: T) => boolean, xs: List<T>) {
  return reverse(seqFold((acc, x) => f(x) ? new List<T>(x, acc) : acc, new List<T>(), xs));
}

export function where<T>(f: (x: T) => boolean, xs: List<T>) {
  return filter(f, xs);
}

export function initialize<T>(n: number, f: (i: number) => T) {
  if (n < 0) {
    throw "List length must be non-negative";
  }
  let xs = new List<T>();
  for (let i = 1; i <= n; i++) {
    xs = new List<T>(f(n - i), xs);
  }
  return xs;
}

export function map<T, U>(f: (x: T) => U, xs: List<T>) {
  return reverse(seqFold((acc: List<U>, x: T) => new List<U>(f(x), acc), new List<U>(), xs));
}

export function mapIndexed<T, U>(f: (i: number, x: T) => U, xs: List<T>) {
  return reverse(seqFold((acc, x, i) => new List<U>(f(i, x), acc), new List<U>(), xs));
}

export function partition<T>(f: (x: T) => boolean, xs: List<T>) {
  return seqFold((acc, x) => {
    const lacc = acc[0], racc = acc[1];
    return f(x) ? [new List<T>(x, lacc), racc] : [lacc, new List<T>(x, racc)];
  }, [new List<T>(), new List<T>()], reverse(xs));
}

export function replicate<T>(n: number, x: T) {
  return initialize(n, () => x);
}

export function reverse<T>(xs: List<T>) {
  return seqFold((acc, x) => new List<T>(x, acc), new List<T>(), xs);
}

export function singleton<T>(x: T) {
  return new List<T>(x, new List<T>());
}

export function slice<T>(lower: number, upper: number, xs: List<T>) {
  const noLower = (lower == null);
  const noUpper = (upper == null);
  return reverse(seqFold((acc, x, i) => (noLower || lower <= i) && (noUpper || i <= upper) ? new List<T>(x, acc) : acc, new List<T>(), xs));
}
/* ToDo: instance unzip() */

export function unzip<T1, T2>(xs: List<[T1, T2]>) {
  return seqFoldBack((xy, acc) =>
    [new List<T1>(xy[0], acc[0]), new List<T2>(xy[1], acc[1])] as [List<T1>, List<T2>], xs, [new List<T1>(), new List<T2>()] as [List<T1>, List<T2>]);
}
/* ToDo: instance unzip3() */

export function unzip3<T1, T2, T3>(xs: List<[T1, T2, T3]>) {
  return seqFoldBack((xyz, acc) =>
    [new List<T1>(xyz[0], acc[0]), new List<T2>(xyz[1], acc[1]), new List<T3>(xyz[2], acc[2])] as [List<T1>, List<T2>, List<T3>], xs, [new List<T1>(), new List<T2>(), new List<T3>()] as [List<T1>, List<T2>, List<T3>]);
}

export function groupBy<T, K>(f: (x: T) => K, xs: List<T>): List<[K, List<T>]> {
  return seqToList(seqMap(k => [k[0], seqToList(k[1])], seqGroupBy(f, xs))) as List<[K, List<T>]>;
}

export default class List<T> implements IEquatable<List<T>>, IComparable<List<T>>, Iterable<T> {
  head: T;
  tail: List<T>;

  constructor(head?: T, tail?: List<T>) {
    this.head = head;
    this.tail = tail;
  }

  ToString() {
    return "[" + Array.from(this).map(toString).join("; ") + "]";
  }

  Equals(x: List<T>) {
    // Optimization if they are referencially equal
    if (this === x) {
      return true;
    }
    else {
      const iter1 = this[Symbol.iterator](), iter2 = x[Symbol.iterator]();
      for (;;) {
        let cur1 = iter1.next(), cur2 = iter2.next();
        if (cur1.done)
          return cur2.done ? true : false;
        else if (cur2.done)
          return false;
        else if (!equals(cur1.value, cur2.value))
          return false
      }
    }
  }

  CompareTo(x: List<T>) {
    // Optimization if they are referencially equal
    if (this === x) {
      return 0;
    }
    else {
      let acc = 0;
      const iter1 = this[Symbol.iterator](), iter2 = x[Symbol.iterator]();
      for (;;) {
        let cur1 = iter1.next(), cur2 = iter2.next();
        if (cur1.done)
          return cur2.done ? acc : -1;
        else if (cur2.done)
          return 1;
        else {
          acc = compare(cur1.value, cur2.value);
          if (acc != 0) return acc;
        }
      }
    }
  }

  get length() {
    return seqFold((acc, x) => acc + 1, 0, this);
  }

  [Symbol.iterator]() {
    let cur: List<T> = this;
    return <Iterator<T>>{
      next: () => {
        const tmp = cur;
        cur = cur.tail;
        return { done: tmp.tail == null, value: tmp.head };
      }
    };
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

  [FSymbol.interfaces]() {
    return ["System.IEquatable", "System.IComparable"];
  }

  [FSymbol.typeName]() {
    return "Microsoft.FSharp.Collections.FSharpList";
  }
}
