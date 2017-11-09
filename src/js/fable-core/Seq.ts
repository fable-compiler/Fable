import { permute as arrayPermute } from "./Array";
import { chunkBySize as arrayChunkBySize } from "./Array";
import List from "./ListClass";
import { getValue, Some } from "./Option";
import { compare, equals, hasInterface, IDisposable } from "./Util";

export class Enumerator<T> {
  private current: T;
  constructor(private iter: Iterator<T>) { }
  public MoveNext() {
    const cur = this.iter.next();
    this.current = cur.value;
    return !cur.done;
  }
  get Current() { return this.current; }
  get get_Current() { return this.current; }
  public Reset() {
    throw new Error("JS iterators cannot be reset");
  }
  public Dispose() { return; }
}

export function getEnumerator<T>(o: any): Enumerator<T> {
  return typeof o.GetEnumerator === "function"
    ? o.GetEnumerator() : new Enumerator(o[Symbol.iterator]());
}

export function toIterator<T>(en: Enumerator<T>): Iterator<T> {
  return {
    next() {
      return en.MoveNext()
        ? { done: false, value: en.Current }
        : { done: true, value: null };
    },
  };
}

function __failIfNone<T>(res: T) {
  if (res == null) {
    throw new Error("Seq did not contain any matching element");
  }
  return getValue(res);
}

export function toList<T>(xs: Iterable<T>) {
  return foldBack((x, acc) =>
    new List(x, acc), xs, new List<T>());
}

export function ofList<T>(xs: List<T>) {
  return delay(() => unfold((x) => x.tail != null ? [x.head, x.tail] : null, xs));
}

export function ofArray<T>(xs: ArrayLike<T>) {
  return delay(() => unfold((i) => i < xs.length ? [xs[i], i + 1] : null, 0));
}

export function append<T>(xs: Iterable<T>, ys: Iterable<T>) {
  return delay(() => {
    let firstDone = false;
    const i = xs[Symbol.iterator]();
    let iters = [i, null as Iterator<T>];
    return unfold(() => {
      let cur: IteratorResult<T>;
      if (!firstDone) {
        cur = iters[0].next();
        if (!cur.done) {
          return [cur.value, iters];
        } else {
          firstDone = true;
          iters = [null as Iterator<T>, ys[Symbol.iterator]()];
        }
      }
      cur = iters[1].next();
      return !cur.done ? [cur.value, iters] : null;
    }, iters);
  });
}

export function average(xs: Iterable<number>) {
  let count = 1;
  const sum = reduce((acc: number, x: number) => {
    count++;
    return acc + x;
  }, xs);
  return sum / count;
}

export function averageBy(f: (a: number) => number, xs: Iterable<number>) {
  let count = 1;
  const sum = reduce((acc: number, x: number) => {
    count++;
    return (count === 2 ? f(acc) : acc) + f(x);
  }, xs);
  return sum / count;
}

export function concat<T>(xs: Iterable<Iterable<T>>) {
  return delay(() => {
    const iter = xs[Symbol.iterator]();
    let output: any = { value: null };
    return unfold((innerIter) => {
      let hasFinished = false;
      while (!hasFinished) {
        if (innerIter == null) {
          const cur = iter.next();
          if (!cur.done) {
            innerIter = cur.value[Symbol.iterator]();
          } else {
            hasFinished = true;
          }
        } else {
          const cur = innerIter.next();
          if (!cur.done) {
            output = { value: cur.value };
            hasFinished = true;
          } else {
            innerIter = null;
          }
        }
      }
      return innerIter != null && output != null ? [output.value, innerIter] : null;
    }, null);
  });
}

export function collect<T, U>(f: (x: T) => Iterable<U>, xs: Iterable<T>) {
  return concat(map(f, xs));
}

export function choose<T, U>(f: (x: T) => U, xs: Iterable<T>) {
  return delay(() => unfold((iter) => {
    let cur = iter.next();
    while (!cur.done) {
      const y = f(cur.value);
      if (y != null) {
        return [getValue(y), iter];
      }
      cur = iter.next();
    }
    return null;
  }, xs[Symbol.iterator]()));
}

export function compareWith<T>(f: (x: T, y: T) => number, xs: Iterable<T>, ys: Iterable<T>) {
  const nonZero = tryFind((i: number) => i !== 0, map2((x: T, y: T) => f(x, y), xs, ys));
  return nonZero != null ? getValue(nonZero) : count(xs) - count(ys);
}

export function delay<T>(f: () => Iterable<T>) {
  return {
    [Symbol.iterator]: () => f()[Symbol.iterator](),
  } as Iterable<T>;
}

export function empty<T>() {
  return unfold((): [T, T] => void 0);
}

export function enumerateWhile<T>(cond: () => boolean, xs: Iterable<T>) {
  return concat(unfold(() => cond() ? [xs, true] : null));
}

export function enumerateThenFinally<T>(xs: Iterable<T>, finalFn: () => void) {
  return delay(() => {
    let iter: Iterator<T>;
    try {
      iter = xs[Symbol.iterator]();
    } catch (err) {
      return void 0;
    } finally {
      finalFn();
    }
    return unfold((it) => {
      try {
        const cur = it.next();
        return !cur.done ? [cur.value, it] : null;
      } catch (err) {
        return void 0;
      } finally {
        finalFn();
      }
    }, iter);
  });
}

export function enumerateUsing<T extends IDisposable, U>(disp: T, work: (x: T) => Iterable<U>) {
  let isDisposed = false;
  const disposeOnce = () => {
    if (!isDisposed) {
      isDisposed = true;
      disp.Dispose();
    }
  };
  try {
    return enumerateThenFinally(work(disp), disposeOnce);
  } catch (err) {
    return void 0;
  } finally {
    disposeOnce();
  }
}

export function exactlyOne<T>(xs: Iterable<T>) {
  const iter = xs[Symbol.iterator]();
  const fst = iter.next();
  if (fst.done) {
    throw new Error("Seq was empty");
  }
  const snd = iter.next();
  if (!snd.done) {
    throw new Error("Seq had multiple items");
  }
  return fst.value;
}

export function except<T>(itemsToExclude: Iterable<T>, source: Iterable<T>) {
  const exclusionItems = Array.from(itemsToExclude);
  const testIsNotInExclusionItems = (element: T) =>
    !exclusionItems.some((excludedItem) => equals(excludedItem, element));
  return filter(testIsNotInExclusionItems, source);
}

export function exists<T>(f: (x: T) => boolean, xs: Iterable<T>) {
  let cur: IteratorResult<T>;
  for (const iter = xs[Symbol.iterator](); ; ) {
    cur = iter.next();
    if (cur.done) { break; }
    if (f(cur.value)) { return true; }
  }
  return false;
}

export function exists2<T1, T2>(f: (x: T1, y: T2) => boolean, xs: Iterable<T1>, ys: Iterable<T2>) {
  let cur1: IteratorResult<T1>;
  let cur2: IteratorResult<T2>;
  for (const iter1 = xs[Symbol.iterator](), iter2 = ys[Symbol.iterator](); ; ) {
    cur1 = iter1.next();
    cur2 = iter2.next();
    if (cur1.done || cur2.done) { break; }
    if (f(cur1.value, cur2.value)) { return true; }
  }
  return false;
}

export function filter<T>(f: (x: T) => boolean, xs: Iterable<T>) {
  return delay(() => unfold((iter) => {
    let cur = iter.next();
    while (!cur.done) {
      if (f(cur.value)) {
        return [cur.value, iter];
      }
      cur = iter.next();
    }
    return null;
  }, xs[Symbol.iterator]()));
}

export function where<T>(f: (x: T) => boolean, xs: Iterable<T>) {
  return filter(f, xs);
}

export function fold<T, ST>(f: (acc: ST, x: T, i?: number) => ST, acc: ST, xs: Iterable<T>) {
  if (Array.isArray(xs) || ArrayBuffer.isView(xs)) {
    return (xs as T[]).reduce(f, acc);
  } else {
    let cur: IteratorResult<T>;
    for (let i = 0, iter = xs[Symbol.iterator](); ; i++) {
      cur = iter.next();
      if (cur.done) { break; }
      acc = f(acc, cur.value, i);
    }
    return acc;
  }
}

export function foldBack<T, ST>(f: (x: T, acc: ST, i?: number) => ST, xs: Iterable<T>, acc: ST) {
  const arr = Array.isArray(xs) || ArrayBuffer.isView(xs) ? xs as T[] : Array.from(xs);
  for (let i = arr.length - 1; i >= 0; i--) {
    acc = f(arr[i], acc, i);
  }
  return acc;
}

export function fold2<T1, T2, ST>(
  f: (acc: ST, x: T1, y: T2, i?: number) => ST, acc: ST, xs: Iterable<T1>, ys: Iterable<T2>) {
  const iter1 = xs[Symbol.iterator]();
  const iter2 = ys[Symbol.iterator]();
  let cur1: IteratorResult<T1>;
  let cur2: IteratorResult<T2>;
  for (let i = 0; ; i++) {
    cur1 = iter1.next();
    cur2 = iter2.next();
    if (cur1.done || cur2.done) {
      break;
    }
    acc = f(acc, cur1.value, cur2.value, i);
  }
  return acc;
}

export function foldBack2<T1, T2, ST>(
  f: (x: T1, y: T2, acc: ST, i?: number) => ST, xs: Iterable<T1>, ys: Iterable<T2>, acc: ST) {
  const ar1: T1[] = Array.isArray(xs) || ArrayBuffer.isView(xs) ? xs as T1[] : Array.from(xs);
  const ar2: T2[] = Array.isArray(ys) || ArrayBuffer.isView(ys) ? ys as T2[] : Array.from(ys);
  for (let i = ar1.length - 1; i >= 0; i--) {
    acc = f(ar1[i], ar2[i], acc, i);
  }
  return acc;
}

export function forAll<T>(f: (x: T) => boolean, xs: Iterable<T>) {
  return fold((acc, x) => acc && f(x), true, xs);
}

export function forAll2<T1, T2>(f: (x: T1, y: T2) => boolean, xs: Iterable<T1>, ys: Iterable<T2>) {
  return fold2((acc, x, y) => acc && f(x, y), true, xs, ys);
}

export function tryHead<T>(xs: Iterable<T>): Some<T> {
  const iter = xs[Symbol.iterator]();
  const cur = iter.next();
  return cur.done ? null : new Some(cur.value);
}

export function head<T>(xs: Iterable<T>): T {
  return __failIfNone(tryHead(xs));
}

export function initialize<T>(n: number, f: (i: number) => T) {
  return delay(() =>
    unfold((i) => i < n ? [f(i), i + 1] : null, 0));
}

export function initializeInfinite<T>(f: (i: number) => T) {
  return delay(() =>
    unfold((i) => [f(i), i + 1], 0));
}

export function tryItem<T>(i: number, xs: Iterable<T>): Some<T> {
  if (i < 0) {
    return null;
  }
  if (Array.isArray(xs) || ArrayBuffer.isView(xs)) {
    return i < (xs as T[]).length ? new Some((xs as T[])[i]) : null;
  }
  for (let j = 0, iter = xs[Symbol.iterator](); ; j++) {
    const cur = iter.next();
    if (cur.done) {
      break;
    }
    if (j === i) {
      return new Some(cur.value);
    }
  }
  return null;
}

export function item<T>(i: number, xs: Iterable<T>): T {
  return __failIfNone(tryItem(i, xs));
}

export function iterate<T>(f: (x: T) => void, xs: Iterable<T>) {
  fold((_, x) => f(x), null, xs);
}

export function iterate2<T1, T2>(f: (x: T1, y: T2) => void, xs: Iterable<T1>, ys: Iterable<T2>) {
  fold2((_, x, y) => f(x, y), null, xs, ys);
}

export function iterateIndexed<T>(f: (i: number, x: T) => void, xs: Iterable<T>) {
  fold((_, x, i) => f(i, x), null, xs);
}

export function iterateIndexed2<T1, T2>(f: (i: number, x: T1, y: T2) => void, xs: Iterable<T1>, ys: Iterable<T2>) {
  fold2((_, x, y, i) => f(i, x, y), null, xs, ys);
}

export function isEmpty<T>(xs: Iterable<T>) {
  const i = xs[Symbol.iterator]();
  return i.next().done;
}

export function tryLast<T>(xs: Iterable<T>): Some<T> {
  try {
    return new Some(reduce((_, x) => x, xs));
  } catch (err) {
    return null;
  }
}

export function last<T>(xs: Iterable<T>): T {
  return __failIfNone(tryLast(xs));
}

// A export function 'length' method causes problems in JavaScript -- https://github.com/Microsoft/TypeScript/issues/442

export function count<T>(xs: Iterable<T>) {
  return Array.isArray(xs) || ArrayBuffer.isView(xs)
    ? (xs as T[]).length
    : fold((acc, x) => acc + 1, 0, xs);
}

export function map<T, U>(f: (x: T) => U, xs: Iterable<T>) {
  return delay(() => unfold((iter) => {
    const cur = iter.next();
    return !cur.done ? [f(cur.value), iter] : null;
  }, xs[Symbol.iterator]()));
}

export function mapIndexed<T, U>(f: (i: number, x: T) => U, xs: Iterable<T>) {
  return delay(() => {
    let i = 0;
    return unfold((iter) => {
      const cur = iter.next();
      return !cur.done ? [f(i++, cur.value), iter] : null;
    }, xs[Symbol.iterator]());
  });
}

export function indexed<T>(xs: Iterable<T>) {
  return mapIndexed((i, x) => [i, x] as [number, T], xs);
}

export function map2<T1, T2, U>(f: (x: T1, y: T2) => U, xs: Iterable<T1>, ys: Iterable<T2>) {
  return delay(() => {
    const iter1 = xs[Symbol.iterator]();
    const iter2 = ys[Symbol.iterator]();
    return unfold(() => {
      const cur1 = iter1.next();
      const cur2 = iter2.next();
      return !cur1.done && !cur2.done ? [f(cur1.value, cur2.value), null] : null;
    });
  });
}

export function mapIndexed2<T1, T2, U>(f: (i: number, x: T1, y: T2) => U, xs: Iterable<T1>, ys: Iterable<T2>) {
  return delay(() => {
    let i = 0;
    const iter1 = xs[Symbol.iterator]();
    const iter2 = ys[Symbol.iterator]();
    return unfold(() => {
      const cur1 = iter1.next();
      const cur2 = iter2.next();
      return !cur1.done && !cur2.done ? [f(i++, cur1.value, cur2.value), null] : null;
    });
  });
}

export function map3<T1, T2, T3, U>(
  f: (x: T1, y: T2, z: T3) => U, xs: Iterable<T1>, ys: Iterable<T2>, zs: Iterable<T3>) {
  return delay(() => {
    const iter1 = xs[Symbol.iterator]();
    const iter2 = ys[Symbol.iterator]();
    const iter3 = zs[Symbol.iterator]();
    return unfold(() => {
      const cur1 = iter1.next();
      const cur2 = iter2.next();
      const cur3 = iter3.next();
      return !cur1.done && !cur2.done && !cur3.done ? [f(cur1.value, cur2.value, cur3.value), null] : null;
    });
  });
}

export function chunkBySize<T>(size: number, xs: Iterable<T>): Iterable<Iterable<T>> {
  const result = arrayChunkBySize(size, Array.from(xs));
  return ofArray(result.map(ofArray));
}

export function mapFold<T, ST, R>(
  f: (acc: ST, x: T) => [R, ST], acc: ST, xs: Iterable<T>,
  transform?: (r: Iterable<R>) => Iterable<R>): [Iterable<R>, ST] {
  const result: R[] = [];
  let r: R;
  let cur: IteratorResult<T>;
  for (let i = 0, iter = xs[Symbol.iterator](); ; i++) {
    cur = iter.next();
    if (cur.done) {
      break;
    }
    [r, acc] = f(acc, cur.value);
    result.push(r);
  }
  return transform !== void 0 ? [transform(result), acc] : [result, acc];
}

export function mapFoldBack<T, ST, R>(
  f: (x: T, acc: ST) => [R, ST], xs: Iterable<T>, acc: ST,
  transform?: (r: Iterable<R>) => Iterable<R>): [Iterable<R>, ST] {
  const arr = Array.isArray(xs) || ArrayBuffer.isView(xs) ? xs as T[] : Array.from(xs);
  const result: R[] = [];
  let r: R;
  for (let i = arr.length - 1; i >= 0; i--) {
    [r, acc] = f(arr[i], acc);
    result.push(r);
  }
  return transform !== void 0 ? [transform(result), acc] : [result, acc];
}

export function max<T extends number>(xs: Iterable<T>) {
  return reduce((acc: T, x: T) => compare(acc, x) === 1 ? acc : x, xs);
}

export function maxBy<T, U extends number>(f: (x: T) => U, xs: Iterable<T>) {
  return reduce((acc: T, x: T) => compare(f(acc), f(x)) === 1 ? acc : x, xs);
}

export function min<T extends number>(xs: Iterable<T>) {
  return reduce((acc: T, x: T) => compare(acc, x) === -1 ? acc : x, xs);
}

export function minBy<T, U extends number>(f: (x: T) => U, xs: Iterable<T>) {
  return reduce((acc: T, x: T) => compare(f(acc), f(x)) === -1 ? acc : x, xs);
}

export function pairwise<T extends number>(xs: Iterable<T>) {
  return skip(2, scan((last, next) => [last[1], next], [0, 0], xs));
}

export function permute<T>(f: (i: number) => number, xs: Iterable<T>) {
  return ofArray(arrayPermute(f, Array.from(xs)));
}

export function rangeStep(first: number, step: number, last: number) {
  if (step === 0) {
    throw new Error("Step cannot be 0");
  }
  return delay(() => unfold((x) => step > 0 && x <= last || step < 0 && x >= last ? [x, x + step] : null, first));
}

export function rangeChar(first: string, last: string) {
  return delay(() => unfold((x) => x <= last ? [x, String.fromCharCode(x.charCodeAt(0) + 1)] : null, first));
}

export function range(first: number, last: number) {
  return rangeStep(first, 1, last);
}

export function readOnly<T>(xs: Iterable<T>) {
  return map((x) => x, xs);
}

export function reduce<T>(f: (acc: T, x: T) => T, xs: Iterable<T>) {
  if (Array.isArray(xs) || ArrayBuffer.isView(xs)) {
    return (xs as T[]).reduce(f);
  }
  const iter = xs[Symbol.iterator]();
  let cur = iter.next();
  if (cur.done) {
    throw new Error("Seq was empty");
  }
  let acc = cur.value;
  while (true) {
    cur = iter.next();
    if (cur.done) {
      break;
    }
    acc = f(acc, cur.value);
  }
  return acc;
}

export function reduceBack<T>(f: (acc: T, x: T, i?: number) => T, xs: Iterable<T>) {
  const ar = Array.isArray(xs) || ArrayBuffer.isView(xs) ? xs as T[] : Array.from(xs);
  if (ar.length === 0) {
    throw new Error("Seq was empty");
  }
  let acc = ar[ar.length - 1];
  for (let i = ar.length - 2; i >= 0; i--) {
    acc = f(ar[i], acc, i);
  }
  return acc;
}

export function replicate<T>(n: number, x: T) {
  return initialize(n, () => x);
}

export function reverse<T>(xs: Iterable<T>) {
  const ar = Array.isArray(xs) || ArrayBuffer.isView(xs) ? (xs as T[]).slice(0) : Array.from(xs);
  return ofArray(ar.reverse());
}

export function scan<T, ST>(f: (st: ST, x: T) => ST, seed: ST, xs: Iterable<T>) {
  return delay(() => {
    const iter = xs[Symbol.iterator]();
    return unfold((acc) => {
      if (acc == null) {
        return [seed, seed];
      }
      const cur = iter.next();
      if (!cur.done) {
        acc = f(acc, cur.value);
        return [acc, acc];
      }
      return void 0;
    }, null as ST);
  });
}

export function scanBack<T, ST>(f: (x: T, st: ST) => ST, xs: Iterable<T>, seed: ST) {
  return reverse(scan((acc, x) => f(x, acc), seed, reverse(xs)));
}

export function singleton<T>(y: T) {
  return unfold((x) => x != null ? [x, null] : null, y);
}

export function skip<T>(n: number, xs: Iterable<T>) {
  return {
    [Symbol.iterator]: () => {
      const iter = xs[Symbol.iterator]();
      for (let i = 1; i <= n; i++) {
        if (iter.next().done) {
          throw new Error("Seq has not enough elements");
        }
      }
      return iter;
    },
  } as Iterable<T>;
}

export function skipWhile<T>(f: (x: T) => boolean, xs: Iterable<T>) {
  return delay(() => {
    let hasPassed = false;
    return filter((x) => hasPassed || (hasPassed = !f(x)), xs);
  });
}

export function sortWith<T>(f: (x: T, y: T) => number, xs: Iterable<T>) {
  const ys = Array.from(xs);
  return ofArray(ys.sort(f));
}

export function sum(xs: Iterable<number>) {
  return fold((acc, x) => acc + x, 0, xs);
}

export function sumBy<T>(f: (x: T) => number, xs: Iterable<T>) {
  return fold((acc, x) => acc + f(x), 0, xs);
}

export function tail<T>(xs: Iterable<T>) {
  const iter = xs[Symbol.iterator]();
  const cur = iter.next();
  if (cur.done) {
    throw new Error("Seq was empty");
  }
  return {
    [Symbol.iterator]: () => iter,
  } as Iterable<T>;
}

export function take<T>(n: number, xs: Iterable<T>, truncate: boolean = false) {
  return delay(() => {
    const iter = xs[Symbol.iterator]();
    return unfold((i) => {
      if (i < n) {
        const cur = iter.next();
        if (!cur.done) {
          return [cur.value, i + 1];
        }
        if (!truncate) {
          throw new Error("Seq has not enough elements");
        }
      }
      return void 0;
    }, 0);
  });
}

export function truncate<T>(n: number, xs: Iterable<T>) {
  return take(n, xs, true);
}

export function takeWhile<T>(f: (x: T) => boolean, xs: Iterable<T>) {
  return delay(() => {
    const iter = xs[Symbol.iterator]();
    return unfold((i) => {
      const cur = iter.next();
      if (!cur.done && f(cur.value)) {
        return [cur.value, null];
      }
      return void 0;
    }, 0);
  });
}

export function tryFind<T>(f: (x: T, i?: number) => boolean, xs: Iterable<T>, defaultValue?: T): Some<T> {
  for (let i = 0, iter = xs[Symbol.iterator](); ; i++) {
    const cur = iter.next();
    if (cur.done) {
      break;
    }
    if (f(cur.value, i)) {
      return new Some(cur.value);
    }
  }
  return defaultValue === void 0 ? null : new Some(defaultValue);
}

export function find<T>(f: (x: T, i?: number) => boolean, xs: Iterable<T>): T {
  return __failIfNone(tryFind(f, xs));
}

export function tryFindBack<T>(f: (x: T, i?: number) => boolean, xs: Iterable<T>, defaultValue?: T): Some<T> {
  const arr = Array.isArray(xs) || ArrayBuffer.isView(xs) ? (xs as T[]).slice(0) : Array.from(xs);
  return tryFind(f, arr.reverse(), defaultValue);
}

export function findBack<T>(f: (x: T, i?: number) => boolean, xs: Iterable<T>): T {
  return __failIfNone(tryFindBack(f, xs));
}

export function tryFindIndex<T>(f: (x: T, i?: number) => boolean, xs: Iterable<T>) {
  for (let i = 0, iter = xs[Symbol.iterator](); ; i++) {
    const cur = iter.next();
    if (cur.done) {
      break;
    }
    if (f(cur.value, i)) {
      return i;
    }
  }
  return null;
}

export function findIndex<T>(f: (x: T, i?: number) => boolean, xs: Iterable<T>): number {
  return __failIfNone(tryFindIndex(f, xs));
}

export function tryFindIndexBack<T>(f: (x: T, i?: number) => boolean, xs: Iterable<T>) {
  const arr = Array.isArray(xs) || ArrayBuffer.isView(xs) ? (xs as T[]).slice(0) : Array.from(xs);
  for (let i = arr.length - 1; i >= 0 ; i--) {
    if (f(arr[i], i)) {
      return i;
    }
  }
  return null;
}

export function findIndexBack<T>(f: (x: T, i?: number) => boolean, xs: Iterable<T>): number {
  return __failIfNone(tryFindIndexBack(f, xs));
}

export function tryPick<T, U>(f: (x: T, i?: number) => Some<U>, xs: Iterable<T>): Some<U> {
  for (let i = 0, iter = xs[Symbol.iterator](); ; i++) {
    const cur = iter.next();
    if (cur.done) {
      break;
    }
    const y = f(cur.value, i);
    if (y != null) {
      return y;
    }
  }
  return null;
}

export function pick<T, U>(f: (x: T, i?: number) => Some<U>, xs: Iterable<T>): U {
  return __failIfNone(tryPick(f, xs));
}

export function unfold<T, ST>(f: (st: ST) => [T, ST], acc?: ST) {
  return {
    [Symbol.iterator]: () => {
      return {
        next: () => {
          const res = f(acc);
          if (res != null) {
            acc = res[1];
            return { done: false, value: res[0] };
          }
          return { done: true };
        },
      };
    },
  } as Iterable<T>;
}

export function zip<T1, T2>(xs: Iterable<T1>, ys: Iterable<T2>) {
  return map2((x, y) => [x, y], xs, ys);
}

export function zip3<T1, T2, T3>(xs: Iterable<T1>, ys: Iterable<T2>, zs: Iterable<T3>) {
  return map3((x, y, z) => [x, y, z], xs, ys, zs);
}
