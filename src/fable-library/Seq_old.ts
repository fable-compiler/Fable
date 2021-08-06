// import Decimal, { makeRangeStepFunction as makeDecimalRangeStepFunction } from "./Decimal.js";
// import Long, { makeRangeStepFunction as makeLongRangeStepFunction } from "./Long.js";
import { Option, some, value } from "./Option.js";
import { compare, equals, IComparer, IDisposable } from "./Util.js";

export interface IEnumerator<T> extends IDisposable {
  ["System.Collections.Generic.IEnumerator`1.get_Current"](): T | undefined;
  ["System.Collections.IEnumerator.get_Current"](): T | undefined;
  ["System.Collections.IEnumerator.MoveNext"](): boolean;
  ["System.Collections.IEnumerator.Reset"](): void;
  Dispose(): void;
}

export interface IEnumerable<T> extends Iterable<T> {
  GetEnumerator(): IEnumerator<T>;
}

interface IGenericAdder<T> {
  GetZero(): T;
  Add(x: T, y: T): T;
}

interface IGenericAverager<T> {
  GetZero(): T;
  Add(x: T, y: T): T;
  DivideByInt(x: T, i: number): T;
}

export class Enumerator<T> implements IEnumerator<T> {
  private current?: T;
  constructor(private iter: Iterator<T>) { }
  public ["System.Collections.Generic.IEnumerator`1.get_Current"]() {
    return this.current;
  }
  public ["System.Collections.IEnumerator.get_Current"]() {
    return this.current;
  }
  public ["System.Collections.IEnumerator.MoveNext"]() {
    const cur = this.iter.next();
    this.current = cur.value;
    return !cur.done;
  }
  public ["System.Collections.IEnumerator.Reset"]() {
    throw new Error("JS iterators cannot be reset");
  }
  public Dispose() {
    return;
  }
}

export function getEnumerator<T>(o: Iterable<T>): IEnumerator<T> {
  return typeof (o as any).GetEnumerator === "function"
    ? (o as IEnumerable<T>).GetEnumerator()
    : new Enumerator(o[Symbol.iterator]());
}

export function toIterator<T>(en: IEnumerator<T>): IterableIterator<T> {
  return {
    [Symbol.iterator]() { return this; },
    next() {
      const hasNext = en["System.Collections.IEnumerator.MoveNext"]();
      const current = hasNext ? en["System.Collections.IEnumerator.get_Current"]() : undefined;
      return { done: !hasNext, value: current } as IteratorResult<T>;
    },
  };
}

// export function toIterable<T>(en: IEnumerable<T>): Iterable<T> {
//   return {
//     [Symbol.iterator]() {
//       return toIterator(en.GetEnumerator());
//     },
//   };
// }

function __failIfNone<T>(res: Option<T>) {
  if (res == null) {
    throw new Error("Seq did not contain any matching element");
  }
  return value(res);
}

class Seq<T> implements IterableIterator<T> {
  private iter?: Iterator<T>;
  constructor(private f: () => Iterator<T>) { }
  [Symbol.iterator]() { return new Seq(this.f); }
  next() {
    this.iter = this.iter ?? this.f();
    return this.iter.next();
  }
  toString() {
    return "seq [" + Array.from(this).join("; ") + "]";
  }
}

function makeSeq<T>(f: () => Iterator<T>): IterableIterator<T> {
  return new Seq(f);
}

function isArrayOrBufferView<T>(xs: Iterable<T>): xs is T[] {
  return Array.isArray(xs) || ArrayBuffer.isView(xs);
}

export function ofArray<T>(xs: ArrayLike<T>) {
  if (Array.isArray(xs)) {
    return delay(() => xs);
  } else {
    return delay(() => unfold((i) => i != null && i < xs.length ? [xs[i], i + 1] : undefined, 0));
  }
}

export function allPairs<T1, T2>(xs: Iterable<T1>, ys: Iterable<T2>): IterableIterator<[T1, T2]> {
  let firstEl = true;
  const ysCache: T2[] = [];
  return collect((x: T1) => {
    if (firstEl) {
      firstEl = false;
      return map((y: T2) => {
        ysCache.push(y);
        return [x, y];
      }, ys);
    } else {
      return ysCache.map((y) => [x, y]);
      // return map(function (i) {
      //     return [x, ysCache[i]];
      // }, rangeNumber(0, 1, ysCache.length - 1));
    }
  }, xs) as IterableIterator<[T1, T2]>;
}

export function append<T>(xs: Iterable<T>, ys: Iterable<T>) {
  return delay(() => {
    let firstDone = false;
    const i = xs[Symbol.iterator]();
    let iters = [i, undefined];
    return unfold(() => {
      let cur: IteratorResult<T> | undefined;
      if (!firstDone) {
        cur = iters[0]?.next();
        if (cur != null && !cur.done) {
          return [cur.value, iters];
        } else {
          firstDone = true;
          iters = [undefined, ys[Symbol.iterator]()];
        }
      }
      cur = iters[1]?.next();
      return cur != null && !cur.done ? [cur.value, iters] : undefined;
    }, iters);
  });
}

export function average<T>(xs: Iterable<T>, averager: IGenericAverager<T>): T {
  let count = 0;
  const total = fold((acc, x) => {
    count++;
    return averager.Add(acc, x);
  }, averager.GetZero(), xs);
  return averager.DivideByInt(total, count);
}

export function averageBy<T, T2>(f: (a: T) => T2, xs: Iterable<T>, averager: IGenericAverager<T2>): T2 {
  let count = 0;
  const total = fold((acc, x) => {
    count++;
    return averager.Add(acc, f(x));
  }, averager.GetZero(), xs);
  return averager.DivideByInt(total, count);
}

export function concat<T>(xs: IterableIterator<Iterable<T>>) {
  return delay(() => {
    const iter = xs[Symbol.iterator]();
    let output: T;
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
            output = cur.value;
            hasFinished = true;
          } else {
            innerIter = undefined;
          }
        }
      }
      return innerIter != null ? [output, innerIter] : undefined;
    }, undefined as Iterator<T> | undefined);
  });
}

export function collect<T, U>(f: (x: T) => Iterable<U>, xs: Iterable<T>): IterableIterator<U> {
  return concat(map(f, xs));
}

export function choose<T, U>(f: (x: T) => U, xs: Iterable<T>) {
  return delay(() => unfold((iter) => {
    let cur = iter.next();
    while (!cur.done) {
      const y = f(cur.value);
      if (y != null) {
        return [value(y), iter];
      }
      cur = iter.next();
    }
    return undefined;
  }, xs[Symbol.iterator]()));
}

export function compareWith<T>(f: (x: T, y: T) => number, xs: Iterable<T>, ys: Iterable<T>) {
  if (xs === ys) { return 0; }
  let cur1: IteratorResult<T>;
  let cur2: IteratorResult<T>;
  let c = 0;
  for (const iter1 = xs[Symbol.iterator](), iter2 = ys[Symbol.iterator](); ;) {
    cur1 = iter1.next();
    cur2 = iter2.next();
    if (cur1.done || cur2.done) { break; }
    c = f(cur1.value, cur2.value);
    if (c !== 0) { break; }
  }
  return (c !== 0) ? c : (cur1.done && !cur2.done) ? -1 : (!cur1.done && cur2.done) ? 1 : 0;
}

export function delay<T>(f: () => Iterable<T>): IterableIterator<T> {
  return makeSeq(() => f()[Symbol.iterator]());
}

export function empty<T>(): IterableIterator<T> {
  return delay(() => []);
}

export function singleton<T>(y: T): IterableIterator<T> {
  return delay(() => [y]);
}

export function enumerateFromFunctions<T, Enumerator>(
  factory: () => Enumerator,
  moveNext: (e: Enumerator) => boolean,
  current: (e: Enumerator) => T) {
  return delay(() => unfold((e) => moveNext(e) ? [current(e), e] : undefined, factory()));
}

export function enumerateWhile<T>(cond: () => boolean, xs: Iterable<T>) {
  return concat(unfold(() => cond() ? [xs, true] : undefined, undefined));
}

export function enumerateThenFinally<T>(xs: Iterable<T>, finalFn: () => void) {
  return delay(() => {
    let iter: Iterator<T>;
    try {
      iter = xs[Symbol.iterator]();
    } catch (err) {
      try {
        return empty<T>();
      } finally {
        finalFn();
      }
    }
    return unfold((it) => {
      try {
        const cur = it.next();
        return !cur.done ? [cur.value, it] : undefined;
      } catch (err) {
        return undefined;
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
  for (const iter = xs[Symbol.iterator](); ;) {
    cur = iter.next();
    if (cur.done) { break; }
    if (f(cur.value)) { return true; }
  }
  return false;
}

export function exists2<T1, T2>(f: (x: T1, y: T2) => boolean, xs: Iterable<T1>, ys: Iterable<T2>) {
  let cur1: IteratorResult<T1>;
  let cur2: IteratorResult<T2>;
  for (const iter1 = xs[Symbol.iterator](), iter2 = ys[Symbol.iterator](); ;) {
    cur1 = iter1.next();
    cur2 = iter2.next();
    if (cur1.done || cur2.done) { break; }
    if (f(cur1.value, cur2.value)) { return true; }
  }
  return false;
}

export function forAll<T>(f: (x: T) => boolean, xs: Iterable<T>) {
  return !exists((x) => !f(x), xs);
}

export function forAll2<T1, T2>(f: (x: T1, y: T2) => boolean, xs: Iterable<T1>, ys: Iterable<T2>) {
  return !exists2((x, y) => !f(x, y), xs, ys);
}

export function contains<T>(i: T, xs: Iterable<T>) {
  return exists((x) => equals(x, i), xs);
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
    return undefined;
  }, xs[Symbol.iterator]()));
}

export function where<T>(f: (x: T) => boolean, xs: Iterable<T>) {
  return filter(f, xs);
}

export function fold<T, ST>(f: (acc: ST, x: T, i?: number) => ST, acc: ST, xs: Iterable<T>) {
  if (isArrayOrBufferView(xs)) {
    return xs.reduce(f, acc);
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
  const arr = isArrayOrBufferView(xs) ? xs as T[] : Array.from(xs);
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
  const ar1: T1[] = isArrayOrBufferView(xs) ? xs as T1[] : Array.from(xs);
  const ar2: T2[] = isArrayOrBufferView(ys) ? ys as T2[] : Array.from(ys);
  for (let i = ar1.length - 1; i >= 0; i--) {
    acc = f(ar1[i], ar2[i], acc, i);
  }
  return acc;
}

export function tryHead<T>(xs: Iterable<T>): Option<T> {
  const iter = xs[Symbol.iterator]();
  const cur = iter.next();
  return cur.done ? undefined : some(cur.value);
}

export function head<T>(xs: Iterable<T>) {
  return __failIfNone(tryHead(xs));
}

export function initialize<T>(n: number, f: (i: number) => T) {
  return delay(() =>
    unfold((i) => i < n ? [f(i), i + 1] : undefined, 0));
}

export function initializeInfinite<T>(f: (i: number) => T) {
  return delay(() =>
    unfold((i) => [f(i), i + 1], 0));
}

export function tryItem<T>(i: number, xs: Iterable<T>): Option<T> {
  if (i < 0) {
    return undefined;
  }
  if (isArrayOrBufferView(xs)) {
    return i < (xs as T[]).length ? some((xs as T[])[i]) : undefined;
  }
  for (let j = 0, iter = xs[Symbol.iterator](); ; j++) {
    const cur = iter.next();
    if (cur.done) {
      break;
    }
    if (j === i) {
      return some(cur.value);
    }
  }
  return undefined;
}

export function item<T>(i: number, xs: Iterable<T>) {
  return __failIfNone(tryItem(i, xs));
}

export function iterate<T>(f: (x: T) => void, xs: Iterable<T>) {
  fold((_, x) => (f(x), undefined), undefined, xs);
}

export function iterate2<T1, T2>(f: (x: T1, y: T2) => void, xs: Iterable<T1>, ys: Iterable<T2>) {
  fold2((_, x, y) => (f(x, y), undefined), undefined, xs, ys);
}

export function iterateIndexed<T>(f: (i: number, x: T) => void, xs: Iterable<T>) {
  fold((_, x, i) => (f(i ?? 0, x), undefined), undefined, xs);
}

export function iterateIndexed2<T1, T2>(f: (i: number, x: T1, y: T2) => void, xs: Iterable<T1>, ys: Iterable<T2>) {
  fold2((_, x, y, i) => (f(i ?? 0, x, y), undefined), undefined, xs, ys);
}

export function isEmpty<T>(xs: Iterable<T>) {
  const i = xs[Symbol.iterator]();
  return i.next().done;
}

export function tryLast<T>(xs: Iterable<T>): Option<T> {
  return isEmpty(xs) ? undefined : some(reduce((_, x) => x, xs));
}

export function last<T>(xs: Iterable<T>): T {
  return __failIfNone(tryLast(xs));
}

export function length<T>(xs: Iterable<T>) {
  return isArrayOrBufferView(xs)
    ? (xs as T[]).length
    : fold((acc, _x) => acc + 1, 0, xs);
}

export function map<T, U>(f: (x: T) => U, xs: Iterable<T>): IterableIterator<U> {
  return delay(() => unfold((iter) => {
    const cur = iter.next();
    return !cur.done ? [f(cur.value), iter] : undefined;
  }, xs[Symbol.iterator]()));
}

export function mapIndexed<T, U>(f: (i: number, x: T) => U, xs: Iterable<T>) {
  return delay(() => {
    let i = 0;
    return unfold((iter) => {
      const cur = iter.next();
      return !cur.done ? [f(i++, cur.value), iter] : undefined;
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
      return !cur1.done && !cur2.done ? [f(cur1.value, cur2.value), undefined] : undefined;
    }, undefined);
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
      return !cur1.done && !cur2.done ? [f(i++, cur1.value, cur2.value), undefined] : undefined;
    }, undefined);
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
      return !cur1.done && !cur2.done && !cur3.done ? [f(cur1.value, cur2.value, cur3.value), undefined] : undefined;
    }, undefined);
  });
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
  const arr = isArrayOrBufferView(xs) ? xs as T[] : Array.from(xs);
  const result: R[] = [];
  let r: R;
  for (let i = arr.length - 1; i >= 0; i--) {
    [r, acc] = f(arr[i], acc);
    result.push(r);
  }
  return transform !== void 0 ? [transform(result), acc] : [result, acc];
}

export function max<T>(xs: Iterable<T>, comparer?: IComparer<T>) {
  const compareFn = comparer != null ? comparer.Compare : compare;
  return reduce((acc: T, x: T) => compareFn(acc, x) === 1 ? acc : x, xs);
}

export function maxBy<T, U>(f: (x: T) => U, xs: Iterable<T>, comparer?: IComparer<U>) {
  const compareFn = comparer != null ? comparer.Compare : compare;
  return reduce((acc: T, x: T) => compareFn(f(acc), f(x)) === 1 ? acc : x, xs);
}

export function min<T>(xs: Iterable<T>, comparer?: IComparer<T>) {
  const compareFn = comparer != null ? comparer.Compare : compare;
  return reduce((acc: T, x: T) => compareFn(acc, x) === -1 ? acc : x, xs);
}

export function minBy<T, U>(f: (x: T) => U, xs: Iterable<T>, comparer?: IComparer<U>) {
  const compareFn = comparer != null ? comparer.Compare : compare;
  return reduce((acc: T, x: T) => compareFn(f(acc), f(x)) === -1 ? acc : x, xs);
}

export function pairwise<T>(xs: Iterable<T>): IterableIterator<[T, T]> {
  return delay(() => {
    const iter = xs[Symbol.iterator]();
    const cur = iter.next();
    if (cur.done) {
      return empty<[T, T]>();
    }
    const hd = cur.value;
    const tl = tail(xs);
    const ys = scan<T, [T, T]>(([_, last], next) => [last, next], [hd, hd], tl);
    return skip(1, ys);
  });
}

// export function rangeChar(first: string, last: string) {
//   const firstNum = first.charCodeAt(0);
//   const lastNum = last.charCodeAt(0);
//   return delay(() => unfold((x) => x <= lastNum ? [String.fromCharCode(x), x + 1] : undefined, firstNum));
// }

// export function rangeLong(first: Long, step: Long, last: Long, unsigned: boolean): IterableIterator<Long> {
//   const stepFn = makeLongRangeStepFunction(step, last, unsigned) as (arg: Long) => Option<[Long, Long]>;
//   return delay(() => unfold(stepFn, first));
// }

// export function rangeDecimal(first: Decimal, step: Decimal, last: Decimal): IterableIterator<Decimal> {
//   const stepFn = makeDecimalRangeStepFunction(step, last) as (arg: Decimal) => Option<[Decimal, Decimal]>;
//   return delay(() => unfold(stepFn, first));
// }

// export function rangeNumber(first: number, step: number, last: number) {
//   if (step === 0) {
//     throw new Error("Step cannot be 0");
//   }
//   return delay(() => unfold((x) => step > 0 && x <= last || step < 0 && x >= last ? [x, x + step] : undefined, first));
// }

export function readOnly<T>(xs: Iterable<T>) {
  return map((x) => x, xs);
}

export function reduce<T>(f: (acc: T, x: T) => T, xs: Iterable<T>) {
  if (isArrayOrBufferView(xs)) {
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
  const ar = isArrayOrBufferView(xs) ? xs as T[] : Array.from(xs);
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
  const ar = isArrayOrBufferView(xs) ? (xs as T[]).slice(0) : Array.from(xs);
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
      return undefined;
    }, undefined as ST | undefined);
  });
}

export function scanBack<T, ST>(f: (x: T, st: ST) => ST, xs: Iterable<T>, seed: ST) {
  return reverse(scan((acc, x) => f(x, acc), seed, reverse(xs)));
}

export function skip<T>(n: number, xs: Iterable<T>): IterableIterator<T> {
  return makeSeq(() => {
    const iter = xs[Symbol.iterator]();
    for (let i = 1; i <= n; i++) {
      if (iter.next().done) {
        throw new Error("Seq has not enough elements");
      }
    }
    return iter;
  });
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

export function sum<T>(xs: Iterable<T>, adder: IGenericAdder<T>): T {
  return fold((acc, x) => adder.Add(acc, x), adder.GetZero(), xs);
}

export function sumBy<T, T2>(f: (x: T) => T2, xs: Iterable<T>, adder: IGenericAdder<T2>): T2 {
  return fold((acc, x) => adder.Add(acc, f(x)), adder.GetZero(), xs);
}

export function tail<T>(xs: Iterable<T>) {
  return skip(1, xs);
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
      return undefined;
    }, 0);
  });
}

export function truncate<T>(n: number, xs: Iterable<T>) {
  return take(n, xs, true);
}

export function takeWhile<T>(f: (x: T) => boolean, xs: Iterable<T>) {
  return delay(() => {
    const iter = xs[Symbol.iterator]();
    return unfold(() => {
      const cur = iter.next();
      if (!cur.done && f(cur.value)) {
        return [cur.value, undefined];
      }
      return undefined;
    }, 0);
  });
}

export function tryFind<T>(f: (x: T, i?: number) => boolean, xs: Iterable<T>, defaultValue?: T): Option<T> {
  for (let i = 0, iter = xs[Symbol.iterator](); ; i++) {
    const cur = iter.next();
    if (cur.done) {
      break;
    }
    if (f(cur.value, i)) {
      return some(cur.value);
    }
  }
  return defaultValue === void 0 ? undefined : some(defaultValue);
}

export function find<T>(f: (x: T, i?: number) => boolean, xs: Iterable<T>) {
  return __failIfNone(tryFind(f, xs));
}

export function tryFindBack<T>(f: (x: T, i?: number) => boolean, xs: Iterable<T>, defaultValue?: T): Option<T> {
  const arr = isArrayOrBufferView(xs) ? (xs as T[]).slice(0) : Array.from(xs);
  return tryFind(f, arr.reverse(), defaultValue);
}

export function findBack<T>(f: (x: T, i?: number) => boolean, xs: Iterable<T>) {
  return __failIfNone(tryFindBack(f, xs));
}

export function tryFindIndex<T>(f: (x: T, i?: number) => boolean, xs: Iterable<T>): Option<number> {
  for (let i = 0, iter = xs[Symbol.iterator](); ; i++) {
    const cur = iter.next();
    if (cur.done) {
      break;
    }
    if (f(cur.value, i)) {
      return i;
    }
  }
  return undefined;
}

export function findIndex<T>(f: (x: T, i?: number) => boolean, xs: Iterable<T>) {
  return __failIfNone(tryFindIndex(f, xs));
}

export function tryFindIndexBack<T>(f: (x: T, i?: number) => boolean, xs: Iterable<T>): Option<number> {
  const arr = isArrayOrBufferView(xs) ? (xs as T[]).slice(0) : Array.from(xs);
  for (let i = arr.length - 1; i >= 0; i--) {
    if (f(arr[i], i)) {
      return i;
    }
  }
  return undefined;
}

export function findIndexBack<T>(f: (x: T, i?: number) => boolean, xs: Iterable<T>) {
  return __failIfNone(tryFindIndexBack(f, xs));
}

export function tryPick<T, U>(f: (x: T, i?: number) => Option<U>, xs: Iterable<T>): Option<U> {
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
  return undefined;
}

export function pick<T, U>(f: (x: T, i?: number) => Option<U>, xs: Iterable<T>) {
  return __failIfNone(tryPick(f, xs));
}

export function unfold<T, ST>(f: (st: ST) => Option<[T, ST]>, fst: ST): IterableIterator<T> {
  return makeSeq(() => {
    // Capture a copy of the first value in the closure
    // so the sequence is restarted every time, see #1230
    let acc = fst;
    const iter: Iterator<T> = {
      next(): IteratorResult<T> {
        const res = f(acc);
        if (res != null) {
          const v = value(res);
          if (v != null) {
            acc = v[1];
            return { done: false, value: v[0] };
          }
        }
        return { done: true, value: undefined };
      },
    };
    return iter;
  });
}

export function zip<T1, T2>(xs: Iterable<T1>, ys: Iterable<T2>): IterableIterator<[T1, T2]> {
  return map2((x, y) => [x, y], xs, ys);
}

export function zip3<T1, T2, T3>(xs: Iterable<T1>, ys: Iterable<T2>, zs: Iterable<T3>): IterableIterator<[T1, T2, T3]> {
  return map3((x, y, z) => [x, y, z], xs, ys, zs);
}

export function windowed<T>(windowSize: number, source: Iterable<T>): IterableIterator<T[]> {
  if (windowSize <= 0) {
    throw new Error("windowSize must be positive");
  }
  return makeSeq(() => {
    let window: T[] = [];
    const iter = source[Symbol.iterator]();
    const iter2: Iterator<T[]> = {
      next(): IteratorResult<T[]> {
        let cur: IteratorResult<T>;
        while (window.length < windowSize) {
          if ((cur = iter.next()).done) {
            return { done: true, value: undefined };
          }
          window.push(cur.value);
        }
        const value = window;
        window = window.slice(1);
        return { done: false, value };
      },
    };
    return iter2;
  });
}

export function transpose<T>(source: Iterable<Iterable<T>>): IterableIterator<Iterable<T>> {
  return makeSeq(() => {
    const iters = Array.from(source, (x) => x[Symbol.iterator]());
    const iter: Iterator<Iterable<T>> = {
      next(): IteratorResult<Iterable<T>> {
        if (iters.length === 0) {
          return { done: true, value: undefined }; // empty sequence
        }
        const results = Array.from(iters, (iter) => iter.next());
        if (results[0].done) {
          if (!results.every((x) => x.done)) {
            throw new Error("Sequences have different lengths");
          }
          return { done: true, value: undefined };
        } else {
          if (!results.every((x) => !x.done)) {
            throw new Error("Sequences have different lengths");
          }
          const values: T[] = results.map((x) => x.value);
          return { done: false, value: values };
        }
      },
    };
    return iter;
  });
}
