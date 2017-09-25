import List from "./ListClass";
import { ofArray } from "./ListClass";
import { groupBy as mapGroupBy } from "./Map";
import { map as seqMap } from "./Seq";
import { fold as seqFold } from "./Seq";
import { foldBack as seqFoldBack } from "./Seq";
import { toList as seqToList } from "./Seq";

export default List;
export { ofArray };

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
  return collect((x) => x, xs);
}

export function filter<T>(f: (x: T) => boolean, xs: List<T>) {
  return reverse(seqFold((acc, x) => f(x) ? new List<T>(x, acc) : acc, new List<T>(), xs));
}

export function where<T>(f: (x: T) => boolean, xs: List<T>) {
  return filter(f, xs);
}

export function initialize<T>(n: number, f: (i: number) => T) {
  if (n < 0) {
    throw new Error("List length must be non-negative");
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

export function indexed<T>(xs: List<T>) {
  return mapIndexed((i, x) => [i, x] as [number, T], xs);
}

export function partition<T>(f: (x: T) => boolean, xs: List<T>) {
  return seqFold((acc, x) => {
    const lacc = acc[0];
    const racc = acc[1];
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
  return reverse(seqFold((acc, x, i) =>
    (noLower || lower <= i) && (noUpper || i <= upper) ? new List<T>(x, acc) : acc, new List<T>(), xs));
}
/* ToDo: instance unzip() */

export function unzip<T1, T2>(xs: List<[T1, T2]>) {
  return seqFoldBack((xy, acc) =>
    [new List<T1>(xy[0], acc[0]), new List<T2>(xy[1], acc[1])] as [List<T1>, List<T2>],
    xs,
    [new List<T1>(), new List<T2>()] as [List<T1>, List<T2>]);
}
/* ToDo: instance unzip3() */

export function unzip3<T1, T2, T3>(xs: List<[T1, T2, T3]>) {
  return seqFoldBack((xyz, acc) =>
    [new List<T1>(xyz[0], acc[0]), new List<T2>(xyz[1], acc[1]), new List<T3>(xyz[2], acc[2]),
    ] as [List<T1>, List<T2>, List<T3>],
    xs,
    [new List<T1>(), new List<T2>(), new List<T3>(),
    ] as [List<T1>, List<T2>, List<T3>]);
}

export function groupBy<T, K>(f: (x: T) => K, xs: List<T>): List<[K, List<T>]> {
  return seqToList(seqMap((k) => [k[0], seqToList(k[1])], mapGroupBy(f, xs))) as List<[K, List<T>]>;
}

export function splitAt<T>(index: number, xs: List<T>): [List<T>, List<T>] {
  if (index < 0) {
    throw new Error("The input must be non-negative.");
  }
  let i = 0;
  let last = xs;
  const first = new Array(index);
  while (i < index) {
    if (last.tail == null) {
      throw new Error("The input sequence has an insufficient number of elements.");
    }
    first[i] = last.head;
    last = last.tail;
    i++;
  }
  return [ofArray(first), last];
}
