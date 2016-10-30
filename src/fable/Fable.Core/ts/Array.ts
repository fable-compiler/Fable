import { iterate } from "./Seq"

export function addRangeInPlace<T>(range: Iterable<T>, xs: Array<T>) {
  iterate(x => xs.push(x), range);
}

export function copyTo<T>(source: ArrayLike<T>, sourceIndex: number, target: ArrayLike<T>, targetIndex: number, count: number) {
  while (count--)
    (target as any)[targetIndex++] = source[sourceIndex++];
}

export function partition<T>(f: (x: T) => boolean, xs: ArrayLike<T>) {
  let ys = <T[]>[], zs = <T[]>[], j = 0, k = 0;
  for (let i = 0; i < xs.length; i++)
    if (f(xs[i]))
      ys[j++] = xs[i];
    else
      zs[k++] = xs[i];
  return [ys, zs];
}

export function permute<T>(f: (i: number) => number, xs: Array<T>) {
  // Keep the type of the array
  let ys = xs.map(() => <T>null);
  let checkFlags = new Array(xs.length);
  for (let i = 0; i < xs.length; i++) {
    const j = f(i);
    if (j < 0 || j >= xs.length)
      throw "Not a valid permutation";
    ys[j] = xs[i];
    checkFlags[j] = 1;
  }
  for (let i = 0; i < xs.length; i++)
    if (checkFlags[i] != 1)
      throw "Not a valid permutation";
  return ys;
}

export function removeInPlace<T>(item: T, xs: Array<T>) {
  const i = xs.indexOf(item);
  if (i > -1) {
    xs.splice(i, 1);
    return true;
  }
  return false;
}

export function setSlice<T>(target: any, lower: number, upper: number, source: ArrayLike<T>) {
  const length = (upper || target.length - 1) - lower;
  if (ArrayBuffer.isView(target) && source.length <= length)
    (target as any).set(source, lower);
  else
    for (let i = lower | 0, j = 0; j <= length; i++ , j++)
      target[i] = source[j];
}

export function sortInPlaceBy<T>(f: (x: T) => T, xs: Array<T>, dir: number = 1) {
  return xs.sort((x, y) => {
    x = f(x);
    y = f(y);
    return (x < y ? -1 : x == y ? 0 : 1) * dir;
  });
}

export function unzip<T1, T2>(xs: ArrayLike<[T1, T2]>) {
  const bs = new Array<T1>(xs.length), cs = new Array<T2>(xs.length);
  for (let i = 0; i < xs.length; i++) {
    bs[i] = xs[i][0];
    cs[i] = xs[i][1];
  }
  return [bs, cs];
}

export function unzip3<T1, T2, T3>(xs: ArrayLike<[T1, T2, T3]>) {
  const bs = new Array<T1>(xs.length), cs = new Array<T2>(xs.length), ds = new Array<T3>(xs.length);
  for (let i = 0; i < xs.length; i++) {
    bs[i] = xs[i][0];
    cs[i] = xs[i][1];
    ds[i] = xs[i][2];
  }
  return [bs, cs, ds];
}
