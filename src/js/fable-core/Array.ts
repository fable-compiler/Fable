export function map<T, U>(f: (x: T) => U, source: T[], target: U[]) {
  for (let i = 0; i < source.length; i++) {
    target[i] = f(source[i]);
  }
  return target;
}

export function mapIndexed<T, U>(f: (i: number, x: T) => U, source: T[], target: U[]) {
  for (let i = 0; i < source.length; i++) {
    target[i] = f(i, source[i]);
  }
  return target;
}

export function indexed<T>(source: T[], target: Array<[number, T]>) {
  return mapIndexed((i, x) => [i, x] as [number, T], source, target);
}

export function addRangeInPlace<T>(range: Iterable<T>, xs: T[]) {
  const iter = range[Symbol.iterator]();
  let cur = iter.next();
  while (!cur.done) {
    xs.push(cur.value);
    cur = iter.next();
  }
}

export function copyTo<T>(
  source: ArrayLike<T>, sourceIndex: number, target: ArrayLike<T>,
  targetIndex: number, count: number) {
  while (count--) {
    (target as any)[targetIndex++] = source[sourceIndex++];
  }
}

export function partition<T>(f: (x: T) => boolean, xs: ArrayLike<T>) {
  const ys = [] as T[];
  const zs = [] as T[];
  let j = 0;
  let k = 0;
  for (let i = 0; i < xs.length; i++) {
    if (f(xs[i])) {
      ys[j++] = xs[i];
    } else {
      zs[k++] = xs[i];
    }
  }
  return [ys, zs];
}

export function permute<T>(f: (i: number) => number, xs: T[]) {
  // Keep the type of the array
  const ys = xs.map(() => null as T);
  const checkFlags = new Array(xs.length);
  for (let i = 0; i < xs.length; i++) {
    const j = f(i);
    if (j < 0 || j >= xs.length) {
      throw new Error("Not a valid permutation");
    }
    ys[j] = xs[i];
    checkFlags[j] = 1;
  }
  for (let i = 0; i < xs.length; i++) {
    if (checkFlags[i] !== 1) {
      throw new Error("Not a valid permutation");
    }
  }
  return ys;
}

export function removeInPlace<T>(item: T, xs: T[]) {
  const i = xs.indexOf(item);
  if (i > -1) {
    xs.splice(i, 1);
    return true;
  }
  return false;
}

export function setSlice<T>(target: any, lower: number, upper: number, source: ArrayLike<T>) {
  const length = (upper || target.length - 1) - lower;
  if (ArrayBuffer.isView(target) && source.length <= length) {
    (target as any).set(source, lower);
  } else {
    for (let i = lower | 0, j = 0; j <= length; i++ , j++) {
      target[i] = source[j];
    }
  }
}

export function sortInPlaceBy<T>(f: (x: T) => T, xs: T[], dir: number = 1) {
  return xs.sort((x, y) => {
    x = f(x);
    y = f(y);
    return (x < y ? -1 : x === y ? 0 : 1) * dir;
  });
}

export function unzip<T1, T2>(xs: ArrayLike<[T1, T2]>) {
  const bs = new Array<T1>(xs.length);
  const cs = new Array<T2>(xs.length);
  for (let i = 0; i < xs.length; i++) {
    bs[i] = xs[i][0];
    cs[i] = xs[i][1];
  }
  return [bs, cs];
}

export function unzip3<T1, T2, T3>(xs: ArrayLike<[T1, T2, T3]>) {
  const bs = new Array<T1>(xs.length);
  const cs = new Array<T2>(xs.length);
  const ds = new Array<T3>(xs.length);
  for (let i = 0; i < xs.length; i++) {
    bs[i] = xs[i][0];
    cs[i] = xs[i][1];
    ds[i] = xs[i][2];
  }
  return [bs, cs, ds];
}

export function chunkBySize<T>(size: number, xs: T[]): T[][] {
  if (size < 1) {
    throw new Error("The input must be positive. parameter name: chunkSize");
  }

  if (xs.length === 0) {
    return [[]];
  }

  const result = [];
  // add each chunk to the result
  for (let x = 0; x < Math.ceil(xs.length / size); x++) {

    const start = x * size;
    const end = start + size;

    result.push(xs.slice(start, end));
  }

  return result;
}

export function getSubArray<T>(xs: T[], startIndex: number, count: number) {
  return xs.slice(startIndex, startIndex + count);
}

export function fill<T>(target: T[], targetIndex: number, count: number, value: T) {
  target.fill(value, targetIndex, targetIndex + count);
}
