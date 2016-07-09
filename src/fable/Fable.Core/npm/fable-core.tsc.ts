// Types needed for unzip/unzip3 -- http://stackoverflow.com/a/32191614
export type TTuple<T1, T2> = [T1, T2];
export type TTuple3<T1, T2, T3> = [T1, T2, T3];


export class Seq<T> {
  static fold<T, ST>(f: (previousValue: ST, currentValue: T, currentIndex?: number) => ST, acc: ST, xs: Iterable<T>): ST {
    if (Array.isArray(xs) || ArrayBuffer.isView(xs)) {
      return (xs as Array<T>).reduce(f, acc);
    } else {
      let cur: IteratorResult<T> = null;
      for (let i = 0, iter = xs[Symbol.iterator](); ; i++) {
        cur = iter.next();
        if (cur.done) {
          break;
        }
        acc = f(acc, cur.value, i);
      }
      return acc;
    }
  }

  static foldBack<T, ST>(f: (currentValue: T, previousValue: ST, currentIndex?: number) => ST, xs: Iterable<T>, acc: ST): ST {
    const arr = Array.isArray(xs) || ArrayBuffer.isView(xs) ? xs as Array<T> : Array.from(xs);
    for (let i = arr.length - 1; i >= 0; i--) {
      acc = f(arr[i], acc, i);
    }
    return acc;
  }
}

export class List<T> {
  public head: T;
  public tail: List<T>;

  public [Symbol.iterator]() {
    let cur: List<T> = this;
    return {
      next: () => {
        const tmp = cur;
        cur = cur.tail;
        return { done: tmp.tail == null, value: tmp.head }
      }
    }
  }

  get length(): number {
    return Seq.fold((acc, x) => acc + 1, 0, this);
  }

  constructor(head?: T, tail?: List<T>) {
    this.head = head;
    this.tail = tail;
  }

  static ofArray<T>(args: Array<T>, base?: List<T>): List<T> {
    let acc = base || new List<T>();
    for (let i = args.length - 1; i >= 0; i--) {
      acc = new List<T>(args[i], acc);
    }
    return acc;
  }

  append(ys: List<T>): List<T> {
    return List.append(this, ys);
  }

  static append<T>(xs: List<T>, ys: List<T>): List<T> {
    return Seq.fold((acc, x) => new List<T>(x, acc), ys, List.rev(xs));
  }

  choose<U>(f: (x: T) => U, xs: List<T>): List<U> {
    return List.choose(f, this);
  }

  static choose<T, U>(f: (x: T) => U, xs: List<T>): List<U> {
    const r = Seq.fold((acc, x) => {
      const y = f(x);
      return y != null ? new List<U>(y, acc) : acc;
    }, new List<U>(), xs);

    return List.rev(r);
  }

  collect<U>(f: (x: T) => List<U>): List<U> {
    return List.collect(f, this);
  }

  static collect<T, U>(f: (x: T) => List<U>, xs: List<T>): List<U> {
    return Seq.fold((acc, x) => f(x).append(acc), new List<U>(), List.rev(xs));
  }

  concat(xs: List<T>): List<T> {
    return List.concat(this);
  }

  static concat<T>(xs: List<T>): List<T> {
    return List.collect((x) => List.singleton(x), xs);
  }


  filter(f: (x: T) => boolean): List<T> {
    return List.filter(f, this);
  }

  static filter<T>(f: (x: T) => boolean, xs: List<T>): List<T> {
    return List.rev(Seq.fold((acc, x) => f(x) ? new List<T>(x, acc) : acc, new List<T>(), xs));
  }

  where(f: (x: T) => boolean): List<T> {
    return List.filter(f, this);
  }

  static where<T>(f: (x: T) => boolean, xs: List<T>): List<T> {
    return List.filter(f, xs);
  }

  static init<T>(n: number, f: (i: number) => T): List<T> {
    if (n < 0) {
      throw "List length must be non-negative";
    }
    let xs = new List<T>();
    for (let i = 1; i <= n; i++) {
      xs = new List<T>(f(n - i), xs);
    }
    return xs;
  }

  map<U>(f: (x: T) => U): List<U> {
    return List.map(f, this);
  }

  static map<T, U>(f: (x: T) => U, xs: List<T>): List<U> {
    return List.rev(Seq.fold((acc: List<U>, x: T) => new List<U>(f(x), acc), new List<U>(), xs));
  }

  mapi<U>(f: (i: number, x: T) => U): List<U> {
    return List.mapi(f, this);
  }

  static mapi<T, U>(f: (i: number, x: T) => U, xs: List<T>): List<U> {
    return List.rev(Seq.fold((acc, x, i) => new List<U>(f(i, x), acc), new List<U>(), xs));
  }

  partition(f: (x: T) => boolean): [List<T>, List<T>] {
    return List.partition(f, this);
  }

  static partition<T>(f: (x: T) => boolean, xs: List<T>): [List<T>, List<T>] {
    const ini: TTuple<List<T>, List<T>> = [new List<T>(), new List<T>()];
    return Seq.fold((acc: TTuple<List<T>, List<T>>, x: T) => {
      const lacc = acc[0], racc = acc[1];
      const l: TTuple<List<T>, List<T>> = [new List<T>(x, lacc), racc];
      const r: TTuple<List<T>, List<T>> = [lacc, new List<T>(x, racc)];
      return f(x) ? l : r;
    }, ini, List.rev(xs));
  }

  static replicate<T>(n: number, x: T): List<T> {
    return List.init(n, () => x);
  }

  rev(): List<T> {
    return List.rev(this);
  }

  static rev<T>(xs: List<T>): List<T> {
    return Seq.fold((acc, x) => new List<T>(x, acc), new List<T>(), xs);
  }

  static singleton<T>(x: T): List<T> {
    return new List<T>(x, new List<T>());
  }

  slice(lower: number, upper: number): List<T> {
    return List.slice(lower, upper, this);
  }

  static slice<T>(lower: number, upper: number, xs: List<T>): List<T> {
    const noLower = (lower == null);
    const noUpper = (upper == null);
    return List.rev(Seq.fold((acc, x, i) => (noLower || lower <= i) && (noUpper || i <= upper) ? new List<T>(x, acc) : acc, new List<T>(), xs));
  }

  /* ToDo: ?
  unzip<T1, T2, T extends TTuple<T1, T2>>(): [List<T1>, List<T2>] {
    return List.unzip(this);
  } */

  static unzip<T1, T2>(xs: List<TTuple<T1, T2>>): [List<T1>, List<T2>] {
    const ini: TTuple<List<T1>, List<T2>> = [new List<T1>(), new List<T2>()];
    return Seq.foldBack((xy, acc): TTuple<List<T1>, List<T2>> => [new List<T1>(xy[0], acc[0]), new List<T2>(xy[1], acc[1])], xs, ini);
  }

  /* ToDo: unzip3() */

  static unzip3<T1, T2, T3>(xs: List<TTuple3<T1, T2, T3>>): [List<T1>, List<T2>, List<T3>] {
    const ini: TTuple3<List<T1>, List<T2>, List<T3>> = [new List<T1>(), new List<T2>(), new List<T3>()];
    return Seq.foldBack((xyz, acc): TTuple3<List<T1>, List<T2>, List<T3>> => [new List<T1>(xyz[0], acc[0]), new List<T2>(xyz[1], acc[1]), new List<T3>(xyz[2], acc[2])], xs, ini);
  }
}
