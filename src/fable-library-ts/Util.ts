// Don't change, this corresponds to DateTime.Kind enum values in .NET
export const enum DateKind {
  Unspecified = 0,
  UTC = 1,
  Local = 2,
}

export interface IDateTime extends Date {
  kind?: DateKind;
}

export interface IDateTimeOffset extends Date {
  offset?: number;
}

export interface IComparable<T> extends IEquatable<T> {
  CompareTo(x: T): number;
}

export interface IEquatable<T> {
  Equals(x: T): boolean;
}

export interface IHashable {
  GetHashCode(): number;
}

export interface IDisposable {
  Dispose(): void;
}

export interface IComparer<T> {
  Compare(x: T, y: T): number;
}

export interface IEqualityComparer<T> {
  Equals(x: T, y: T): boolean;
  GetHashCode(x: T): number;
}

export interface ICollection<T> extends Iterable<T> {
  readonly Count: number;
  readonly IsReadOnly: boolean;
  Add(item: T): void;
  Clear(): void;
  Contains(item: T): boolean;
  CopyTo(array: T[], arrayIndex: number): void;
  Remove(item: T): boolean;
}

export function isArrayLike<T>(x: T | ArrayLike<T> | Iterable<T>): x is T[] {
  return Array.isArray(x) || ArrayBuffer.isView(x);
}

export function isIterable<T>(x: T | ArrayLike<T> | Iterable<T>): x is Iterable<T> {
  return x != null && typeof x === "object" && Symbol.iterator in x;
}

export function isEnumerable<T>(x: T | Iterable<T>): x is IEnumerable<T> {
  return x != null && typeof (x as IEnumerable<T>).GetEnumerator === "function"
}

export function isComparer<T>(x: T | IComparer<T>): x is IComparer<T> {
  return x != null && typeof (x as IComparer<T>).Compare === "function";
}

export function isComparable<T>(x: T | IComparable<T>): x is IComparable<T> {
  return x != null && typeof (x as IComparable<T>).CompareTo === "function";
}

export function isEquatable<T>(x: T | IEquatable<T>): x is IEquatable<T> {
  return x != null && typeof (x as IEquatable<T>).Equals === "function";
}

export function isHashable<T>(x: T | IHashable): x is IHashable {
  return x != null && typeof (x as IHashable).GetHashCode === "function";
}

export function isDisposable<T>(x: T | IDisposable): x is IDisposable {
  return x != null && typeof (x as IDisposable).Dispose === "function";
}

export function disposeSafe(x: any) {
  if (isDisposable(x)) {
    x.Dispose();
  }
}

export function defaultOf<T>(): T {
  return null as T;
}

export function sameConstructor<T>(x: T, y: T) {
  return Object.getPrototypeOf(x)?.constructor === Object.getPrototypeOf(y)?.constructor;
}

export interface IEnumerator<T> extends IDisposable {
  ["System.Collections.Generic.IEnumerator`1.get_Current"](): T;
  ["System.Collections.IEnumerator.get_Current"](): T;
  ["System.Collections.IEnumerator.MoveNext"](): boolean;
  ["System.Collections.IEnumerator.Reset"](): void;
  Dispose(): void;
}

export interface IEnumerable<T> extends Iterable<T> {
  GetEnumerator(): IEnumerator<T>;
  "System.Collections.IEnumerable.GetEnumerator"(): IEnumerator<any>;
}

export class Enumerable<T> implements IEnumerable<T> {
  constructor(private en: IEnumerator<T>) {}
  public GetEnumerator(): IEnumerator<T> { return this.en; }
  public "System.Collections.IEnumerable.GetEnumerator"(): IEnumerator<any> { return this.en; }
  [Symbol.iterator]() {
    return this;
  }
  next() {
    const hasNext = this.en["System.Collections.IEnumerator.MoveNext"]();
    const current = hasNext ? this.en["System.Collections.Generic.IEnumerator`1.get_Current"]() : undefined;
    return { done: !hasNext, value: current } as IteratorResult<T>;
  }
}

export class Enumerator<T> implements IEnumerator<T> {
  private current: T = defaultOf();
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

export function toEnumerable<T>(e: IEnumerable<T> | Iterable<T>): IEnumerable<T> {
  if (isEnumerable(e)) { return e; }
  else { return new Enumerable(new Enumerator(e[Symbol.iterator]())); }
}

export function getEnumerator<T>(e: IEnumerable<T> | Iterable<T>): IEnumerator<T> {
  if (isEnumerable(e)) { return e.GetEnumerator(); }
  else { return new Enumerator(e[Symbol.iterator]()); }
}

export function toIterator<T>(en: IEnumerator<T>): Iterator<T> {
  return {
    next() {
      const hasNext = en["System.Collections.IEnumerator.MoveNext"]();
      const current = hasNext ? en["System.Collections.Generic.IEnumerator`1.get_Current"]() : undefined;
      return { done: !hasNext, value: current } as IteratorResult<T>;
    },
  };
}

export function enumerableToIterator<T>(e: IEnumerable<T> | Iterable<T>): Iterator<T> {
  return toIterator(toEnumerable(e).GetEnumerator());
}

export interface ISet<T> {
  add(value: T): ISet<T>;
  clear(): void;
  delete(value: T): boolean;
  forEach(callbackfn: (value: T, value2: T, set: ISet<T>) => void, thisArg?: any): void;
  has(value: T): boolean;
  readonly size: number;

  [Symbol.iterator](): Iterator<T>;
  entries(): Iterable<[T, T]>;
  keys(): Iterable<T>;
  values(): Iterable<T>;
}

export interface IMapOrWeakMap<K, V> {
  delete(key: K): boolean;
  get(key: K): V | undefined;
  has(key: K): boolean;
  set(key: K, value: V): IMapOrWeakMap<K, V>;
}

export interface IMap<K, V> extends IMapOrWeakMap<K, V> {
  clear(): void;
  set(key: K, value: V): IMap<K, V>;

  forEach(callbackfn: (value: V, key: K, map: IMap<K, V>) => void, thisArg?: any): void;
  readonly size: number;

  [Symbol.iterator](): Iterator<[K, V]>;
  entries(): Iterable<[K, V]>;
  keys(): Iterable<K>;
  values(): Iterable<V>;
}

export class Comparer<T> implements IComparer<T> {
  public Compare: (x: T, y: T) => number;

  constructor(f?: (x: T, y: T) => number) {
    this.Compare = f || compare;
  }
}

export function comparerFromEqualityComparer<T>(comparer: IEqualityComparer<T>): IComparer<T> {
  // Sometimes IEqualityComparer also implements IComparer
  if (isComparer(comparer)) {
    return new Comparer<T>((comparer as any as IComparer<T>).Compare);
  } else {
    return new Comparer<T>((x: T, y: T) => {
      const xhash = comparer.GetHashCode(x);
      const yhash = comparer.GetHashCode(y);
      if (xhash === yhash) {
        return comparer.Equals(x, y) ? 0 : -1;
      } else {
        return xhash < yhash ? -1 : 1;
      }
    });
  }
}

export function assertEqual<T>(actual: T, expected: T, msg?: string): void {
  if (!equals(actual, expected)) {
    throw Object.assign(new Error(msg || `Expected: ${expected} - Actual: ${actual}`), {
      actual,
      expected,
    });
  }
}

export function assertNotEqual<T>(actual: T, expected: T, msg?: string): void {
  if (equals(actual, expected)) {
    throw Object.assign(new Error(msg || `Expected: ${expected} - Actual: ${actual}`), {
      actual,
      expected,
    });
  }
}

export class Lazy<T> {
  private factory: () => T;
  private isValueCreated: boolean;

  private createdValue?: T;

  constructor(factory: () => T) {
    this.factory = factory;
    this.isValueCreated = false;
  }

  get Value() {
    if (!this.isValueCreated) {
      this.createdValue = this.factory();
      this.isValueCreated = true;
    }
    return this.createdValue;
  }

  get IsValueCreated() {
    return this.isValueCreated;
  }
}

export function lazyFromValue<T>(v: T) {
  return new Lazy(() => v);
}

export function padWithZeros(i: number, length: number) {
  return i.toString(10).padStart(length, "0");
}

export function padLeftAndRightWithZeros(i: number, lengthLeft: number, lengthRight: number) {
  return i.toString(10).padStart(lengthLeft, "0").padEnd(lengthRight, "0");
}

export function dateOffset(date: IDateTime | IDateTimeOffset): number {
  const date1 = date as IDateTimeOffset;
  return typeof date1.offset === "number"
    ? date1.offset
    : ((date as IDateTime).kind === DateKind.UTC
      ? 0 : date.getTimezoneOffset() * -60000);
}

export function int16ToString(i: number, radix?: number) {
  i = i < 0 && radix != null && radix !== 10 ? 0xFFFF + i + 1 : i;
  return i.toString(radix);
}

export function int32ToString(i: number, radix?: number) {
  i = i < 0 && radix != null && radix !== 10 ? 0xFFFFFFFF + i + 1 : i;
  return i.toString(radix);
}

export function int64ToString(i: bigint, radix?: number) {
  i = i < 0 && radix != null && radix !== 10 ? 0xFFFFFFFFFFFFFFFFn + i + 1n : i;
  return i.toString(radix);
}

export abstract class ObjectRef {
  public static id(o: any) {
    if (!ObjectRef.idMap.has(o)) {
      ObjectRef.idMap.set(o, ++ObjectRef.count);
    }
    return ObjectRef.idMap.get(o);
  }
  private static idMap = new WeakMap();
  private static count = 0;
}

export function stringHash(s: string) {
  let i = 0;
  let h = 5381;
  const len = s.length;
  while (i < len) {
    h = (h * 33) ^ s.charCodeAt(i++);
  }
  return h;
}

export function numberHash(x: number) {
  return x * 2654435761 | 0;
}

export function bigintHash(x: bigint) {
  return stringHash(x.toString(32));
}

// From https://stackoverflow.com/a/37449594
export function combineHashCodes(hashes: ArrayLike<number>) {
  let h1 = 0;
  const len = hashes.length;
  for (let i = 0; i < len; i++) {
    const h2 = hashes[i];
    h1 = ((h1 << 5) + h1) ^ h2;
  }
  return h1;
}

export function physicalHash<T>(x: T): number {
  if (x == null) {
    return 0;
  }
  switch (typeof x) {
    case "boolean":
      return x ? 1 : 0;
    case "number":
      return numberHash(x);
    case "bigint":
      return bigintHash(x);
    case "string":
      return stringHash(x);
    default:
      return numberHash(ObjectRef.id(x));
  }
}

export function identityHash<T>(x: T): number {
  if (isHashable(x)) {
    return x.GetHashCode();
  } else {
    return physicalHash(x);
  }
}

export function dateHash(x: Date): number {
  return x.getTime();
}

export function arrayHash<T>(x: ArrayLike<T>): number {
  const len = x.length;
  const hashes: number[] = new Array(len);
  for (let i = 0; i < len; i++) {
    hashes[i] = structuralHash(x[i]);
  }
  return combineHashCodes(hashes);
}

export function structuralHash<T>(x: T): number {
  if (x == null) {
    return 0;
  }
  switch (typeof x) {
    case "boolean":
      return x ? 1 : 0;
    case "number":
      return numberHash(x);
    case "bigint":
      return bigintHash(x);
    case "string":
      return stringHash(x);
    default: {
      if (isHashable(x)) {
        return x.GetHashCode();
      } else if (isArrayLike(x)) {
        return arrayHash(x);
      } else if (x instanceof Date) {
        return dateHash(x);
      } else if (Object.getPrototypeOf(x)?.constructor === Object) {
        // TODO: check call-stack to prevent cyclic objects?
        const hashes = Object.values(x).map((v) => structuralHash(v));
        return combineHashCodes(hashes);
      } else {
        // Classes don't implement GetHashCode by default, but must use identity hashing
        return numberHash(ObjectRef.id(x));
        // return stringHash(String(x));
      }
    }
  }
}

// Intended for custom numeric types, like long or decimal
export function fastStructuralHash<T>(x: T): number {
  return stringHash(String(x));
}

// Intended for declared types that may or may not implement GetHashCode
export function safeHash<T>(x: T): number {
  // return x == null ? 0 : isHashable(x) ? x.GetHashCode() : numberHash(ObjectRef.id(x));
  return identityHash(x);
}

export function equalArraysWith<T>(x: ArrayLike<T>, y: ArrayLike<T>, eq: (x: T, y: T) => boolean): boolean {
  if (x == null) { return y == null; }
  if (y == null) { return false; }
  if (x.length !== y.length) { return false; }
  for (let i = 0; i < x.length; i++) {
    if (!eq(x[i], y[i])) { return false; }
  }
  return true;
}

export function equalArrays<T>(x: ArrayLike<T>, y: ArrayLike<T>): boolean {
  return equalArraysWith(x, y, equals);
}

function equalObjects(x: { [k: string]: any }, y: { [k: string]: any }): boolean {
  const xKeys = Object.keys(x);
  const yKeys = Object.keys(y);
  if (xKeys.length !== yKeys.length) {
    return false;
  }
  xKeys.sort();
  yKeys.sort();
  for (let i = 0; i < xKeys.length; i++) {
    if (xKeys[i] !== yKeys[i] || !equals(x[xKeys[i]], y[yKeys[i]])) {
      return false;
    }
  }
  return true;
}

export function physicalEquality<T>(x: T, y: T): boolean {
  return x === y;
}

export function equals<T>(x: T, y: T): boolean {
  if (x === y) {
    return true;
  } else if (x == null) {
    return y == null;
  } else if (y == null) {
    return false;
  } else if (isEquatable(x)) {
    return x.Equals(y);
  } else if (isArrayLike(x)) {
    return isArrayLike(y) && equalArrays(x, y);
  } else if (typeof x !== "object") {
    return false;
  } else if (x instanceof Date) {
    return (y instanceof Date) && compareDates(x, y) === 0;
  } else {
    return Object.getPrototypeOf(x)?.constructor === Object && equalObjects(x, y);
  }
}

export function compareDates(x: Date | IDateTime | IDateTimeOffset, y: Date | IDateTime | IDateTimeOffset) {
  let xtime;
  let ytime;

  // DateTimeOffset and DateTime deals with equality differently.
  if ("offset" in x && "offset" in y) {
    xtime = x.getTime();
    ytime = y.getTime();
  } else {
    xtime = x.getTime() + dateOffset(x);
    ytime = y.getTime() + dateOffset(y);
  }

  return xtime === ytime ? 0 : (xtime < ytime ? -1 : 1);
}

export function comparePrimitives<T>(x: T, y: T): number {
  return x === y ? 0 : (x < y ? -1 : 1);
}

export function compareArraysWith<T>(x: ArrayLike<T>, y: ArrayLike<T>, comp: (x: T, y: T) => number): number {
  if (x == null) { return y == null ? 0 : 1; }
  if (y == null) { return -1; }
  if (x.length !== y.length) {
    return x.length < y.length ? -1 : 1;
  }
  for (let i = 0, j = 0; i < x.length; i++) {
    j = comp(x[i], y[i]);
    if (j !== 0) { return j; }
  }
  return 0;
}

export function compareArrays<T>(x: ArrayLike<T>, y: ArrayLike<T>): number {
  return compareArraysWith(x, y, compare);
}

function compareObjects(x: { [k: string]: any }, y: { [k: string]: any }): number {
  const xKeys = Object.keys(x);
  const yKeys = Object.keys(y);
  if (xKeys.length !== yKeys.length) {
    return xKeys.length < yKeys.length ? -1 : 1;
  }
  xKeys.sort();
  yKeys.sort();
  for (let i = 0, j = 0; i < xKeys.length; i++) {
    const key = xKeys[i];
    if (key !== yKeys[i]) {
      return key < yKeys[i] ? -1 : 1;
    } else {
      j = compare(x[key], y[key]);
      if (j !== 0) { return j; }
    }
  }
  return 0;
}

export function compare<T>(x: T, y: T): number {
  if (x === y) {
    return 0;
  } else if (x == null) {
    return y == null ? 0 : -1;
  } else if (y == null) {
    return 1;
  } else if (isComparable(x)) {
    return x.CompareTo(y);
  } else if (isArrayLike(x)) {
    return isArrayLike(y) ? compareArrays(x, y) : -1;
  } else if (typeof x !== "object") {
    return x < y ? -1 : 1;
  } else if (x instanceof Date) {
    return y instanceof Date ? compareDates(x, y) : -1;
  } else {
    return Object.getPrototypeOf(x)?.constructor === Object ? compareObjects(x, y) : -1;
  }
}

export function min<T>(comparer: (x: T, y: T) => number, x: T, y: T) {
  return comparer(x, y) < 0 ? x : y;
}

export function max<T>(comparer: (x: T, y: T) => number, x: T, y: T) {
  return comparer(x, y) > 0 ? x : y;
}

export function clamp<T>(comparer: (x: T, y: T) => number, value: T, min: T, max: T) {
  return (comparer(value, min) < 0) ? min : (comparer(value, max) > 0) ? max : value;
}

export function createAtom<T>(value: T): (<Args extends [] | [T]>(...args: Args) => Args extends [] ? T : void) {
  let atom = value;
  return (...args) => {
    if (args.length === 0) {
      return atom as any;
    } else {
      atom = args[0];
    }
  };
}

export function createObj(fields: Iterable<[string, any]>) {
  const obj: any = {};
  for (const kv of fields) {
    obj[kv[0]] = kv[1];
  }
  return obj;
}

export function jsOptions<T>(mutator: (x: T) => void): T {
  const opts = {} as T;
  mutator(opts);
  return opts;
}

export function round(value: number, digits: number = 0) {
  const m = Math.pow(10, digits);
  const n = +(digits ? value * m : value).toFixed(8);
  const i = Math.floor(n);
  const f = n - i;
  const e = 1e-8;
  const r = (f > 0.5 - e && f < 0.5 + e) ? ((i % 2 === 0) ? i : i + 1) : Math.round(n);
  return digits ? r / m : r;
}

export function sign(x: number): number {
  return x > 0 ? 1 : x < 0 ? -1 : 0;
}

export function unescapeDataString(s: string): string {
  // https://stackoverflow.com/a/4458580/524236
  return decodeURIComponent((s).replace(/\+/g, "%20"));
}
export function escapeDataString(s: string): string {
  return encodeURIComponent(s).replace(/!/g, "%21")
    .replace(/'/g, "%27")
    .replace(/\(/g, "%28")
    .replace(/\)/g, "%29")
    .replace(/\*/g, "%2A");
}
export function escapeUriString(s: string): string {
  return encodeURI(s);
}

// ICollection.Clear and Count members can be called on Arrays
// or Dictionaries so we need a runtime check (see #1120)
export function count<T>(col: Iterable<T>): number {
  if (isArrayLike(col)) {
    return col.length;
  } else {
    let count = 0;
    for (const _ of col) {
      count++;
    }
    return count;
  }
}

export function clear<T>(col: Iterable<T>) {
  if (isArrayLike(col)) {
    (col as any as T[]).splice(0);
  } else {
    (col as any).clear();
  }
}

const curried = new WeakMap<object, object>();

export function uncurry2<T1, T2, TResult>(f: (a1: T1) => (a2: T2) => TResult) {
  if (f == null) { return null as unknown as (a1: T1, a2: T2) => TResult }
  const f2 = (a1: T1, a2: T2) => f(a1)(a2);
  curried.set(f2, f);
  return f2;
}

export function curry2<T1, T2, TResult>(f: (a1: T1, a2: T2) => TResult) {
  return curried.get(f) as ((a1: T1) => (a2: T2) => TResult) ?? ((a1: T1) => (a2: T2) => f(a1, a2));
}

export function uncurry3<T1, T2, T3, TResult>(f: (a1: T1) => (a2: T2) => (a3: T3) => TResult) {
  if (f == null) { return null as unknown as (a1: T1, a2: T2, a3: T3) => TResult }
  const f2 = (a1: T1, a2: T2, a3: T3) => f(a1)(a2)(a3);
  curried.set(f2, f);
  return f2;
}

export function curry3<T1, T2, T3, TResult>(f: (a1: T1, a2: T2, a3: T3) => TResult) {
  return curried.get(f) as (a1: T1) => (a2: T2) => (a3: T3) => TResult
    ?? ((a1: T1) => (a2: T2) => (a3: T3) => f(a1, a2, a3));
}

export function uncurry4<T1, T2, T3, T4, TResult>(
  f: (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => TResult
) {
  if (f == null) { return null as unknown as (a1: T1, a2: T2, a3: T3, a4: T4) => TResult }
  const f2 = (a1: T1, a2: T2, a3: T3, a4: T4) => f(a1)(a2)(a3)(a4);
  curried.set(f2, f);
  return f2;
}

export function curry4<T1, T2, T3, T4, TResult>(f: (a1: T1, a2: T2, a3: T3, a4: T4) => TResult) {
  return curried.get(f) as (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => TResult
    ?? ((a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => f(a1, a2, a3, a4));
}

export function uncurry5<T1, T2, T3, T4, T5, TResult>(
  f: (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => TResult
) {
  if (f == null) { return null as unknown as (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5) => TResult }
  const f2 = (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5) => f(a1)(a2)(a3)(a4)(a5);
  curried.set(f2, f);
  return f2;
}

export function curry5<T1, T2, T3, T4, T5, TResult>(f: (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5) => TResult) {
  return curried.get(f) as (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => TResult
    ?? ((a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => f(a1, a2, a3, a4, a5));
}

export function uncurry6<T1, T2, T3, T4, T5, T6, TResult>(
  f: (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => TResult
) {
  if (f == null) { return null as unknown as (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6) => TResult }
  const f2 = (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6) => f(a1)(a2)(a3)(a4)(a5)(a6);
  curried.set(f2, f);
  return f2;
}

export function curry6<T1, T2, T3, T4, T5, T6, TResult>(f: (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6) => TResult) {
  return curried.get(f) as (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => TResult
    ?? ((a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => f(a1, a2, a3, a4, a5, a6));
}

export function uncurry7<T1, T2, T3, T4, T5, T6, T7, TResult>(
  f: (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => TResult
) {
  if (f == null) { return null as unknown as (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7) => TResult }
  const f2 = (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7) => f(a1)(a2)(a3)(a4)(a5)(a6)(a7);
  curried.set(f2, f);
  return f2;
}

export function curry7<T1, T2, T3, T4, T5, T6, T7, TResult>(f: (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7) => TResult) {
  return curried.get(f) as (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => TResult
    ?? ((a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => f(a1, a2, a3, a4, a5, a6, a7));
}

export function uncurry8<T1, T2, T3, T4, T5, T6, T7, T8, TResult>(
  f: (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => TResult
) {
  if (f == null) { return null as unknown as (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8) => TResult }
  const f2 = (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8) => f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8);
  curried.set(f2, f);
  return f2;
}

export function curry8<T1, T2, T3, T4, T5, T6, T7, T8, TResult>(f: (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8) => TResult) {
  return curried.get(f) as (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => TResult
    ?? ((a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => f(a1, a2, a3, a4, a5, a6, a7, a8));
}

export function uncurry9<T1, T2, T3, T4, T5, T6, T7, T8, T9, TResult>(
  f: (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => TResult
) {
  if (f == null) { return null as unknown as (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9) => TResult }
  const f2 = (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9) => f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9);
  curried.set(f2, f);
  return f2;
}

export function curry9<T1, T2, T3, T4, T5, T6, T7, T8, T9, TResult>(f: (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9) => TResult) {
  return curried.get(f) as (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => TResult
    ?? ((a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => f(a1, a2, a3, a4, a5, a6, a7, a8, a9));
}

export function uncurry10<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, TResult>(
  f: (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => TResult
) {
  if (f == null) { return null as unknown as (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10) => TResult }
  const f2 = (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10) => f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10);
  curried.set(f2, f);
  return f2;
}

export function curry10<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, TResult>(f: (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10) => TResult) {
  return curried.get(f) as (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => TResult
    ?? ((a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10));
}

export function uncurry11<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, TResult>(
  f: (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => (a11: T11) => TResult
) {
  if (f == null) { return null as unknown as (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11) => TResult }
  const f2 = (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11) => f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10)(a11);
  curried.set(f2, f);
  return f2;
}

export function curry11<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, TResult>(f: (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11) => TResult) {
  return curried.get(f) as (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => (a11: T11) => TResult
    ?? ((a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => (a11: T11) => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11));
}

export function uncurry12<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, TResult>(
  f: (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => (a11: T11) => (a12: T12) => TResult
) {
  if (f == null) { return null as unknown as (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12) => TResult }
  const f2 = (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12) => f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10)(a11)(a12);
  curried.set(f2, f);
  return f2;
}

export function curry12<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, TResult>(f: (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12) => TResult) {
  return curried.get(f) as (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => (a11: T11) => (a12: T12) => TResult
    ?? ((a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => (a11: T11) => (a12: T12) => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12));
}

export function uncurry13<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, TResult>(
  f: (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => (a11: T11) => (a12: T12) => (a13: T13) => TResult
) {
  if (f == null) { return null as unknown as (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12, a13: T13) => TResult }
  const f2 = (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12, a13: T13) => f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10)(a11)(a12)(a13);
  curried.set(f2, f);
  return f2;
}

export function curry13<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, TResult>(f: (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12, a13: T13) => TResult) {
  return curried.get(f) as (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => (a11: T11) => (a12: T12) => (a13: T13) => TResult
    ?? ((a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => (a11: T11) => (a12: T12) => (a13: T13) => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13));
}

export function uncurry14<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, TResult>(
  f: (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => (a11: T11) => (a12: T12) => (a13: T13) => (a14: T14) => TResult
) {
  if (f == null) { return null as unknown as (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12, a13: T13, a14: T14) => TResult }
  const f2 = (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12, a13: T13, a14: T14) => f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10)(a11)(a12)(a13)(a14);
  curried.set(f2, f);
  return f2;
}

export function curry14<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, TResult>(f: (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12, a13: T13, a14: T14) => TResult) {
  return curried.get(f) as (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => (a11: T11) => (a12: T12) => (a13: T13) => (a14: T14) => TResult
    ?? ((a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => (a11: T11) => (a12: T12) => (a13: T13) => (a14: T14) => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14));
}

export function uncurry15<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, TResult>(
  f: (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => (a11: T11) => (a12: T12) => (a13: T13) => (a14: T14) => (a15: T15) => TResult
) {
  if (f == null) { return null as unknown as (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12, a13: T13, a14: T14, a15: T15) => TResult }
  const f2 = (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12, a13: T13, a14: T14, a15: T15) => f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10)(a11)(a12)(a13)(a14)(a15);
  curried.set(f2, f);
  return f2;
}

export function curry15<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, TResult>(f: (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12, a13: T13, a14: T14, a15: T15) => TResult) {
  return curried.get(f) as (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => (a11: T11) => (a12: T12) => (a13: T13) => (a14: T14) => (a15: T15) => TResult
    ?? ((a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => (a11: T11) => (a12: T12) => (a13: T13) => (a14: T14) => (a15: T15) => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15));
}

export function uncurry16<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, TResult>(
  f: (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => (a11: T11) => (a12: T12) => (a13: T13) => (a14: T14) => (a15: T15) => (a16: T16) => TResult
) {
  if (f == null) { return null as unknown as (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12, a13: T13, a14: T14, a15: T15, a16: T16) => TResult }
  const f2 = (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12, a13: T13, a14: T14, a15: T15, a16: T16) => f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10)(a11)(a12)(a13)(a14)(a15)(a16);
  curried.set(f2, f);
  return f2;
}

export function curry16<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, TResult>(f: (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12, a13: T13, a14: T14, a15: T15, a16: T16) => TResult) {
  return curried.get(f) as (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => (a11: T11) => (a12: T12) => (a13: T13) => (a14: T14) => (a15: T15) => (a16: T16) => TResult
    ?? ((a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => (a11: T11) => (a12: T12) => (a13: T13) => (a14: T14) => (a15: T15) => (a16: T16) => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16));
}

export function uncurry17<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, TResult>(
  f: (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => (a11: T11) => (a12: T12) => (a13: T13) => (a14: T14) => (a15: T15) => (a16: T16) => (a17: T17) => TResult
) {
  if (f == null) { return null as unknown as (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12, a13: T13, a14: T14, a15: T15, a16: T16, a17: T17) => TResult }
  const f2 = (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12, a13: T13, a14: T14, a15: T15, a16: T16, a17: T17) => f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10)(a11)(a12)(a13)(a14)(a15)(a16)(a17);
  curried.set(f2, f);
  return f2;
}

export function curry17<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, TResult>(f: (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12, a13: T13, a14: T14, a15: T15, a16: T16, a17: T17) => TResult) {
  return curried.get(f) as (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => (a11: T11) => (a12: T12) => (a13: T13) => (a14: T14) => (a15: T15) => (a16: T16) => (a17: T17) => TResult
    ?? ((a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => (a11: T11) => (a12: T12) => (a13: T13) => (a14: T14) => (a15: T15) => (a16: T16) => (a17: T17) => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17));
}

export function uncurry18<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, TResult>(
  f: (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => (a11: T11) => (a12: T12) => (a13: T13) => (a14: T14) => (a15: T15) => (a16: T16) => (a17: T17) => (a18: T18) => TResult
) {
  if (f == null) { return null as unknown as (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12, a13: T13, a14: T14, a15: T15, a16: T16, a17: T17, a18: T18) => TResult }
  const f2 = (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12, a13: T13, a14: T14, a15: T15, a16: T16, a17: T17, a18: T18) => f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10)(a11)(a12)(a13)(a14)(a15)(a16)(a17)(a18);
  curried.set(f2, f);
  return f2;
}

export function curry18<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, TResult>(f: (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12, a13: T13, a14: T14, a15: T15, a16: T16, a17: T17, a18: T18) => TResult) {
  return curried.get(f) as (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => (a11: T11) => (a12: T12) => (a13: T13) => (a14: T14) => (a15: T15) => (a16: T16) => (a17: T17) => (a18: T18) => TResult
    ?? ((a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => (a11: T11) => (a12: T12) => (a13: T13) => (a14: T14) => (a15: T15) => (a16: T16) => (a17: T17) => (a18: T18) => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18));
}

export function uncurry19<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, TResult>(
  f: (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => (a11: T11) => (a12: T12) => (a13: T13) => (a14: T14) => (a15: T15) => (a16: T16) => (a17: T17) => (a18: T18) => (a19: T19) => TResult
) {
  if (f == null) { return null as unknown as (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12, a13: T13, a14: T14, a15: T15, a16: T16, a17: T17, a18: T18, a19: T19) => TResult }
  const f2 = (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12, a13: T13, a14: T14, a15: T15, a16: T16, a17: T17, a18: T18, a19: T19) => f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10)(a11)(a12)(a13)(a14)(a15)(a16)(a17)(a18)(a19);
  curried.set(f2, f);
  return f2;
}

export function curry19<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, TResult>(f: (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12, a13: T13, a14: T14, a15: T15, a16: T16, a17: T17, a18: T18, a19: T19) => TResult) {
  return curried.get(f) as (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => (a11: T11) => (a12: T12) => (a13: T13) => (a14: T14) => (a15: T15) => (a16: T16) => (a17: T17) => (a18: T18) => (a19: T19) => TResult
    ?? ((a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => (a11: T11) => (a12: T12) => (a13: T13) => (a14: T14) => (a15: T15) => (a16: T16) => (a17: T17) => (a18: T18) => (a19: T19) => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19));
}

export function uncurry20<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, TResult>(
  f: (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => (a11: T11) => (a12: T12) => (a13: T13) => (a14: T14) => (a15: T15) => (a16: T16) => (a17: T17) => (a18: T18) => (a19: T19) => (a20: T20) => TResult
) {
  if (f == null) { return null as unknown as (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12, a13: T13, a14: T14, a15: T15, a16: T16, a17: T17, a18: T18, a19: T19, a20: T20) => TResult }
  const f2 = (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12, a13: T13, a14: T14, a15: T15, a16: T16, a17: T17, a18: T18, a19: T19, a20: T20) => f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10)(a11)(a12)(a13)(a14)(a15)(a16)(a17)(a18)(a19)(a20);
  curried.set(f2, f);
  return f2;
}

export function curry20<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, TResult>(f: (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12, a13: T13, a14: T14, a15: T15, a16: T16, a17: T17, a18: T18, a19: T19, a20: T20) => TResult) {
  return curried.get(f) as (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => (a11: T11) => (a12: T12) => (a13: T13) => (a14: T14) => (a15: T15) => (a16: T16) => (a17: T17) => (a18: T18) => (a19: T19) => (a20: T20) => TResult
    ?? ((a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => (a11: T11) => (a12: T12) => (a13: T13) => (a14: T14) => (a15: T15) => (a16: T16) => (a17: T17) => (a18: T18) => (a19: T19) => (a20: T20) => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20));
}

// More performant method to copy arrays, see #2352
export function copyToArray<T>(source: T[], sourceIndex: number, target: T[], targetIndex: number, count: number): void {
  if (ArrayBuffer.isView(source) && ArrayBuffer.isView(target)) {
    (target as any).set((source as any).subarray(sourceIndex, sourceIndex + count), targetIndex);
  } else {
    for (let i = 0; i < count; ++i) {
      target[targetIndex + i] = source[sourceIndex + i];
    }
  }
}
