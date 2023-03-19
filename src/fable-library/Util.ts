// tslint:disable:ban-types

export type Nullable<T> = T | undefined;

export type Option<T> = T | Some<T> | undefined;

// Using a class here for better compatibility with TS files importing Some
export class Some<T> {
  public value: T;

  constructor(value: T) {
    this.value = value;
  }

  public toJSON() {
    return this.value;
  }

  // Don't add "Some" for consistency with erased options
  public toString() {
    return String(this.value);
  }

  public GetHashCode() {
    return structuralHash(this.value);
  }

  public Equals(other: Option<T>): boolean {
    if (other == null) {
      return false;
    } else {
      return equals(this.value, other instanceof Some ? other.value : other);
    }
  }

  public CompareTo(other: Option<T>) {
    if (other == null) {
      return 1;
    } else {
      return compare(this.value, other instanceof Some ? other.value : other);
    }
  }
}

export function value<T>(x: Option<T>) {
  if (x == null) {
    throw new Error("Option has no value");
  } else {
    return x instanceof Some ? x.value : x;
  }
}

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

export interface ICollection<T> extends IterableIterator<T> {
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

export interface IEnumerable<T> extends IterableIterator<T> {
  GetEnumerator(): IEnumerator<T>;
}

export class Enumerable<T> implements IEnumerable<T> {
  constructor(private en: IEnumerator<T>) {}
  public GetEnumerator(): IEnumerator<T> { return this.en; }
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

export function toIterator<T>(en: IEnumerator<T>): IterableIterator<T> {
  return {
    [Symbol.iterator]() {
      return this;
    },
    next() {
      const hasNext = en["System.Collections.IEnumerator.MoveNext"]();
      const current = hasNext ? en["System.Collections.Generic.IEnumerator`1.get_Current"]() : undefined;
      return { done: !hasNext, value: current } as IteratorResult<T>;
    },
  };
}

export function enumerableToIterator<T>(e: IEnumerable<T> | Iterable<T>): IterableIterator<T> {
  return toIterator(toEnumerable(e).GetEnumerator());
}

export interface ISet<T> {
  add(value: T): this;
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

export interface IMap<K, V> {
  clear(): void;
  delete(key: K): boolean;
  forEach(callbackfn: (value: V, key: K, map: IMap<K, V>) => void, thisArg?: any): void;
  get(key: K): V | undefined;
  has(key: K): boolean;
  set(key: K, value: V): this;
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
  let str = i.toString(10);
  while (str.length < length) {
    str = "0" + str;
  }
  return str;
}

export function padLeftAndRightWithZeros(i: number, lengthLeft: number, lengthRight: number) {
  let str = i.toString(10);
  while (str.length < lengthLeft) {
    str = "0" + str;
  }
  while (str.length < lengthRight) {
    str = str + "0";
  }
  return str;
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

// From https://stackoverflow.com/a/37449594
export function combineHashCodes(hashes: number[]) {
  if (hashes.length === 0) { return 0; }
  return hashes.reduce((h1, h2) => {
    return ((h1 << 5) + h1) ^ h2;
  });
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
export function safeHash(x: IHashable | undefined): number {
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

export function comparePrimitives(x: any, y: any): number {
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

export function jsOptions(mutator: (x: object) => void): object {
  const opts = {};
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

const CURRIED = Symbol("curried");

export function uncurry(arity: number, f: Option<Function>): any {
  // f may be a function option with None value
  if (f == null) {
    return f;
  }
  const f2 = value(f);
  if (f2.length > 1) {
    return f2;
  }
  const uncurried = (...args: any[]) => {
    let res = f2;
    for (let i = 0; i < arity; i++) {
      res = res(args[i]);
    }
    return res;
  }
  (uncurried as any)[CURRIED] = f2;
  return uncurried;
}

function _curry(args: any[],  arity: number, f: Function): (x: any) => any {
  return (arg: any) => arity === 1
    ? f(...args.concat([arg]))
    // Note it's important to generate a new args array every time
    // because a partially applied function can be run multiple times
    : _curry(args.concat([arg]), arity - 1, f);
}

export function curry(arity: number, f: Option<Function>): any {
  if (f == null) {
    return f;
  }
  const f2 = value(f);
  if (f2.length === 1) {
    return f2;
  }
  else if (CURRIED in f2) {
    return (f2 as any)[CURRIED];
  } else {
    return _curry([], arity, f2);
  }
}

export function checkArity(arity: number, f: Function): Function {
  return f.length > arity
    ? (...args1: any[]) => (...args2: any[]) => f.apply(undefined, args1.concat(args2))
    : f;
}

export function partialApply(arity: number, f: Function, args: any[]): any {
  if (f == null) {
    return undefined;
  } else if (CURRIED in f) {
    f = (f as any)[CURRIED];
    for (let i = 0; i < args.length; i++) {
      f = f(args[i]);
    }
    return f;
  } else {
    return _curry(args, arity, f);
  }
}

type CurriedArgMapping = [number, number] | 0; // expected arity, actual arity

export function mapCurriedArgs(fn: Function, mappings: CurriedArgMapping[]) {
  function mapArg(fn: Function, arg: any, mappings: CurriedArgMapping[], idx: number) {
    const mapping = mappings[idx];
    if (mapping !== 0) {
      const expectedArity = mapping[0];
      const actualArity = mapping[1];
      if (expectedArity > 1) {
        arg = curry(expectedArity, arg);
      }
      if (actualArity > 1) {
        arg = uncurry(actualArity, arg);
      }
    }
    const res = fn(arg);
    if (idx + 1 === mappings.length) {
      return res;
    } else {
      return (arg: any) => mapArg(res, arg, mappings, idx + 1);
    }
  }
  return (arg: any) => mapArg(fn, arg, mappings, 0);
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
