import { compare as compareDates, toString as dateToString } from "./Date";

// Object.assign flattens getters and setters
// See https://stackoverflow.com/questions/37054596/js-es5-how-to-assign-objects-with-setters-and-getters
export function extend(target: any, ...sources: any[]) {
  for (const source of sources) {
    for (const key of Object.keys(source)) {
        Object.defineProperty(target, key, Object.getOwnPropertyDescriptor(source, key));
    }
  }
  return target;
}

export type Choice<T1, T2> = ["Choice1Of2", T1] | ["Choice2Of2", T2];

export interface IComparer<T> {
  Compare(x: T, y: T): number;
}

export interface IComparable<T> {
  CompareTo(x: T): number;
}

export interface IEqualityComparer<T> {
  Equals(x: T, y: T): boolean;
  GetHashCode(x: T): number;
}

export interface IEquatable<T> {
  Equals(x: T): boolean;
}

export interface IDisposable {
  Dispose(): void;
}

export function isDisposable(x: any) {
  return x != null && typeof x.Dispose === "function";
}

export class Comparer<T> implements IComparer<T> {
  public Compare: (x: T, y: T) => number;

  constructor(f?: (x: T, y: T) => number) {
    this.Compare = f || compare;
  }
}

export function comparerFromEqualityComparer<T>(comparer: IEqualityComparer<T>) {
  // Sometimes IEqualityComparer also implements IComparer
  if (typeof (comparer as any).Compare === "function") {
    return new Comparer<T>((comparer as any).Compare);
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

// TODO: Move these three methods to Map and Set modules
export function containsValue<K, V>(v: V, map: Map<K, V>) {
  for (const kv of map) {
    if (equals(v, kv[1])) {
      return true;
    }
  }
  return false;
}

export function tryGetValue<K, V>(map: Map<K, V>, key: K, defaultValue: V): [boolean, V] {
  return map.has(key) ? [true, map.get(key)] : [false, defaultValue];
}

export function addToSet<T>(v: T, set: Set<T>) {
  if (set.has(v)) {
    return false;
  }
  set.add(v);
  return true;
}

export class AssertionError<T> extends Error {
  public actual: T;
  public expected: T;
  constructor(msg: string, actual: T, expected: T) {
    super(msg);
    this.actual = actual;
    this.expected = expected;
  }
}

export function assertEqual<T>(actual: T, expected: T, msg?: string): void {
  if (!equals(actual, expected)) {
    throw new AssertionError(msg || `Expected: ${expected} - Actual: ${actual}`, actual, expected);
  }
}

export class Lazy<T> {
  public factory: () => T;
  public isValueCreated: boolean;

  private createdValue: T;

  constructor(factory: () => T) {
    this.factory = factory;
    this.isValueCreated = false;
  }

  get value() {
    if (!this.isValueCreated) {
      this.createdValue = this.factory();
      this.isValueCreated = true;
    }
    return this.createdValue;
  }
}

export function lazyFromValue<T>(v: T) {
  return new Lazy(() => v);
}

export function int16ToString(i: number, radix: number) {
  i = i < 0 && radix != null && radix !== 10 ? 0xFFFF + i + 1 : i;
  return i.toString(radix);
}

export function int32ToString(i: number, radix: number) {
  i = i < 0 && radix != null && radix !== 10 ? 0xFFFFFFFF + i + 1 : i;
  return i.toString(radix);
}

export function toString(obj: any, quoteStrings = false): string {
  if (obj == null) {
    return String(obj);
  }
  switch (typeof obj) {
    case "number":
    case "boolean":
    case "symbol":
    case "undefined":
      return String(obj);
    case "string":
      return quoteStrings ? JSON.stringify(obj) : obj;
    case "function":
      return obj.name;
    case "object":
      if (isPlainObject(obj)) {
        try {
          return JSON.stringify(obj, (k, v) => {
            if (v != null) {
              if (v instanceof Date) {
                return dateToString(v);
              } else if (v[Symbol.iterator] && !Array.isArray(v)) {
                return Array.from(v);
              }
            }
            return v;
          });
        } catch (err) {
          // Fallback for objects with circular references
          return "{" + Object.getOwnPropertyNames(obj).map((k) => k + ": " + String(obj[k])).join(", ") + "}";
        }
      } else {
        return obj instanceof Date ? dateToString(obj) : String(obj);
      }
  }
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

export function getHashCode(x: any): number {
  return ObjectRef.id(x) * 2654435761 | 0;
}

export function hash(x: any): number {
  if (typeof x === typeof 1) {
    return x * 2654435761 | 0;
  }
  if (x != null && typeof x.GetHashCode === "function") {
    return x.GetHashCode();
  } else {
    const s = toString(x);
    let h = 5381;
    let i = 0;
    const len = s.length;
    while (i < len) { h = (h * 33) ^ s.charCodeAt(i++); }
    return h;
  }
}

export function isArray(x: any) {
  return Array.isArray(x) || ArrayBuffer.isView(x);
}

export function isPlainObject(x: any) {
  return x != null && Object.getPrototypeOf(x).constructor === Object;
}

export function equalArrays<T>(x: ArrayLike<T>, y: ArrayLike<T>): boolean {
  if (x == null) { return y == null; }
  if (y == null) { return false; }
  if (x.length !== y.length) { return false; }
  for (let i = 0; i < x.length; i++) {
    if (!equals(x[i], y[i])) { return false; }
  }
  return true;
}

export function equalObjects(x: { [k: string]: any }, y: { [k: string]: any }): boolean {
  if (x == null) { return y == null; }
  if (y == null) { return false; }
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

export function equals(x: any, y: any): boolean {
  if (x === y) {
    return true;
  } else if (x == null) {
    return y == null;
  } else if (typeof x !== "object") {
    return false;
  } else if (typeof x.Equals === "function") {
    return x.Equals(y);
  } else if (isPlainObject(x)) {
    return isPlainObject(y) && equalObjects(x, y);
  } else if (isArray(x)) {
    return isArray(y) && equalArrays(x, y);
  } else if (x instanceof Date) {
    return x.getTime() === y.getTime();
  } else {
    return false;
  }
}

export function comparePrimitives(x: any, y: any): number {
  return x === y ? 0 : (x < y ? -1 : 1);
}

export function compareArrays<T>(x: ArrayLike<T>, y: ArrayLike<T>): number {
  if (x == null) { return y == null ? 0 : 1; }
  if (y == null) { return -1; }
  if (x.length !== y.length) {
    return x.length < y.length ? -1 : 1;
  }
  for (let i = 0, j = 0; i < x.length; i++) {
    j = compare(x[i], y[i]);
    if (j !== 0) { return j; }
  }
  return 0;
}

export function compareObjects(x: { [k: string]: any }, y: { [k: string]: any }): number {
  if (x == null) { return y == null ? 0 : 1; }
  if (y == null) { return -1; }
  const xhash = hash(x);
  const yhash = hash(y);
  if (xhash === yhash) {
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
  } else {
    return xhash < yhash ? -1 : 1;
  }
}

export function compare(x: any, y: any): number {
  if (x === y) {
    return 0;
  } else if (x == null) {
    return y == null ? 0 : -1;
  } else if (typeof x !== "object") {
    return x < y ? -1 : 1;
  } else if (typeof x.CompareTo === "function") {
    return x.CompareTo(y);
  } else if (isPlainObject(x)) {
    return isPlainObject(y) && compareObjects(x, y);
  } else if (isArray(x)) {
    return isArray(y) && compareArrays(x, y);
  } else if (x instanceof Date) {
    return compareDates(x, y);
  } else {
    return 1;
  }
}

export function min<T>(comparer: (x: T, y: T) => number, x: T, y: T) {
  return comparer(x, y) < 0 ? x : y;
}

export function max<T>(comparer: (x: T, y: T) => number, x: T, y: T) {
  return comparer(x, y) > 0 ? x : y;
}

export function createAtom<T>(value: T): (v?: T) => T|void {
  let atom = value;
  return (value: T) => {
    if (value === void 0) {
      return atom;
    } else {
      atom = value;
      return void 0;
    }
  };
}

const CaseRules = {
  None: 0,
  LowerFirst: 1,
};

export function createObj(fields: Iterable<any>, caseRule = CaseRules.None) {
  const o: { [k: string]: any } = {};
  for (const kvPair of fields) {
    if (Array.isArray(kvPair) && kvPair.length > 0) {
      if (kvPair.length === 1) {
        o[kvPair[0]] = true;
      } else if (kvPair.length === 2) {
        const value = kvPair[1];
        o[kvPair[0]] = typeof value[Symbol.iterator] === "function"
          ? createObj(value, caseRule)
          : value;
      } else {
        o[kvPair[0]] = kvPair.slice(1);
      }
    } else if (typeof kvPair === "string") {
      o[kvPair] = true;
    } else {
      throw new Error("Cannot infer key and value of " + toString(kvPair));
    }
  }
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

export function randomNext(min: number, max: number) {
  return Math.floor(Math.random() * (max - min)) + min;
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

// ICollection.Clear method can be called on IDictionary
// too so we need to make a runtime check (see #1120)
export function clear<T>(col: Iterable<T>) {
  if (Array.isArray(col)) {
    col.splice(0);
  } else {
    (col as any).clear();
  }
}

export interface ICurried {
  curried?: boolean;
  (...args: any[]): any;
}

/* tslint:disable */
export function uncurry(arity: number, f: Function) {
/* tslint:enable */

  // f may be a function option with None value
  if (f == null) {
    return null;
  }

  const wrap: ICurried = (...args: any[]) => {
    // In some cases there may be more arguments applied than necessary
    // (e.g. index when mapping an array), discard them
    let res: any = f;
    for (let i = 0; i < arity; i++) {
      const accArgs = [args[i]];
      const partialArity = Math.max(f.length, 1);
      while (accArgs.length < partialArity) {
        accArgs.push(args[++i]);
      }
      res = res.apply(null, accArgs);
    }
    return res;
  };
  wrap.curried = true;
  return wrap;
}

/* tslint:disable */
export function curry(arity: number, f: Function): Function {
/* tslint:disable */
  switch (arity) {
    case 2:
      return (a1: any) => (a2: any) => f(a1, a2);
    case 3:
      return (a1: any) => (a2: any) => (a3: any) => f(a1, a2, a3);
    case 4:
      return (a1: any) => (a2: any) => (a3: any) => (a4: any) => f(a1, a2, a3, a4);
    case 5:
      return (a1: any) => (a2: any) => (a3: any) =>
        (a4: any) => (a5: any) => f(a1, a2, a3, a4, a5);
    case 6:
      return (a1: any) => (a2: any) => (a3: any) => (a4: any) =>
        (a5: any) => (a6: any) => f(a1, a2, a3, a4, a5, a6);
    case 7:
      return (a1: any) => (a2: any) => (a3: any) => (a4: any) => (a5: any) =>
        (a6: any) => (a7: any) => f(a1, a2, a3, a4, a5, a6, a7);
    case 8:
      return (a1: any) => (a2: any) => (a3: any) => (a4: any) => (a5: any) => (a6: any) =>
        (a7: any) => (a8: any) => f(a1, a2, a3, a4, a5, a6, a7, a8);
    default:
      throw new Error("Currying to more than 8-arity is not supported: " + arity);
  }
}

export function partialApply(arity: number, f: ICurried, args: any[]): any {
  if (f.curried) {
    return f.apply(null, args);
  } else {
    switch (arity) {
      case 1:
        return (a1: any) => f.apply(null, args.concat(a1));
      case 2:
        return (a1: any) => (a2: any) => f.apply(null, args.concat(a1, a2));
      case 3:
        return (a1: any) => (a2: any) => (a3: any) => f.apply(null, args.concat(a1, a2, a3));
      case 4:
        return (a1: any) => (a2: any) => (a3: any) => (a4: any) => f.apply(null, args.concat(a1, a2, a3, a4));
      case 5:
        return (a1: any) => (a2: any) => (a3: any) =>
          (a4: any) => (a5: any) => f.apply(null, args.concat(a1, a2, a3, a4, a5));
      case 6:
        return (a1: any) => (a2: any) => (a3: any) => (a4: any) =>
          (a5: any) => (a6: any) => f.apply(null, args.concat(a1, a2, a3, a4, a5, a6));
      case 7:
        return (a1: any) => (a2: any) => (a3: any) => (a4: any) => (a5: any) =>
          (a6: any) => (a7: any) => f.apply(null, args.concat(a1, a2, a3, a4, a5, a6, a7));
      case 8:
        return (a1: any) => (a2: any) => (a3: any) => (a4: any) => (a5: any) => (a6: any) =>
          (a7: any) => (a8: any) => f.apply(null, args.concat(a1, a2, a3, a4, a5, a6, a7, a8));
      default:
        throw new Error("Partially applying to get a function with more than 8-arity is not supported: " + arity);
    }
  }
}
