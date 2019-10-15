// tslint:disable:ban-types

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
  public factory: () => T;
  public isValueCreated: boolean;

  private createdValue: T;

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

export function identityHash(x: any): number {
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

export function structuralHash(x: any): number {
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
      if (typeof x.GetHashCode === "function") {
        return x.GetHashCode();
      } else if (isArray(x)) {
        const ar = (x as ArrayLike<any>);
        const len = ar.length;
        const hashes: number[] = new Array(len);
        for (let i = 0; i < len; i++) {
          hashes[i] = structuralHash(ar[i]);
        }
        return combineHashCodes(hashes);
      } else {
        return stringHash(String(x));
      }
    }
  }
}

export function isArray(x: any) {
  return Array.isArray(x) || ArrayBuffer.isView(x);
}

export function isIterable(x: any) {
  return x != null && typeof x === "object" && Symbol.iterator in x;
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

// export function equalObjects(x: { [k: string]: any }, y: { [k: string]: any }): boolean {
//   if (x == null) { return y == null; }
//   if (y == null) { return false; }
//   const xKeys = Object.keys(x);
//   const yKeys = Object.keys(y);
//   if (xKeys.length !== yKeys.length) {
//     return false;
//   }
//   xKeys.sort();
//   yKeys.sort();
//   for (let i = 0; i < xKeys.length; i++) {
//     if (xKeys[i] !== yKeys[i] || !equals(x[xKeys[i]], y[yKeys[i]])) {
//       return false;
//     }
//   }
//   return true;
// }

export function equals(x: any, y: any): boolean {
  if (x === y) {
    return true;
  } else if (x == null) {
    return y == null;
  } else if (y == null) {
    return false;
  } else if (typeof x !== "object") {
    return false;
  } else if (typeof x.Equals === "function") {
    return x.Equals(y);
  } else if (isArray(x)) {
    return isArray(y) && equalArrays(x, y);
  } else if (x instanceof Date) {
    return (y instanceof Date) && compareDates(x, y) === 0;
  } else {
    return false;
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

export function compareObjects(x: { [k: string]: any }, y: { [k: string]: any }): number {
  if (x == null) { return y == null ? 0 : 1; }
  if (y == null) { return -1; }
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

export function compare(x: any, y: any): number {
  if (x === y) {
    return 0;
  } else if (x == null) {
    return y == null ? 0 : -1;
  } else if (y == null) {
    return 1;
  } else if (typeof x !== "object") {
    return x < y ? -1 : 1;
  } else if (typeof x.CompareTo === "function") {
    return x.CompareTo(y);
  } else if (isArray(x)) {
    return isArray(y) && compareArrays(x, y);
  } else if (x instanceof Date) {
    return (y instanceof Date) && compareDates(x, y);
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

export function createAtom<T>(value: T): (v?: T) => T | void {
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
  SnakeCase: 2,
  SnakeCaseAllCaps: 3,
  KebabCase: 4,
};

function dashify(str: string, separator: string) {
    return str.replace(/[a-z]?[A-Z]/g, (m) => m.length === 1
        ? m.toLowerCase()
        : m.charAt(0) + separator + m.charAt(1).toLowerCase());
}

function changeCase(str: string, caseRule: number) {
  switch (caseRule) {
    case CaseRules.LowerFirst:
      return str.charAt(0).toLowerCase() + str.slice(1);
    case CaseRules.SnakeCase:
      return dashify(str, "_");
    case CaseRules.SnakeCaseAllCaps:
      return dashify(str, "_").toUpperCase();
    case CaseRules.KebabCase:
      return dashify(str, "-");
    case CaseRules.None:
    default:
      return str;
  }
}

export function createObj(fields: Iterable<any>, caseRule = CaseRules.None) {
  function fail(kvPair: any) {
    throw new Error("Cannot infer key and value of " + String(kvPair));
  }
  const o: { [k: string]: any } = {};
  const definedCaseRule = caseRule;
  for (let kvPair of fields) {
    let caseRule = CaseRules.None;
    if (kvPair == null) {
      fail(kvPair);
    }
    // Deflate unions and use the defined case rule
    if (typeof kvPair.toJSON === "function") {
      kvPair = kvPair.toJSON();
      caseRule = definedCaseRule;
    }
    if (Array.isArray(kvPair)) {
      switch (kvPair.length) {
        case 0:
          fail(kvPair);
          break;
        case 1:
          o[changeCase(kvPair[0], caseRule)] = true;
          break;
        case 2:
          const value = kvPair[1];
          o[changeCase(kvPair[0], caseRule)] = value;
          break;
        default:
          o[changeCase(kvPair[0], caseRule)] = kvPair.slice(1);
      }
    } else if (typeof kvPair === "string") {
      o[changeCase(kvPair, caseRule)] = true;
    } else {
      fail(kvPair);
    }
  }
  return o;
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

export function randomBytes(buffer: Uint8Array) {
  if (buffer == null) { throw new Error("Buffer cannot be null"); }
  for (let i = 0; i < buffer.length; i += 6) {
    // Pick random 48-bit number. Fill buffer in 2 24-bit chunks to avoid bitwise truncation.
    let r = Math.floor(Math.random() * 281474976710656); // Low 24 bits = chunk 1.
    const rhi = Math.floor(r / 16777216); // High 24 bits shifted via division = chunk 2.
    for (let j = 0; j < 6 && i + j < buffer.length; j++) {
      if (j === 3) { r = rhi; }
      buffer[i + j] = r & 255;
      r >>>= 8;
    }
  }
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
  if (isArray(col)) {
    return (col as T[]).length;
  } else {
    let count = 0;
    for (const _ of col) {
      count++;
    }
    return count;
  }
}

export function clear<T>(col: Iterable<T>) {
  if (isArray(col)) {
    (col as any).splice(0);
  } else {
    (col as any).clear();
  }
}

const CURRIED_KEY = "__CURRIED__";

export function uncurry(arity: number, f: Function) {
  // f may be a function option with None value
  if (f == null) { return null; }

  // The function is already uncurried
  if (f.length > 1) {
    // if (CURRIED_KEY in f) { // This doesn't always work
    return f;
  }

  let uncurriedFn: Function;
  switch (arity) {
    case 2:
      uncurriedFn = (a1: any, a2: any) => f(a1)(a2);
      break;
    case 3:
      uncurriedFn = (a1: any, a2: any, a3: any) => f(a1)(a2)(a3);
      break;
    case 4:
      uncurriedFn = (a1: any, a2: any, a3: any, a4: any) => f(a1)(a2)(a3)(a4);
      break;
    case 5:
      uncurriedFn = (a1: any, a2: any, a3: any, a4: any, a5: any) => f(a1)(a2)(a3)(a4)(a5);
      break;
    case 6:
      uncurriedFn = (a1: any, a2: any, a3: any, a4: any, a5: any, a6: any) => f(a1)(a2)(a3)(a4)(a5)(a6);
      break;
    case 7:
      uncurriedFn = (a1: any, a2: any, a3: any, a4: any, a5: any, a6: any, a7: any) =>
        f(a1)(a2)(a3)(a4)(a5)(a6)(a7);
      break;
    case 8:
      uncurriedFn = (a1: any, a2: any, a3: any, a4: any, a5: any, a6: any, a7: any, a8: any) =>
        f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8);
      break;
    default:
      throw new Error("Uncurrying to more than 8-arity is not supported: " + arity);
  }
  (uncurriedFn as any)[CURRIED_KEY] = f;
  return uncurriedFn;
}

export function curry(arity: number, f: Function): Function {
  if (f == null) { return null; }
  if (CURRIED_KEY in f) {
    return (f as any)[CURRIED_KEY];
  }
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

export function partialApply(arity: number, f: Function, args: any[]): any {
  if (f == null) {
    return null;
  } else if (CURRIED_KEY in f) {
    f = (f as any)[CURRIED_KEY];
    for (let i = 0; i < args.length; i++) {
      f = f(args[i]);
    }
    return f;
  } else {
    switch (arity) {
      case 1:
        // Wrap arguments to make sure .concat doesn't destruct arrays. Example
        // [1,2].concat([3,4],5)   --> [1,2,3,4,5]    // fails
        // [1,2].concat([[3,4],5]) --> [1,2,[3,4],5]  // ok
        return (a1: any) => f.apply(null, args.concat([a1]));
      case 2:
        return (a1: any) => (a2: any) => f.apply(null, args.concat([a1, a2]));
      case 3:
        return (a1: any) => (a2: any) => (a3: any) => f.apply(null, args.concat([a1, a2, a3]));
      case 4:
        return (a1: any) => (a2: any) => (a3: any) => (a4: any) => f.apply(null, args.concat([a1, a2, a3, a4]));
      case 5:
        return (a1: any) => (a2: any) => (a3: any) =>
          (a4: any) => (a5: any) => f.apply(null, args.concat([a1, a2, a3, a4, a5]));
      case 6:
        return (a1: any) => (a2: any) => (a3: any) => (a4: any) =>
          (a5: any) => (a6: any) => f.apply(null, args.concat([a1, a2, a3, a4, a5, a6]));
      case 7:
        return (a1: any) => (a2: any) => (a3: any) => (a4: any) => (a5: any) =>
          (a6: any) => (a7: any) => f.apply(null, args.concat([a1, a2, a3, a4, a5, a6, a7]));
      case 8:
        return (a1: any) => (a2: any) => (a3: any) => (a4: any) => (a5: any) => (a6: any) =>
          (a7: any) => (a8: any) => f.apply(null, args.concat([a1, a2, a3, a4, a5, a6, a7, a8]));
      default:
        throw new Error("Partially applying to more than 8-arity is not supported: " + arity);
    }
  }
}

type CurriedArgMapping = [number, number] | 0;

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

export function addToDict<K, V>(dict: Map<K, V>, k: K, v: V) {
  if (dict.has(k)) {
    throw new Error("An item with the same key has already been added. Key: " + k);
  }
  dict.set(k, v);
}

export function getItemFromDict<K, V>(map: Map<K, V>, key: K): V {
  if (map.has(key)) {
    return map.get(key);
  } else {
    throw new Error(`The given key '${key}' was not present in the dictionary.`);
  }
}
