import FSymbol from "./Symbol";

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

export type NonDeclaredTypeKind =
  | "Any" | "Unit" | "Option" | "Array" | "Tuple" | "Function"
  | "GenericParam" | "GenericType" | "Interface";

export type Type = string | NonDeclaredType | FunctionConstructor;
export interface Dic<T> { [key: string]: T; }

export class NonDeclaredType implements IEquatable<NonDeclaredType> {
  public kind: NonDeclaredTypeKind;
  public definition: Type;
  public generics: Type[] | Dic<Type>;

  constructor(kind: NonDeclaredTypeKind, definition?: string | Type, generics?: Type[] | Dic<Type>) {
    this.kind = kind;
    this.definition = definition;
    this.generics = generics;
  }

  public Equals(other: NonDeclaredType) {
    if (this.kind === other.kind && this.definition === other.definition) {
      return typeof this.generics === "object"
        // equalsRecords should also work for Type[] (tuples)
        ? equalsRecords(this.generics, other.generics)
        : this.generics === other.generics;
    }
    return false;
  }
}

export const Any = new NonDeclaredType("Any");

export const Unit = new NonDeclaredType("Unit");

export function Option(t: Type) {
  return new NonDeclaredType("Option", null, [t]) as NonDeclaredType;
}

function FableArray(t: Type, isTypedArray = false) {
  let def: Type = null;
  let genArg: Type = null;
  if (isTypedArray) {
    def = t;
  } else {
    genArg = t;
  }
  return new NonDeclaredType("Array", def, [genArg]) as NonDeclaredType;
}
export { FableArray as Array };

export function Tuple(types: Type[]) {
  return new NonDeclaredType("Tuple", null, types) as NonDeclaredType;
}

function FableFunction(types: Type[]) {
  return new NonDeclaredType("Function", null, types) as NonDeclaredType;
}
export { FableFunction as Function };

export function GenericParam(definition: string) {
  return new NonDeclaredType("GenericParam", definition);
}

export function Interface(definition: string) {
  return new NonDeclaredType("Interface", definition);
}

export function makeGeneric(typeDef: Type, genArgs: Dic<Type>) {
  return new NonDeclaredType("GenericType", typeDef, genArgs);
}

export function isGeneric(typ: any): boolean {
  return typ instanceof NonDeclaredType && typ.kind === "GenericType";
}

/**
 * Returns the parent if this is a declared generic type or the argument otherwise.
 * Attention: Unlike .NET this doesn't throw an exception if type is not generic.
 */
export function getDefinition(typ: any): any {
  return isGeneric(typ) ? typ.definition : typ;
}

export function extendInfo(cons: FunctionConstructor, info: any) {
  const parent: any = Object.getPrototypeOf(cons.prototype);
  if (typeof parent[FSymbol.reflection] === "function") {
    const newInfo: any = {};
    const parentInfo = parent[FSymbol.reflection]();
    Object.getOwnPropertyNames(info).forEach((k) => {
      const i = info[k];
      if (typeof i === "object") {
        newInfo[k] = Array.isArray(i)
          ? (parentInfo[k] || []).concat(i)
          : Object.assign(parentInfo[k] || {}, i);
      } else {
        newInfo[k] = i;
      }
    });
    return newInfo;
  }
  return info;
}

export function hasInterface(obj: any, interfaceName: string) {
  if (interfaceName === "System.Collections.Generic.IEnumerable") {
    return typeof obj[Symbol.iterator] === "function";
  } else if (typeof obj[FSymbol.reflection] === "function") {
    const interfaces = obj[FSymbol.reflection]().interfaces;
    return Array.isArray(interfaces) && interfaces.indexOf(interfaceName) > -1;
  }
  return false;
}

/**
 * Returns:
 * - Records: array with names of fields
 * - Classes: array with names of getters
 * - Nulls and unions: empty array
 * - JS Objects: The result of calling Object.getOwnPropertyNames
 */
export function getPropertyNames(obj: any) {
  if (obj == null) {
    return [];
  }
  const propertyMap = typeof obj[FSymbol.reflection] === "function" ? obj[FSymbol.reflection]().properties || [] : obj;
  return Object.getOwnPropertyNames(propertyMap);
}

export function isArray(obj: any) {
  return Array.isArray(obj) || ArrayBuffer.isView(obj);
}

export function toString(obj: any, quoteStrings = false): string {
  function isObject(x: any) {
    return x !== null && typeof x === "object" && !(x instanceof Number)
      && !(x instanceof String) && !(x instanceof Boolean);
  }
  if (obj == null || typeof obj === "number") {
    return String(obj);
  }
  if (typeof obj === "string") {
    return quoteStrings ? JSON.stringify(obj) : obj;
  }
  if (typeof obj.ToString === "function") {
    return obj.ToString();
  }
  if (hasInterface(obj, "FSharpUnion")) {
    const info = obj[FSymbol.reflection]();
    const uci = info.cases[obj.tag];
    switch (uci.length) {
      case 1:
        return uci[0];
      case 2:
        // For simplicity let's always use parens, in .NET they're ommitted in some cases
        return uci[0] + " (" + toString(obj.data, true) + ")";
      default:
        return uci[0] + " (" + obj.data.map((x: any) => toString(x, true)).join(",") + ")";
    }
  }
  try {
    return JSON.stringify(obj, (k, v) => {
      return v && v[Symbol.iterator] && !Array.isArray(v) && isObject(v) ? Array.from(v)
        : v && typeof v.ToString === "function" ? toString(v) : v;
    });
  } catch (err) {
    // Fallback for objects with circular references
    return "{" + Object.getOwnPropertyNames(obj).map((k) => k + ": " + String(obj[k])).join(", ") + "}";
  }
}

export function hash(x: any): number {
  if (x != null && typeof x.GetHashCode === "function") {
    return x.GetHashCode();
  } else {
    const s = JSON.stringify(x);
    let h = 5381;
    let i = 0;
    const len = s.length;
    while (i < len) { h = (h * 33) ^ s.charCodeAt(i++); }
    return h;
  }
}

export function equals(x: any, y: any): boolean {
  // Optimization if they are referencially equal
  if (x === y) {
    return true;
  } else if (x == null) { // Return true if both are null or undefined
    return y == null;
  } else if (y == null) {
    return false;
  } else if (Object.getPrototypeOf(x) !== Object.getPrototypeOf(y)) {
    return false;
    // Equals override or IEquatable implementation
  } else if (typeof x.Equals === "function") {
    return x.Equals(y);
  } else if (Array.isArray(x)) {
    if (x.length !== y.length) { return false; }
    for (let i = 0; i < x.length; i++) {
      if (!equals(x[i], y[i])) { return false; }
    }
    return true;
  } else if (ArrayBuffer.isView(x)) {
    if (x.byteLength !== y.byteLength) { return false; }
    const dv1 = new DataView(x.buffer);
    const dv2 = new DataView(y.buffer);
    for (let i = 0; i < x.byteLength; i++) {
      if (dv1.getUint8(i) !== dv2.getUint8(i)) { return false; }
    }
    return true;
  } else if (x instanceof Date) {
    return x.getTime() === y.getTime();
  } else {
    return false;
  }
}

export function comparePrimitives(x: any, y: any): number {
  return x === y ? 0 : (x < y ? -1 : 1);
}

export function compare(x: any, y: any): number {
  // Optimization if they are referencially equal
  if (x === y) {
    return 0;
  } else if (x == null) { // Return 0 if both are null or undefined
    return y == null ? 0 : -1;
  } else if (y == null) {
    return 1; // everything is bigger than null
  } else if (Object.getPrototypeOf(x) !== Object.getPrototypeOf(y)) {
    return -1;
    // Some types (see Long.ts) may just implement the function and not the interface
    // else if (hasInterface(x, "System.IComparable"))
  } else if (typeof x.CompareTo === "function") {
    return x.CompareTo(y);
  } else if (Array.isArray(x)) {
    if (x.length !== y.length) { return x.length < y.length ? -1 : 1; }
    for (let i = 0, j = 0; i < x.length; i++) {
      j = compare(x[i], y[i]);
      if (j !== 0) { return j; }
    }
    return 0;
  } else if (ArrayBuffer.isView(x)) {
    if (x.byteLength !== y.byteLength) { return x.byteLength < y.byteLength ? -1 : 1; }
    const dv1 = new DataView(x.buffer);
    const dv2 = new DataView(y.buffer);
    for (let i = 0, b1 = 0, b2 = 0; i < x.byteLength; i++) {
      b1 = dv1.getUint8(i), b2 = dv2.getUint8(i);
      if (b1 < b2) { return -1; }
      if (b1 > b2) { return 1; }
    }
    return 0;
  } else if (x instanceof Date) {
    const xtime = x.getTime();
    const ytime = y.getTime();
    return xtime === ytime ? 0 : (xtime < ytime ? -1 : 1);
  } else if (typeof x === "object") {
    const xhash = hash(x);
    const yhash = hash(y);
    if (xhash === yhash) {
      return equals(x, y) ? 0 : -1;
    } else {
      return xhash < yhash ? -1 : 1;
    }
  } else {
    return x < y ? -1 : 1;
  }
}

export function equalsRecords(x: any, y: any): boolean {
  // Optimization if they are referencially equal
  if (x === y) {
    return true;
  } else {
    const keys = getPropertyNames(x);
    for (const key of keys) {
      if (!equals(x[key], y[key])) {
        return false;
      }
    }
    return true;
  }
}

export function compareRecords(x: any, y: any): number {
  // Optimization if they are referencially equal
  if (x === y) {
    return 0;
  } else {
    const keys = getPropertyNames(x);
    for (const key of keys) {
      const res = compare(x[key], y[key]);
      if (res !== 0) {
        return res;
      }
    }
    return 0;
  }
}

export function equalsUnions(x: any, y: any): boolean {
  return x === y || (x.tag === y.tag && equals(x.data, y.data));
}

export function compareUnions(x: any, y: any): number {
  if (x === y) {
    return 0;
  } else {
    const res = x.tag < y.tag ? -1 : (x.tag > y.tag ? 1 : 0);
    return res !== 0 ? res : compare(x.data, y.data);
  }
}

export function createDisposable(f: () => void): IDisposable {
  return {
    Dispose: f,
    [FSymbol.reflection]() { return { interfaces: ["System.IDisposable"] }; },
  };
}

// tslint forbids non-arrow functions, but it's
// necessary here to use the arguments object
/* tslint:disable */
export function createAtom<T>(value: T): (v?: T) => T|undefined {
  let atom = value;
  return function() {
    return arguments.length === 0
      ? atom
      : (atom = arguments[0], void 0);
  };
}
/* tslint:enable */

const CaseRules = {
  None: 0,
  LowerFirst: 1,
};

function isList(o: any) {
  if (o != null) {
    if (typeof o[FSymbol.reflection] === "function") {
      return o[FSymbol.reflection]().type === "Microsoft.FSharp.Collections.FSharpList";
    }
  }
  return false;
}

export function createObj(
      fields: Iterable<[string, any]>,
      caseRule = CaseRules.None,
      casesCache?: Map<FunctionConstructor, any[]>) {
  const iter = fields[Symbol.iterator]();
  let cur = iter.next();
  const o: any = {};
  while (!cur.done) {
    const value: any = cur.value;
    if (Array.isArray(value)) {
      o[value[0]] = value[1];
    } else {
      casesCache = casesCache || new Map();
      const proto = Object.getPrototypeOf(value);
      let cases = casesCache.get(proto);
      if (cases == null) {
        if (typeof proto[FSymbol.reflection] === "function") {
          cases = proto[FSymbol.reflection]().cases;
          casesCache.set(proto, cases);
        }
      }
      const caseInfo = (cases != null) ? cases[value.tag] : null;
      if (Array.isArray(caseInfo)) {
        let key = caseInfo[0];
        if (caseRule === CaseRules.LowerFirst) {
          key = key[0].toLowerCase() + key.substr(1);
        }
        o[key] = caseInfo.length === 1
          ? true
          : (isList(value.data) ? createObj(value.data, caseRule, casesCache) : value.data);
      } else {
        throw new Error("Cannot infer key and value of " + value);
      }
    }
    cur = iter.next();
  }
  return o;
}

export function toPlainJsObj(source: any) {
  if (source != null && source.constructor !== Object) {
    const target: { [index: string]: string } = {};
    let props = Object.getOwnPropertyNames(source);
    for (const p of props) {
      target[p] = source[p];
    }
    // Copy also properties from prototype, see #192
    const proto = Object.getPrototypeOf(source);
    if (proto != null) {
      props = Object.getOwnPropertyNames(proto);
      for (const p of props) {
        const prop = Object.getOwnPropertyDescriptor(proto, p);
        if (prop.value) {
          target[p] = prop.value;
        } else if (prop.get) {
          target[p] = prop.get.apply(source);
        }
      }
    }
    return target;
  } else {
    return source;
  }
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

export function randomNext(min: number, max: number) {
  return Math.floor(Math.random() * (max - min)) + min;
}

export function defaultArg<T, U>(arg: T, defaultValue: T, f?: (x: T) => U) {
  return arg == null ? defaultValue : (f != null ? f(arg) : arg);
}

export function applyOperator(x: any, y: any, operator: string): any {
  function getMethod(obj: any): (...args: any[]) => any {
    if (typeof obj === "object") {
      const cons = Object.getPrototypeOf(obj).constructor;
      if (typeof cons[operator] === "function") {
        return cons[operator];
      }
    }
    return null;
  }

  let meth = getMethod(x);
  if (meth != null) {
    return meth(x, y);
  }

  meth = getMethod(y);
  if (meth != null) {
    return meth(x, y);
  }

  switch (operator) {
    case "op_Addition":
      return x + y;
    case "op_Subtraction":
      return x - y;
    case "op_Multiply":
      return x * y;
    case "op_Division":
      return x / y;
    case "op_Modulus":
      return x % y;
    case "op_LeftShift":
      return x << y;
    case "op_RightShift":
      return x >> y;
    case "op_BitwiseAnd":
      return x & y;
    case "op_BitwiseOr":
      return x | y;
    case "op_ExclusiveOr":
      return x ^ y;
    case "op_LogicalNot":
      return x + y;
    case "op_UnaryNegation":
      return !x;
    case "op_BooleanAnd":
      return x && y;
    case "op_BooleanOr":
      return x || y;
    default:
      return null;
  }
}

export function parseNumber(v: string): number {
  return +v;
}

export function tryParse<A>(v: string | null, initial: A, parser: RegExp, fn: (s: string) => A): [boolean, A] {
  if (v != null) {
    const a = parser.exec(v);

    if (a !== null) {
      return [true, fn(a[1])];
    }
  }

  return [false, initial];
}

export function parse<A>(v: string | null, initial: A, parser: RegExp, fn: (s: string) => A): A {
  const a = tryParse(v, initial, parser, fn);

  if (a[0]) {
    return a[1];

  } else {
    // TODO FormatException ?
    throw new Error("Input string was not in a correct format.");
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
