import FSymbol from "./Symbol"

export interface IComparer<T> {
  Compare(x: T, y: T): number;
}

export interface IComparable<T> {
  CompareTo(x: T): number;
}

export interface IEquatable<T> {
  Equals(x: T): boolean;
}

export interface IDisposable {
  Dispose(): void;
}

export type NonDeclaredTypeKind =
  | "Any" | "Unit" | "Option" | "Array" | "Tuple"
  | "GenericParam" | "GenericType" | "Interface"

export type Type = FunctionConstructor;
export type Dic<T> = {[key: string]: T}

export class NonDeclaredType implements IEquatable<NonDeclaredType> {
  public kind: NonDeclaredTypeKind;
  public definition: string | Type;
  public generics: Type | Type[] | Dic<Type>;

  constructor(kind: NonDeclaredTypeKind, definition?: string | Type, generics?: Type | Type[] | Dic<Type>) {
    this.kind = kind;
    this.definition = definition;
    this.generics = generics;
  }

  Equals(other: NonDeclaredType) {
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
  return new NonDeclaredType("Option", "Option", t) as NonDeclaredType;
}

function FArray(t: Type) {
  return new NonDeclaredType("Array", "Array", t) as NonDeclaredType;
}
export { FArray as Array }

export function Tuple(ts: Type[]) {
  return new NonDeclaredType("Tuple", "Tuple", ts) as NonDeclaredType;
}

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
  return typ instanceof NonDeclaredType && typ.generics != null;
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
    const newInfo: any = {}, parentInfo = parent[FSymbol.reflection]();
    Object.getOwnPropertyNames(info).forEach(k => {
      const i = info[k];
      if (typeof i === "object") {
        newInfo[k] = Array.isArray(i)
          ? (parentInfo[k] || []).concat(i)
          : Object.assign(parentInfo[k] || {}, i);
      }
      else {
        newInfo[k] = i;
      }
    });
    return newInfo;
  }
  return info;
}

export function hasInterface(obj: any, interfaceName: string) {
  if (typeof obj[FSymbol.reflection] === "function") {
    const interfaces = obj[FSymbol.reflection]().interfaces;
    return Array.isArray(interfaces) && interfaces.indexOf(interfaceName) > -1;
  }
  return false;
}

export function isArray(obj: any) {
  return Array.isArray(obj) || ArrayBuffer.isView(obj);
}

export function getRestParams(args: ArrayLike<any>, idx: number) {
  for (var _len = args.length, restArgs = Array(_len > idx ? _len - idx : 0), _key = idx; _key < _len; _key++)
    restArgs[_key - idx] = args[_key];
  return restArgs;
}

export function toString(o: any) {
  return o != null && typeof o.ToString == "function" ? o.ToString() : String(o);
}

export function hash(x: any): number {
  let s = JSON.stringify(x);
  let h = 5381, i = 0, len = s.length;
  while (i < len) { h = (h * 33) ^ s.charCodeAt(i++); }
  return h;
}

export function equals(x: any, y: any): boolean {
  // Optimization if they are referencially equal
  if (x === y)
    return true;
  else if (x == null) // Return true if both are null or undefined
    return y == null;
  else if (y == null)
    return false;
  else if (Object.getPrototypeOf(x) !== Object.getPrototypeOf(y))
    return false;
  // Equals override or IEquatable implementation
  else if (typeof x.Equals === "function")
    return x.Equals(y);
  else if (Array.isArray(x)) {
    if (x.length != y.length) return false;
    for (let i = 0; i < x.length; i++)
      if (!equals(x[i], y[i])) return false;
    return true;
  }
  else if (ArrayBuffer.isView(x)) {
    if (x.byteLength !== y.byteLength) return false;
    const dv1 = new DataView(x.buffer), dv2 = new DataView(y.buffer);
    for (let i = 0; i < x.byteLength; i++)
      if (dv1.getUint8(i) !== dv2.getUint8(i)) return false;
    return true;
  }
  else if (x instanceof Date)
    return x.getTime() == y.getTime();
  else
    return false;
}

export function compare(x: any, y: any): number {
  // Optimization if they are referencially equal
  if (x === y)
    return 0;
  if (x == null) // Return 0 if both are null or undefined
    return y == null ? 0 : -1;
  else if (y == null)
    return 1; // everything is bigger than null
  else if (Object.getPrototypeOf(x) !== Object.getPrototypeOf(y))
    return -1;
  // Some types (see Long.ts) may just implement the function and not the interface
  // else if (hasInterface(x, "System.IComparable"))
  else if (typeof x.CompareTo === "function")
    return x.CompareTo(y);
  else if (Array.isArray(x)) {
    if (x.length != y.length) return x.length < y.length ? -1 : 1;
    for (let i = 0, j = 0; i < x.length; i++)
      if ((j = compare(x[i], y[i])) !== 0)
        return j;
    return 0;
  }
  else if (ArrayBuffer.isView(x)) {
    if (x.byteLength != y.byteLength) return x.byteLength < y.byteLength ? -1 : 1;
    const dv1 = new DataView(x.buffer), dv2 = new DataView(y.buffer);
    for (let i = 0, b1 = 0, b2 = 0; i < x.byteLength; i++) {
      b1 = dv1.getUint8(i), b2 = dv2.getUint8(i);
      if (b1 < b2) return -1;
      if (b1 > b2) return 1;
    }
    return 0;
  }
  else if (x instanceof Date)
    return compare(x.getTime(), y.getTime());
  else
    return x < y ? -1 : 1;
}

export function equalsRecords(x: any, y: any): boolean {
  // Optimization if they are referencially equal
  if (x === y) {
    return true;
  }
  else {
    const keys = Object.getOwnPropertyNames(x);
    for (let i=0; i<keys.length; i++) {
      if (!equals(x[keys[i]], y[keys[i]]))
        return false;
    }
    return true;
  }
}

export function compareRecords(x: any, y: any): number {
  // Optimization if they are referencially equal
  if (x === y) {
    return 0;
  }
  else {
    const keys = Object.getOwnPropertyNames(x);
    for (let i=0; i<keys.length; i++) {
      let res = compare(x[keys[i]], y[keys[i]]);
      if (res !== 0)
        return res;
    }
    return 0;
  }
}

export function equalsUnions(x: any, y: any): boolean {
  // Optimization if they are referencially equal
  if (x === y) {
    return true;
  }
  else if (x.Case !== y.Case) {
    return false;
  }
  else {
    for (let i=0; i<x.Fields.length; i++) {
      if (!equals(x.Fields[i], y.Fields[i]))
        return false;
    }
    return true;
  }
}

export function compareUnions(x: any, y: any): number {
  // Optimization if they are referencially equal
  if (x === y) {
    return 0;
  }
  else {
    let res = compare(x.Case, y.Case)
    if (res !== 0)
      return res;
    for (let i=0; i<x.Fields.length; i++) {
      res = compare(x.Fields[i], y.Fields[i]);
      if (res !== 0)
        return res;
    }
    return 0;
  }
}

export function createDisposable(f: () => void): IDisposable {
  return {
    Dispose: f,
    [FSymbol.reflection]() { return { interfaces: ["System.IDisposable"] } }
  }
}

export function createObj(fields: Iterable<[string, any]>) {
  const iter = fields[Symbol.iterator]();
  let cur = iter.next(), o: any = {};
  while (!cur.done) {
    o[cur.value[0]] = cur.value[1];
    cur = iter.next();
  }
  return o;
}

export function toPlainJsObj(source: any) {
  if (source != null && source.constructor != Object) {
    let target: { [index: string]: string } = {};
    let props = Object.getOwnPropertyNames(source);
    for (let i = 0; i < props.length; i++) {
      target[props[i]] = source[props[i]];
    }
    // Copy also properties from prototype, see #192
    const proto = Object.getPrototypeOf(source);
    if (proto != null) {
      props = Object.getOwnPropertyNames(proto);
      for (let i = 0; i < props.length; i++) {
        const prop = Object.getOwnPropertyDescriptor(proto, props[i]);
        if (prop.value) {
          target[props[i]] = prop.value;
        }
        else if (prop.get) {
          target[props[i]] = prop.get.apply(source);
        }
      }
    }
    return target;
  }
  else {
    return source;
  }
}

export function round(value : number, digits: number = 0) {
    const m = Math.pow(10, digits);
    const n = +(digits ? value * m : value).toFixed(8);
    const i = Math.floor(n), f = n - i;
    const e = 1e-8;
    const r = (f > 0.5 - e && f < 0.5 + e) ? ((i % 2 == 0) ? i : i + 1) : Math.round(n);
    return digits ? r / m : r;
}

export function randomNext(min: number, max: number) {
  return Math.floor(Math.random() * (max - min)) + min;
}
