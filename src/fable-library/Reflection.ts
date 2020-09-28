import { value as getOptionValue } from "./Option.js";
import { anonRecord as makeAnonRecord, FSharpRef, List } from "./Types.js";
import { compareArraysWith, equalArraysWith, isArrayLike, isUnionLike } from "./Util.js";

export type FieldInfo = [string, TypeInfo];
export type PropertyInfo = FieldInfo;

export type Constructor = new (...args: any[]) => any;

export class CaseInfo {
  constructor(
    public declaringType: TypeInfo,
    public tag: number,
    public name: string,
    public fields?: FieldInfo[]) {
  }
}

export type EnumCase = [string, number];

export class TypeInfo {
  constructor(
    public fullname: string,
    public generics?: TypeInfo[],
    public construct?: Constructor,
    public fields?: () => FieldInfo[],
    public cases?: () => CaseInfo[],
    public enumCases?: EnumCase[]) {
  }
  public toString() {
    return this.ToString();
  }
  public ToString() {
    return fullName(this);
  }
  public Equals(other: TypeInfo) {
    return equals(this, other);
  }
  public CompareTo(other: TypeInfo) {
    return compare(this, other);
  }
}

export function getGenerics(t: TypeInfo): TypeInfo[] {
  return t.generics != null ? t.generics : [];
}

export function equals(t1: TypeInfo, t2: TypeInfo): boolean {
  if (t1.fullname === "") { // Anonymous records
    return t2.fullname === ""
      && equalArraysWith(getRecordElements(t1),
        getRecordElements(t2),
        ([k1, v1], [k2, v2]) => k1 === k2 && equals(v1, v2));
  } else {
    return t1.fullname === t2.fullname
      && equalArraysWith(getGenerics(t1), getGenerics(t2), equals);
  }
}

// System.Type is not comparable in .NET, but let's implement this
// in case users want to create a dictionary with types as keys
export function compare(t1: TypeInfo, t2: TypeInfo): number {
  if (t1.fullname !== t2.fullname) {
    return t1.fullname < t2.fullname ? -1 : 1;
  } else {
    return compareArraysWith(getGenerics(t1), getGenerics(t2), compare);
  }
}

export function class_type(
  fullname: string,
  generics?: TypeInfo[],
  construct?: Constructor): TypeInfo {
  return new TypeInfo(fullname, generics, construct);
}

export function record_type(
  fullname: string,
  generics: TypeInfo[],
  construct: Constructor,
  fields: () => FieldInfo[]): TypeInfo {
  return new TypeInfo(fullname, generics, construct, fields);
}

export function anonRecord_type(...fields: FieldInfo[]): TypeInfo {
  return new TypeInfo("", undefined, undefined, () => fields);
}

export function union_type(
  fullname: string,
  generics: TypeInfo[],
  construct: Constructor,
  cases: () => FieldInfo[][]): TypeInfo {
  const t: TypeInfo = new TypeInfo(fullname, generics, construct, undefined, () => {
    const caseNames = construct.prototype.cases() as string[];
    return cases().map((fields, i) => new CaseInfo(t, i, caseNames[i], fields))
  });
  return t;
}

export function tuple_type(...generics: TypeInfo[]): TypeInfo {
  return new TypeInfo("System.Tuple`" + generics.length, generics);
}

export function delegate_type(...generics: TypeInfo[]): TypeInfo {
  return new TypeInfo("System.Func`" + generics.length, generics);
}

export function lambda_type(argType: TypeInfo, returnType: TypeInfo): TypeInfo {
  return new TypeInfo("Microsoft.FSharp.Core.FSharpFunc`2", [argType, returnType]);
}

export function option_type(generic: TypeInfo): TypeInfo {
  return new TypeInfo("Microsoft.FSharp.Core.FSharpOption`1", [generic]);
}

export function list_type(generic: TypeInfo): TypeInfo {
  return new TypeInfo("Microsoft.FSharp.Collections.FSharpList`1", [generic]);
}

export function array_type(generic: TypeInfo): TypeInfo {
  return new TypeInfo(generic.fullname + "[]", [generic]);
}

export function enum_type(fullname: string, underlyingType: TypeInfo, enumCases: EnumCase[]): TypeInfo {
  return new TypeInfo(fullname, [underlyingType], undefined, undefined, undefined, enumCases);
}

export const obj_type: TypeInfo = new TypeInfo("System.Object");
export const unit_type: TypeInfo = new TypeInfo("Microsoft.FSharp.Core.Unit");
export const char_type: TypeInfo = new TypeInfo("System.Char");
export const string_type: TypeInfo = new TypeInfo("System.String");
export const bool_type: TypeInfo = new TypeInfo("System.Boolean");
export const int8_type: TypeInfo = new TypeInfo("System.SByte");
export const uint8_type: TypeInfo = new TypeInfo("System.Byte");
export const int16_type: TypeInfo = new TypeInfo("System.Int16");
export const uint16_type: TypeInfo = new TypeInfo("System.UInt16");
export const int32_type: TypeInfo = new TypeInfo("System.Int32");
export const uint32_type: TypeInfo = new TypeInfo("System.UInt32");
export const float32_type: TypeInfo = new TypeInfo("System.Single");
export const float64_type: TypeInfo = new TypeInfo("System.Double");
export const decimal_type: TypeInfo = new TypeInfo("System.Decimal");

export function name(info: FieldInfo | CaseInfo | TypeInfo): string {
  if (Array.isArray(info)) {
    return info[0];
  } else if (info instanceof CaseInfo) {
    return info.name;
  } else {
    const i = info.fullname.lastIndexOf(".");
    return i === -1 ? info.fullname : info.fullname.substr(i + 1);
  }
}

export function fullName(t: TypeInfo): string {
  const gen = t.generics != null && !isArray(t) ? t.generics : [];
  if (gen.length > 0) {
    return t.fullname + "[" + gen.map((x) => fullName(x)).join(",") + "]";
  } else {
    return t.fullname;
  }
}

export function namespace(t: TypeInfo) {
  const i = t.fullname.lastIndexOf(".");
  return i === -1 ? "" : t.fullname.substr(0, i);
}

export function isArray(t: TypeInfo): boolean {
  return t.fullname.endsWith("[]");
}

export function getElementType(t: TypeInfo): TypeInfo | undefined {
  return isArray(t) ? t.generics?.[0] : undefined;
}

export function isGenericType(t: TypeInfo) {
  return t.generics != null && t.generics.length > 0;
}

export function isEnum(t: TypeInfo) {
  return t.enumCases != null && t.enumCases.length > 0;
}

/**
 * This doesn't replace types for fields (records) or cases (unions)
 * but it should be enough for type comparison purposes
 */
export function getGenericTypeDefinition(t: TypeInfo) {
  return t.generics == null ? t : new TypeInfo(t.fullname, t.generics.map(() => obj_type));
}

export function getEnumUnderlyingType(t: TypeInfo) {
  return t.generics?.[0];
}

export function getEnumValues(t: TypeInfo): number[] {
  if (isEnum(t) && t.enumCases != null) {
    return t.enumCases.map((kv) => kv[1]);
  } else {
    throw new Error(`${t.fullname} is not an enum type`);
  }
}

export function getEnumNames(t: TypeInfo): string[] {
  if (isEnum(t) && t.enumCases != null) {
    return t.enumCases.map((kv) => kv[0]);
  } else {
    throw new Error(`${t.fullname} is not an enum type`);
  }
}

function getEnumCase(t: TypeInfo, v: number | string): EnumCase {
  if (t.enumCases != null) {
    if (typeof v === "string") {
      for (const kv of t.enumCases) {
        if (kv[0] === v) {
          return kv;
        }
      }
      throw new Error(`'${v}' was not found in ${t.fullname}`);
    } else {
      for (const kv of t.enumCases) {
        if (kv[1] === v) {
          return kv;
        }
      }
      // .NET returns the number even if it doesn't match any of the cases
      return ["", v];
    }
  } else {
    throw new Error(`${t.fullname} is not an enum type`);
  }
}

export function parseEnum(t: TypeInfo, str: string): number {
  // TODO: better int parsing here, parseInt ceils floats: "4.8" -> 4
  const value = parseInt(str, 10);
  return getEnumCase(t, isNaN(value) ? str : value)[1];
}

export function tryParseEnum(t: TypeInfo, str: string, defValue: FSharpRef<number>): boolean {
  try {
    defValue.contents = parseEnum(t, str);
    return true;
  } catch {
    return false;
  }
}

export function getEnumName(t: TypeInfo, v: number): string {
  return getEnumCase(t, v)[0];
}

export function isEnumDefined(t: TypeInfo, v: string | number): boolean {
  try {
    const kv = getEnumCase(t, v);
    return kv[0] != null && kv[0] !== "";
  } catch {
    // supress error
  }
  return false;
}

// FSharpType

export function getUnionCases(t: TypeInfo): CaseInfo[] {
  if (t.cases != null) {
    return t.cases();
  } else {
    throw new Error(`${t.fullname} is not an F# union type`);
  }
}

export function getRecordElements(t: TypeInfo): FieldInfo[] {
  if (t.fields != null) {
    return t.fields();
  } else {
    throw new Error(`${t.fullname} is not an F# record type`);
  }
}

export function getTupleElements(t: TypeInfo): TypeInfo[] {
  if (isTuple(t) && t.generics != null) {
    return t.generics;
  } else {
    throw new Error(`${t.fullname} is not a tuple type`);
  }
}

export function getFunctionElements(t: TypeInfo): [TypeInfo, TypeInfo] {
  if (isFunction(t) && t.generics != null) {
    const gen = t.generics;
    return [gen[0], gen[1]];
  } else {
    throw new Error(`${t.fullname} is not an F# function type`);
  }
}

export function isUnion(t: any): boolean {
  return t instanceof TypeInfo ? t.cases != null : isUnionLike(t);
}

export function isRecord(t: any): boolean {
  return t instanceof TypeInfo ? t.fields != null : typeof t === "object" && !isUnionLike(t); // TODO: better test
}

export function isTuple(t: TypeInfo): boolean {
  return t.fullname.startsWith("System.Tuple");
}

// In .NET this is false for delegates
export function isFunction(t: TypeInfo): boolean {
  return t.fullname === "Microsoft.FSharp.Core.FSharpFunc`2";
}

// FSharpValue

export function getUnionFields(v: any, t: TypeInfo): [CaseInfo, any[]] {
  const cases = getUnionCases(t);
  const case_ = cases[v.tag];
  if (case_ == null) {
    throw new Error(`Cannot find case ${v.name} in union type`);
  }
  return [case_, v.fields];
}

export function getUnionCaseFields(uci: CaseInfo): FieldInfo[] {
  return uci.fields == null ? [] : uci.fields;
}

export function getRecordFields(v: any): FieldInfo[] {
  return Object.keys(v).map((k) => v[k]);
}

export function getRecordField(v: any, field: FieldInfo): any {
  return v[field[0]];
}

export function getTupleFields(v: any): any[] {
  return v;
}

export function getTupleField(v: any, i: number): any {
  return v[i];
}

export function makeUnion(uci: CaseInfo, values: any[]): any {
  const expectedLength = (uci.fields || []).length;
  if (values.length !== expectedLength) {
    throw new Error(`Expected an array of length ${expectedLength} but got ${values.length}`);
  }
  return uci.declaringType.construct != null
    ? new uci.declaringType.construct(uci.tag, ...values)
    : {};
}

export function makeRecord(t: TypeInfo, values: any[]): any {
  const fields = getRecordElements(t);
  if (fields.length !== values.length) {
    throw new Error(`Expected an array of length ${fields.length} but got ${values.length}`);
  }
  return t.construct != null
    ? new t.construct(...values)
    : makeAnonRecord(fields.reduce((obj, [key, _t], i) => {
      obj[key] = values[i];
      return obj;
    }, {} as any));
}

export function makeTuple(values: any[], _t: TypeInfo): any {
  return values;
}

export function makeGenericType(t: TypeInfo, generics: TypeInfo[]): TypeInfo {
  return new TypeInfo(
    t.fullname,
    generics,
    t.construct,
    t.fields,
    t.cases);
}

export function createInstance(t: TypeInfo, consArgs?: any[]): any {
  // TODO: Check if consArgs length is same as t.construct?
  // (Arg types can still be different)
  if (typeof t.construct === "function") {
    return new t.construct(...(consArgs ?? []));
  } else {
    throw new Error(`Cannot access constructor of ${t.fullname}`);
  }
}

export function getValue(propertyInfo: PropertyInfo, v: any): any {
  return v[propertyInfo[0]];
}

// Fable.Core.Reflection

function assertUnion(x: any) {
  if (!isUnionLike(x)) {
    throw new Error(`Value is not an F# union type`);
  }
}

export function getCaseTag(x: any): number {
  assertUnion(x);
  return x.tag;
}

export function getCaseName(x: any): string {
  assertUnion(x);
  return x.cases()[x.tag];
}

export function getCaseFields(x: any): any[] {
  assertUnion(x);
  return x.fields;
}

type TypeTester =
  | "any"
  | "unknown"
  | "undefined"
  | "function"
  | "boolean"
  | "number"
  | "string"
  | ["tuple", TypeTester[]]
  | ["array", TypeTester | undefined]
  | ["list", TypeTester]
  | ["option", TypeTester]
  | FunctionConstructor

export function typeTest(x: any, typeTester: TypeTester): boolean {
  if (typeof typeTester === "string") {
    if (typeTester === "any") {
      return true;
    } else if (typeTester === "unknown") {
      return false;
    } else {
      return typeof x === typeTester;
    }
  } else if (Array.isArray(typeTester)) {
    switch (typeTester[0]) {
      case "tuple":
        return Array.isArray(x)
          && x.length === typeTester[1].length
          && x.every((x, i) => typeTest(x, typeTester[1][i]));
      case "array":
        return isArrayLike(x)
          && (x.length === 0
            || typeTester[1] == null
            || typeTest(x[0], typeTester[1]));
      case "list":
        return x instanceof List
          && (x.tail == null || typeTest(x.head, typeTester[1]));
      case "option":
        return x == null || typeTest(getOptionValue(x), typeTester[1]);
      default:
        return false
    }
  } else {
    return x instanceof typeTester;
  }
}