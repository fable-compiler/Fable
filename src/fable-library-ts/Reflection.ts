import { FSharpRef, Record, Union } from "./Types.js";
import { combineHashCodes, equalArraysWith, IEquatable, stringHash } from "./Util.js";
import Decimal from "./Decimal.js";

export type FieldInfo = [string, TypeInfo];
export type PropertyInfo = FieldInfo;
export type ParameterInfo = FieldInfo;

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

export class MethodInfo {
  constructor(
    public name: string,
    public parameters: ParameterInfo[],
    public returnType: TypeInfo,
  ) {
  }
}

export class TypeInfo implements IEquatable<TypeInfo> {
  constructor(
    public fullname: string,
    public generics?: TypeInfo[],
    public construct?: Constructor,
    public parent?: TypeInfo,
    public fields?: () => FieldInfo[],
    public cases?: () => CaseInfo[],
    public enumCases?: EnumCase[]) {
  }
  public toString() {
    return fullName(this);
  }
  public GetHashCode() {
    return getHashCode(this);
  }
  public Equals(other: TypeInfo) {
    return equals(this, other);
  }
}

export class GenericParameter extends TypeInfo {
    constructor(name: string) {
      super(name);
    }
}

export function getGenerics(t: TypeInfo): TypeInfo[] {
  return t.generics != null ? t.generics : [];
}

export function getHashCode(t: TypeInfo) {
  const fullnameHash = stringHash(t.fullname);
  const genHashes: number[] = getGenerics(t).map(getHashCode);
  return combineHashCodes([fullnameHash, ...genHashes]);
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

export function class_type(
  fullname: string,
  generics?: TypeInfo[],
  construct?: Constructor,
  parent?: TypeInfo): TypeInfo {
  return new TypeInfo(fullname, generics, construct, parent);
}

export function record_type(
  fullname: string,
  generics: TypeInfo[],
  construct: Constructor,
  fields: () => FieldInfo[]): TypeInfo {
  return new TypeInfo(fullname, generics, construct, undefined, fields);
}

export function anonRecord_type(...fields: FieldInfo[]): TypeInfo {
  return new TypeInfo("", undefined, undefined, undefined, () => fields);
}

export function union_type(
  fullname: string,
  generics: TypeInfo[],
  construct: Constructor,
  cases: () => FieldInfo[][]): TypeInfo {
  const t: TypeInfo = new TypeInfo(fullname, generics, construct, undefined, undefined, () => {
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
  return new TypeInfo("[]", [generic]);
}

export function enum_type(fullname: string, underlyingType: TypeInfo, enumCases: EnumCase[]): TypeInfo {
  return new TypeInfo(fullname, [underlyingType], undefined, undefined, undefined, undefined, enumCases);
}

export function measure_type(fullname: string): TypeInfo {
  return new TypeInfo(fullname);
}

export function generic_type(name: string): TypeInfo {
  return new GenericParameter(name);
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
export const int64_type: TypeInfo = new TypeInfo("System.Int64");
export const uint64_type: TypeInfo = new TypeInfo("System.UInt64");
export const int128_type: TypeInfo = new TypeInfo("System.Int128");
export const uint128_type: TypeInfo = new TypeInfo("System.UInt128");
export const nativeint_type: TypeInfo = new TypeInfo("System.IntPtr");
export const unativeint_type: TypeInfo = new TypeInfo("System.UIntPtr");
export const float16_type: TypeInfo = new TypeInfo("System.Half");
export const float32_type: TypeInfo = new TypeInfo("System.Single");
export const float64_type: TypeInfo = new TypeInfo("System.Double");
export const decimal_type: TypeInfo = new TypeInfo("System.Decimal");
export const bigint_type: TypeInfo = new TypeInfo("System.Numerics.BigInteger");

export function name(info: FieldInfo | TypeInfo | CaseInfo | MethodInfo): string {
  if (Array.isArray(info)) {
    return info[0];
  } else if (info instanceof TypeInfo) {
    const elemType = getElementType(info);
    if (elemType != null) {
      return name(elemType) + "[]";
    } else {
      const i = info.fullname.lastIndexOf(".");
      return i === -1 ? info.fullname : info.fullname.substr(i + 1);
    }
  } else {
    return info.name;
  }
}

export function fullName(t: TypeInfo): string {
  const elemType = getElementType(t);
  if (elemType != null) {
    return fullName(elemType) + "[]";
  } else if (t.generics == null || t.generics.length === 0) {
    return t.fullname
  } else {
    return t.fullname + "[" + t.generics.map((x) => fullName(x)).join(",") + "]";
  }
}

export function namespace(t: TypeInfo): string {
  const elemType = getElementType(t);
  if (elemType != null) {
    return namespace(elemType);
  } else {
    const i = t.fullname.lastIndexOf(".");
    return i === -1 ? "" : t.fullname.substr(0, i);
  }
}

export function isArray(t: TypeInfo): boolean {
  return getElementType(t) != null;
}

export function getElementType(t: TypeInfo): TypeInfo | undefined {
  return t.fullname === "[]" && t.generics?.length === 1 ? t.generics[0] : undefined;
}

export function isGenericType(t: TypeInfo) {
  return t.generics != null && t.generics.length > 0;
}

export function isGenericParameter(t: TypeInfo) {
  return t instanceof GenericParameter;
}

export function isEnum(t: TypeInfo) {
  return t.enumCases != null && t.enumCases.length > 0;
}

export function isSubclassOf(t1: TypeInfo, t2: TypeInfo): boolean {
  return (t2.fullname === obj_type.fullname) || (t1.parent != null && (t1.parent.Equals(t2) || isSubclassOf(t1.parent, t2)));
}

function isErasedToNumber(t: TypeInfo) {
  return isEnum(t) || [
    int8_type.fullname,
    uint8_type.fullname,
    int16_type.fullname,
    uint16_type.fullname,
    int32_type.fullname,
    uint32_type.fullname,
    float16_type.fullname,
    float32_type.fullname,
    float64_type.fullname,
  ].includes(t.fullname);
}

function isErasedToBigInt(t: TypeInfo) {
  return isEnum(t) || [
    int64_type.fullname,
    uint64_type.fullname,
    int128_type.fullname,
    uint128_type.fullname,
    nativeint_type.fullname,
    unativeint_type.fullname,
    bigint_type.fullname,
  ].includes(t.fullname);
}

export function isInstanceOfType(t: TypeInfo, o: any) {
  if (t.fullname === obj_type.fullname)
    return true;
  switch (typeof o) {
    case "boolean":
      return t.fullname === bool_type.fullname;
    case "string":
      return t.fullname === string_type.fullname;
    case "function":
      return isFunction(t);
    case "number":
      return isErasedToNumber(t);
    case "bigint":
      return isErasedToBigInt(t);
    default:
      return t.construct != null && o instanceof t.construct;
  }
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
  return t instanceof TypeInfo ? t.cases != null : t instanceof Union;
}

export function isRecord(t: any): boolean {
  return t instanceof TypeInfo ? t.fields != null : t instanceof Record;
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

// This is used as replacement of `FSharpValue.GetRecordFields`
// For `FSharpTypes.GetRecordFields` see `getRecordElements`
// Object.keys returns keys in the order they were added to the object
export function getRecordFields(v: any): any[] {
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
  const construct = uci.declaringType.construct;
  if (construct == null) {
    return {};
  }
  const isSingleCase = uci.declaringType.cases ? uci.declaringType.cases().length == 1 : false;
  if (isSingleCase) {
    return new construct(...values);
  }
  else {
    return new construct(uci.tag, values);
  }
}

export function makeRecord(t: TypeInfo, values: any[]): any {
  const fields = getRecordElements(t);
  if (fields.length !== values.length) {
    throw new Error(`Expected an array of length ${fields.length} but got ${values.length}`);
  }
  return t.construct != null
    ? new t.construct(...values)
    : fields.reduce((obj, [key, _t], i) => {
      obj[key] = values[i];
      return obj;
    }, {} as any);
}

export function makeTuple(values: any[], _t: TypeInfo): any {
  return values;
}

export function makeGenericType(t: TypeInfo, generics: TypeInfo[]): TypeInfo {
  return new TypeInfo(
    t.fullname,
    generics,
    t.construct,
    t.parent,
    t.fields,
    t.cases);
}

export function createInstance(t: TypeInfo, consArgs?: any[]): any {
  // TODO: Check if consArgs length is same as t.construct?
  // (Arg types can still be different)
  if (typeof t.construct === "function") {
    return new t.construct(...(consArgs ?? []));
  } else if (isErasedToNumber(t)) {
    return 0;
  } else if (isErasedToBigInt(t)) {
    return 0n;
  } else {
    switch (t.fullname) {
      case obj_type.fullname:
        return {};
      case bool_type.fullname:
        return false;
      case decimal_type.fullname:
        return new Decimal(0);
      case char_type.fullname:
        // Even though char is a value type, it's erased to string, and Unchecked.defaultof<char> is null
        return null;
      default:
        throw new Error(`Cannot access constructor of ${t.fullname}`);
    }
  }
}

export function getValue(propertyInfo: PropertyInfo, v: any): any {
  return v[propertyInfo[0]];
}

// Fable.Core.Reflection

function assertUnion(x: any) {
  if (!(x instanceof Union)) {
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
