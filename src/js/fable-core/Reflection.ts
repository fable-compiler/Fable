import { compareArraysWith, equalArraysWith } from "./Util";

export interface CaseInfoWithFields {
  name: string;
  fields: TypeInfo[];
}

export type FieldInfo = [string, TypeInfo];
export type CaseInfo = string | CaseInfoWithFields;

export interface TypeInfo {
  fullname: string;
  generics?: TypeInfo[];
  fields?: FieldInfo[];
  cases?: CaseInfo[];
}

export function equals(t1: TypeInfo, t2: TypeInfo): boolean {
  return t1.fullname === t2.fullname
    && equalArraysWith(t1.generics, t2.generics, equals);
}

// System.Type is not comparable in .NET, but let's implement this
// in case users want to create a dictionary with types as keys
export function compare(t1: TypeInfo, t2: TypeInfo): number {
  if (t1.fullname !== t2.fullname) {
    return t1.fullname < t2.fullname ? -1 : 1;
  } else {
    return compareArraysWith(t1.generics, t2.generics, compare);
  }
}

export function type(fullname: string, generics?: TypeInfo[]): TypeInfo {
  return {
    fullname,
    generics,
  };
}

export function record(fullname: string, generics: TypeInfo[], ...fields: FieldInfo[]): TypeInfo {
  return {
    fullname,
    generics,
    fields,
  };
}

export function union(fullname: string, generics: TypeInfo[], ...cases: CaseInfo[]): TypeInfo {
  return {
    fullname,
    generics,
    cases,
  };
}

export function tuple(...generics: TypeInfo[]): TypeInfo {
  return {
    fullname: "System.Tuple`" + generics.length,
    generics,
  };
}

export function delegate(...generics: TypeInfo[]): TypeInfo {
  return {
    fullname: "System.Func`" + generics.length,
    generics,
  };
}

export function lambda(argType: TypeInfo, returnType: TypeInfo): TypeInfo {
  return {
    fullname: "Microsoft.FSharp.Core.FSharpFunc`2",
    generics: [argType, returnType],
  };
}

export function option(generic: TypeInfo): TypeInfo {
  return {
    fullname: "Microsoft.FSharp.Core.FSharpOption`1",
    generics: [generic],
  };
}

export function list(generic: TypeInfo): TypeInfo {
  return {
    fullname: "Microsoft.FSharp.Collections.FSharpList`1",
    generics: [generic],
  };
}

export function array(generic: TypeInfo): TypeInfo {
  return {
    fullname: generic.fullname,
    generics: [generic],
  };
}

export const obj: TypeInfo = { fullname: "System.Object" };
export const unit: TypeInfo = { fullname: "Microsoft.FSharp.Core.Unit" };
export const char: TypeInfo = { fullname: "System.Char" };
export const string: TypeInfo = { fullname: "System.String" };
export const bool: TypeInfo = { fullname: "System.Boolean" };
export const int8: TypeInfo = { fullname: "System.SByte" };
export const uint8: TypeInfo = { fullname: "System.Byte" };
export const int16: TypeInfo = { fullname: "System.Int16" };
export const uint16: TypeInfo = { fullname: "System.UInt16" };
export const int32: TypeInfo = { fullname: "System.Int32" };
export const uint32: TypeInfo = { fullname: "System.UInt32" };
export const float32: TypeInfo = { fullname: "System.Single" };
export const float64: TypeInfo = { fullname: "System.Double" };
export const decimal: TypeInfo = { fullname: "System.Decimal" };

export function name(i: FieldInfo | CaseInfo | TypeInfo): string {
  if (typeof i === "string") {
    return i;
  } else if (Array.isArray(i)) {
    return i[0];
  } else {
    const fullname = (i as TypeInfo).fullname;
    if (fullname != null) {
      const i = fullname.lastIndexOf(".");
      return i === -1 ? fullname : fullname.substr(i + 1);
    } else {
      return (i as CaseInfoWithFields).name;
    }
  }
}

export function namespace(t: TypeInfo) {
  const i = t.fullname.lastIndexOf(".");
  return i === -1 ? "" : t.fullname.substr(0, i);
}

export function isGenericType(t: TypeInfo) {
  return t.generics != null && t.generics.length > 0;
}

// FSharpType

export function getUnionCases(t: TypeInfo): CaseInfo[] {
  if (Array.isArray(t.cases)) {
    return t.cases;
  } else {
    throw new Error(`${t.fullname} is not an F# union type`);
  }
}

export function getRecordElements(t: TypeInfo): FieldInfo[] {
  if (Array.isArray(t.fields)) {
    return t.fields;
  } else {
    throw new Error(`${t.fullname} is not an F# record type`);
  }
}

export function getTupleElements(t: TypeInfo): TypeInfo[] {
  if (isTuple(t)) {
    return t.generics;
  } else {
    throw new Error(`${t.fullname} is not a tuple type`);
  }
}

export function getFunctionElements(t: TypeInfo): [TypeInfo, TypeInfo] {
  if (isFunction(t)) {
    return [t.generics[0], t.generics[1]];
  } else {
    throw new Error(`${t.fullname} is not an F# function type`);
  }
}

export function isUnion(t: TypeInfo): boolean {
  return Array.isArray(t.cases);
}

export function isRecord(t: TypeInfo): boolean {
  return Array.isArray(t.fields);
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
  function fail(caseName: string) {
    throw new Error(`Cannot find case ${caseName} in union type`);
  }
  const cases = getUnionCases(t);
  if (typeof v === "string") {
    if ((cases as string[]).indexOf(v) === -1) {
      fail(v);
    }
    return [v, []];
  } else {
    const name = v[0];
    const case_ = (cases as CaseInfoWithFields[]).find((x) => x.name === name);
    if (case_ == null) {
      fail(name);
    }
    return [case_, v.slice(1)];
  }
}

export function getUnionCaseFields(i: CaseInfo): FieldInfo[] {
  return typeof i === "string"
    ? [] : i.fields.map((t, i) => ["Data" + i, t] as FieldInfo);
}

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
  return typeof uci === "string" ? uci : [uci.name].concat(values);
}

export function makeRecord(t: TypeInfo, values: any[]): any {
  const o: any = {};
  const fields = getRecordElements(t);
  if (fields.length !== values.length) {
    throw new Error(`Expected an array of length ${fields.length} but got ${values.length}`);
  }
  for (let i = 0; i < fields.length; i++) {
    o[fields[i][0]] = values[i];
  }
  return o;
}

export function makeTuple(values: any[], t: TypeInfo): any {
  return values;
}
