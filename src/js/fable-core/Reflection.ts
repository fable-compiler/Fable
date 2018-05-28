export interface CaseInfo {
  name: string;
  fields: TypeInfo[];
}

export type FieldInfo = [string, TypeInfo];

export interface TypeInfo {
  fullname: string;
  generics?: TypeInfo[];
  fields?: FieldInfo[];
  cases?: CaseInfo[];
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

export function union(fullname: string, generics: TypeInfo[], ...cases: Array<string | CaseInfo>): TypeInfo {
  return {
    fullname,
    generics,
    cases: cases.map((x) => typeof x === "string" ? { name: x, fields: [] } : x),
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

// This is different to how .NET reflection deals with arrays
export function array(generic: TypeInfo): TypeInfo {
  return {
    fullname: "System.Array",
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
