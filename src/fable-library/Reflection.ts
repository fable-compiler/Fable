import { Record, Union } from "./Types";
import { compareArraysWith, equalArraysWith, stringHash } from "./Util";

// tslint:disable: max-line-length

export class NParameterInfo {
  constructor(
    public Name: string,
    public ParameterType: NTypeInfo,
  ) {}

  public toString() {
    return this.Name + " : " + this.ParameterType.toString();
  }
}

export class NMemberInfo {
  constructor(
      public DeclaringType: NTypeInfo,
      public Name: string,
      private attributes: CustomAttribute[],
  ) {}

  public get_Name() { return this.Name; }

  public get_DeclaringType() { return this.DeclaringType; }

  public GetCustomAttributes(a: (boolean|NTypeInfo), b?: boolean) {
    if (typeof a === "boolean") {
      return this.attributes;
    } else if (a.fullname) {
      return this.attributes.filter((att) => att.AttributeType === a.fullname).map((att) => att.AttributeValue);
    } else {
      return this.attributes.map((att) => att.AttributeValue);
    }
  }
}

export class NMethodBase extends NMemberInfo {
  constructor(
    DeclaringType: NTypeInfo,
    Name: string,
    public Parameters: NParameterInfo[],
    public IsStatic: boolean,
    attributes: CustomAttribute[],
  ) {
    super(DeclaringType, Name, attributes);
  }

  public GetParameters() { return this.Parameters; }

  public get_IsStatic() { return this.IsStatic; }

  public get_IsGenericMethod() {  return false; }

  public GetGenericMethodDefinition() { return this; }

}

export class NMethodInfo extends NMethodBase {
  constructor(
    DeclaringType: NTypeInfo,
    Name: string,
    Parameters: NParameterInfo[],
    public ReturnType: NTypeInfo,
    IsStatic: boolean,
    private invoke: (...args: any[]) => any,
    attributes: CustomAttribute[],
  ) {
    super(DeclaringType, Name, Parameters, IsStatic, attributes);
  }

  public get_ReturnType() { return this.ReturnType; }

  public get_ReturnParameter() {
    return new NParameterInfo("Return", this.ReturnType);
  }

  public toString() {
    const args = this.Parameters.map((p) => p.toString()).join(", ");

    let attPrefix = "";
    const atts = this.GetCustomAttributes(true);
    if (atts.length > 0) {
      attPrefix = "[<" + atts.map((a) => a.toString()).join("; ") + ">] ";
    }
    let prefix = "member ";
    if (this.IsStatic) { prefix = "static " + prefix; }

    return attPrefix + prefix + this.Name + "(" + args + ") : " + this.ReturnType.toString();
  }

  public Invoke(target: any, ...args: any[]) {
    if (!this.IsStatic) {
      const a = [target, ...args];
      return this.invoke.apply(null, [target, ...args]);
    } else {
      return this.invoke.apply(null, args);
    }
  }
}

export class NConstructorInfo extends NMethodBase {
  constructor(
    DeclaringType: NTypeInfo,
    Name: string,
    Parameters: NParameterInfo[],
    IsStatic: boolean,
    private invoke: (...args: any[]) => any,
    attributes: CustomAttribute[],
  ) {
    super(DeclaringType, Name, Parameters, IsStatic, attributes);
  }

  public toString() {
    const args = this.Parameters.map((p) => p.toString()).join(", ");

    let attPrefix = "";
    const atts = this.GetCustomAttributes(true);
    if (atts.length > 0) {
      attPrefix = "[<" + atts.map((a) => a.toString()).join("; ") + ">] ";
    }

    return attPrefix + "new(" + args + ")";
  }

  public Invoke(...args: any[]) {
      return this.invoke.apply(null, args);
  }
}

export class NFieldInfo extends NMemberInfo {
  constructor(
    DeclaringType: NTypeInfo,
    Name: string,
    public Type: NTypeInfo,
    public IsStatic: boolean,
    attributes: CustomAttribute[],
  ) {
    super(DeclaringType, Name, attributes);
  }
  public get_FieldType() { return this.Type; }
  public get_IsStatic() { return this.IsStatic; }

  public toString() {
    const typ = this.Type.toString();
    let prefix = "val ";
    if (this.IsStatic) { prefix = "static " + prefix; }

    let attPrefix = "";
    const atts = this.GetCustomAttributes(true);
    if (atts.length > 0) {
      attPrefix = "[<" + atts.map((a) => a.toString()).join("; ") + ">] ";
    }

    return attPrefix + prefix + this.Name + " : " + typ;
  }
}

export class NPropertyInfo extends NMemberInfo {
  constructor(
    DeclaringType: NTypeInfo,
    Name: string,
    public Type: NTypeInfo,
    public IsStatic: boolean,
    public IsFSharp: boolean,
    attributes: CustomAttribute[],
  ) {
    super(DeclaringType, Name, attributes);
  }
  public get_PropertyType() { return this.Type; }
  public get_IsStatic() { return this.IsStatic; }
  public get_IsFSharp() { return this.IsFSharp; }

  public get_GetMethod() {
    const getterName = "get_" + this.Name;
    const mems = this.DeclaringType.GetAllMembers();
    const idx = mems.findIndex((m) => m instanceof NMethodInfo && m.Name === getterName);
    if (idx >= 0) { return mems[idx] as NMethodInfo; } else { return null; }
  }

  public get_SetMethod() {
    const getterName = "set_" + this.Name;
    const mems = this.DeclaringType.GetAllMembers();
    const idx = mems.findIndex((m) => m instanceof NMethodInfo && m.Name === getterName);
    if (idx >= 0) { return mems[idx] as NMethodInfo; } else { return null; }
  }

  public GetValue(target: any, ...index: any[]) {
    const g = this.get_GetMethod();
    return g.Invoke(target, index);
  }

  public SetValue(target: any, value: any, ...index: any[]) {
    const s = this.get_SetMethod();
    s.Invoke(target, [...index, value]);
  }

  public toString() {
    const g = this.get_GetMethod();
    const s = this.get_SetMethod();
    let typ = this.Type.toString();
    if (g && g.Parameters.length > 0) {
      const prefix = g.Parameters.map((p) => p.ParameterType.toString()).join(" * ");
      typ = prefix + " -> " + typ;
    } else if (s && s.Parameters.length > 0) {
      const prefix = s.Parameters.slice(0, s.Parameters.length - 1).map((p) => p.ParameterType.toString()).join(" * ");
      typ = prefix + " -> " + typ;
    }

    let suffix = "";
    if (g && s) {
      suffix = " with get, set";
    } else if (g) {
      suffix = " with get";
    } else if (s) {
      suffix = " with set";
    }

    let prefix = "member ";
    if (this.IsStatic) { prefix = "static " + prefix; }

    let attPrefix = "";
    const atts = this.GetCustomAttributes(true);
    if (atts.length > 0) {
      attPrefix = "[<" + atts.map((a) => a.toString()).join("; ") + ">] ";
    }

    return attPrefix + prefix + this.Name + " : " + typ + suffix;
  }
}

export class NUnionCaseInfo extends NMemberInfo {
  public constructor(
    DeclaringType: NTypeInfo,
    public Tag: number,
    Name: string,
    Attributes: CustomAttribute[],
    public Fields: Array<[string, NTypeInfo]>,
    public Invoke: (...args: any[]) => any,
  ) { super(DeclaringType, Name, Attributes); }

  public GetFields() {
    return this.Fields.map((nt) => new NPropertyInfo(this.DeclaringType, nt[0], nt[1], false, true, []));
  }

  public get_Tag() {
    return this.Tag;
  }
}

export class NTypeInfo {
  public static getParameter(i: string) {
    if (NTypeInfo.parameterCache[i]) {
      return NTypeInfo.parameterCache[i];
    } else {
      const p = new NTypeInfo(i, 0, true, [], (_s) => []);
      this.parameterCache[i] = p;
      return p;
    }
  }

  public static Simple(name: string) {
    return new NTypeInfo(name, 0, false, [], ((_s) => []));
  }

  private static parameterCache: {[i: string]: NTypeInfo} = {};
  public generics: NTypeInfo[] = null;

  private instantiations: {[i: string]: NTypeInfo} = {};
  private mems: NMemberInfo[] = null;
  private genericMap: {[name: string]: number} = {};
  private declaration: NTypeInfo = null;

  constructor(
    public fullname: string,
    public genericCount: number,
    public isGenericParameter: boolean,
    _generics: NTypeInfo[],
    public members: (self: NTypeInfo) => NMemberInfo[],
    decl?: NTypeInfo) {
      const g = _generics.filter((t) => !t.isGenericParameter);
      if (g.length === genericCount) {
        this.generics = g;
      } else {
        _generics.forEach((g, i) => {
          this.genericMap[g.get_Name()] = i;
        });
        this.generics = [];
      }
      this.declaration = decl || null;
    }

    public ResolveGeneric(name: string) {
      if (this.genericMap[name]) {
        return this.generics[this.genericMap[name]];
      } else {
        return null;
      }
    }

    public GetGenericTypeDefinition() {
      if (this.genericCount === 0 || this.generics.length === 0) {
        return this;
      } else {
        if (this.declaration) {
          return this.declaration;
        } else {
          return new NTypeInfo(this.fullname, this.genericCount, this.isGenericParameter, [], this.members);
        }
      }
    }

  public MakeGenericType(args: NTypeInfo[]) {
    if (args.length !== this.genericCount) { throw new Error("invalid generic argument count "); }

    const key = args.map((t) => t.toString()).join(", ");
    if (this.instantiations[key]) {
      return this.instantiations[key];
    } else {
      const res = new NTypeInfo(this.fullname, this.genericCount, this.isGenericParameter, args, this.members, this);
      this.instantiations[key] = res;
      return res;
    }
  }

  public get_FullName() {
    return this.fullname;
  }
  public get_Namespace() {
    const i = this.fullname.lastIndexOf(".");
    return i === -1 ? "" : this.fullname.substr(0, i);
  }
  public get_Name() {
    const i = this.fullname.lastIndexOf(".");
    return i === -1 ? this.fullname : this.fullname.substr(i + 1);
  }
  public get_IsArray() {
    return this.fullname.endsWith("[]");
  }

  public GetElementType(): NTypeInfo {
    return this.get_IsArray() ? this.generics[0] : null;
  }

  public get_IsGenericType() {
    return this.genericCount > 0;
  }
  public get_IsGenericTypeDefinition() {
    return this.genericCount > 0 && !this.generics;
  }

  public GetGenericArguments() {
    if (this.genericCount > 0) {
      if (this.generics) {
        return this.generics;
      } else {
        return Array(this.genericCount).map((_, i) => NTypeInfo.getParameter(i.toString()));
      }
    } else {
      return [];
    }
  }

  public get_GenericTypeArguments() {
    return this.GetGenericArguments();
  }

  public GetAllMembers() {
    if (!this.mems) {
      if (this.members) {
        this.mems = this.members(this);
      } else {
        this.mems = [];
      }
    }
    return this.mems;
  }

  public GetMembers() {
    const m = this.GetAllMembers();
    return m.filter((m) => !(m instanceof NUnionCaseInfo));
  }

  public GetProperties() {
    const m = this.GetAllMembers();
    return m.filter((m) => m instanceof NPropertyInfo) as NPropertyInfo[];
  }

  public GetMethods() {
    const m = this.GetAllMembers();
    return m.filter((m) => m instanceof NMethodInfo) as NMethodInfo[];
  }

  public GetProperty(name: string) {
      const m = this.GetAllMembers();
      const prop = m.find((m) => m instanceof NPropertyInfo && m.Name === name) as NPropertyInfo;
      return prop;
  }
  public GetMethod(name: string) {
      const m = this.GetAllMembers();
      const meth = m.find((m) => m instanceof NMethodInfo && m.Name === name) as NMethodInfo;
      return meth;
  }
  public toFullString(): string {
    if (this.genericCount > 0) {
      const args = this.generics.map((t) => t.toFullString()).join(", ");
      return this.fullname + "<" + args + ">";
    } else {
      return this.fullname;
    }
  }

  public toString(): string {
    if (this.genericCount > 0) {
      const args = this.generics.map((t) => t.toString()).join(", ");
      return this.fullname + "<" + args + ">";
    } else {
      return this.fullname;
    }
  }
  public toLongString() {
    const members =
      this
      .GetMembers()
      .filter((m) => !(m instanceof NMethodInfo) || !(m.Name.startsWith("get_") || m.Name.startsWith("set_")))
      .map((m) => "    " + m.toString()).join("\n");
    return "type " + this.fullname + "=\n" + members;
  }

  public GetHashCode() {
    return stringHash(this.toFullString());
  }

  public Equals(other: NTypeInfo) {
    return equals(this, other);
  }
  public CompareTo(other: NTypeInfo) {
    return compare(this, other);
  }
}

export enum MemberKind {
  Property = 0,
  Field = 1,
  FSharpField = 2,
  Constructor = 3,
  Method = 4,
  UnionCase = 5,
}

export type ParameterInfo = [string, NTypeInfo];

export type FieldInfo = [string, NTypeInfo, boolean, MemberKind, ParameterInfo[], CustomAttribute[], (...args: any) => any];

export interface CustomAttribute {
  AttributeType: string;
  AttributeValue: any;
}

// export type Constructor = new(...args: any[]) => any;

// export class CaseInfo {
//   constructor(public declaringType: TypeInfo,
//               public tag: number,
//               public name: string,
//               public fields?: TypeInfo[]) {
//   }
// }

const typeCache: { [fullname: string]: NTypeInfo } = {};

function mkParameterInfo(info: ParameterInfo) {
  return new NParameterInfo(info[0], mkNTypeInfo(info[1]));
}

function mkNMemberInfo(self: NTypeInfo, info: FieldInfo): NMemberInfo {
  switch (info[3]) {
    case MemberKind.Method:
      return new NMethodInfo(
        self, info[0], info[4].map((a) => mkParameterInfo(a)),
        mkNTypeInfo(info[1]), info[2], info[6], info[5],
      );
    case MemberKind.Property:
      return new NPropertyInfo(self, info[0], mkNTypeInfo(info[1]), info[2], false, info[5]);
    case MemberKind.FSharpField:
      return new NPropertyInfo(self, info[0], mkNTypeInfo(info[1]), info[2], true, info[5]);
    case MemberKind.Constructor:
      return new NConstructorInfo(
        self, info[0], info[4].map((a) => mkParameterInfo(a)),
        info[2], info[6], info[5],
      );
    case MemberKind.Field:
        return new NFieldInfo(self, info[0], mkNTypeInfo(info[1]), info[2], info[5]);
    case MemberKind.UnionCase:
        const tag = (info[2] as unknown) as number;
        const arr = info[4].map((v) => [v[0], mkNTypeInfo(v[1])]) as Array<[string, NTypeInfo]>;
        return new NUnionCaseInfo(self, tag, info[0], info[5], arr, info[6]);
    default:
      return null;
  }
}
function mkNMemberInfos(fields: () => FieldInfo[]): (self: NTypeInfo) => NMemberInfo[] {
  if (fields) {
    return (self: NTypeInfo) => fields().map((f) => mkNMemberInfo(self, f)).filter((f) => f !== null);
  } else {
    return (_self: NTypeInfo) => [];
  }
}

function mkNTypeInfo(info: NTypeInfo): NTypeInfo {
  return info;
  // if (typeCache[info.fullname]) {
  //   return typeCache[info.fullname];
  // } else {
  //   const gen = info.generics || [];
  //   const fields = info.fields || (() => []);

  //   const res = new NTypeInfo(info.fullname, gen.length, false, gen.map((a) => mkNTypeInfo(a)), mkNMemberInfos(fields));
  //   typeCache[info.fullname] = res;
  //   return res;
  // }
}

export function declareType(fullname: string, generics: NTypeInfo[], members: () => FieldInfo[]): NTypeInfo {
  let gen: NTypeInfo = null;
  if (typeCache[fullname]) {
    gen = typeCache[fullname];
  } else {
    gen = new NTypeInfo(fullname, generics.length, false, [], mkNMemberInfos(members));
    typeCache[fullname] = gen;
  }

  if (generics.length > 0 && generics.length === gen.genericCount) {
    return gen.MakeGenericType(generics);
  } else {
    return gen;
  }
}

export function getGenericParamter(name: string) {
  return NTypeInfo.getParameter(name);
}

// export class TypeInfo {
//   public NewInfo: NTypeInfo;
//   constructor(public fullname: string,
//               public generics?: TypeInfo[],
//               public constructor?: Constructor,
//               public fields?: () => FieldInfo[],
//               public cases?: () => CaseInfo[]) {
//     this.NewInfo = mkNTypeInfo(this);
//   }
//   public toString() {
//     return fullName(this);
//   }
//   public Equals(other: TypeInfo) {
//     return equals(this, other);
//   }
//   public CompareTo(other: TypeInfo) {
//     return compare(this, other);
//   }
// }

export function getGenerics(t: NTypeInfo): NTypeInfo[] {
  return t.generics != null ? t.generics : [];
}

export function equals(t1: NTypeInfo, t2: NTypeInfo): boolean {
  return t1.fullname === t2.fullname
    && equalArraysWith(getGenerics(t1), getGenerics(t2), equals);
}

// System.Type is not comparable in .NET, but let's implement this
// in case users want to create a dictionary with types as keys
export function compare(t1: NTypeInfo, t2: NTypeInfo): number {
  if (t1.fullname !== t2.fullname) {
    return t1.fullname < t2.fullname ? -1 : 1;
  } else {
    return compareArraysWith(getGenerics(t1), getGenerics(t2), compare);
  }
}

export function type(fullname: string, generics?: NTypeInfo[], fields?: () => FieldInfo[]): NTypeInfo {
  const gen = generics || [];
  const f: () => FieldInfo[] = fields || (() => []);
  return new NTypeInfo(fullname, gen.length, false, gen, mkNMemberInfos(f));
}

export function tuple(...generics: NTypeInfo[]): NTypeInfo {
  return new NTypeInfo("System.Tuple`" + generics.length, generics.length, false, generics, (_s) => []);
}

export function delegate(...generics: NTypeInfo[]): NTypeInfo {
  return new NTypeInfo("System.Func`" + generics.length, generics.length, false, generics, (_s) => []);
}

export function lambda(argType: NTypeInfo, returnType: NTypeInfo): NTypeInfo {
  return new NTypeInfo("Microsoft.FSharp.Core.FSharpFunc`2", 2, false, [argType, returnType], (_s) => []);
}

export function option(generic: NTypeInfo): NTypeInfo {
  return new NTypeInfo("Microsoft.FSharp.Core.FSharpOption`1", 1, false, [generic], (s) =>
    [
      new NUnionCaseInfo(s, 0, "None", [], [], ((_) => null)),
      new NUnionCaseInfo(s, 1, "Some", [], [["Value", generic]], ((args) => args[0])),
    ]
  );
}

export function list(generic: NTypeInfo): NTypeInfo {
  return new NTypeInfo("Microsoft.FSharp.Collections.FSharpList`1", 1, false, [generic], (_s) => []);
}

export function array(generic: NTypeInfo): NTypeInfo {
  return new NTypeInfo(generic.fullname + "[]", 1, false, [generic], (_s) => []);
}

export const obj: NTypeInfo = NTypeInfo.Simple("System.Object");
export const unit: NTypeInfo = NTypeInfo.Simple("Microsoft.FSharp.Core.Unit");
export const char: NTypeInfo = NTypeInfo.Simple("System.Char");
export const string: NTypeInfo = NTypeInfo.Simple("System.String");
export const bool: NTypeInfo = NTypeInfo.Simple("System.Boolean");
export const int8: NTypeInfo = NTypeInfo.Simple("System.SByte");
export const uint8: NTypeInfo = NTypeInfo.Simple("System.Byte");
export const int16: NTypeInfo = NTypeInfo.Simple("System.Int16");
export const uint16: NTypeInfo = NTypeInfo.Simple("System.UInt16");
export const int32: NTypeInfo = NTypeInfo.Simple("System.Int32");
export const uint32: NTypeInfo = NTypeInfo.Simple("System.UInt32");
export const float32: NTypeInfo = NTypeInfo.Simple("System.Single");
export const float64: NTypeInfo = NTypeInfo.Simple("System.Double");
export const decimal: NTypeInfo = NTypeInfo.Simple("System.Decimal");

// export function name(info: FieldInfo | CaseInfo | TypeInfo): string {
//   if (Array.isArray(info)) {
//     return info[0];
//   } else if (info instanceof CaseInfo) {
//     return info.name;
//   } else {
//     const i = info.fullname.lastIndexOf(".");
//     return i === -1 ? info.fullname : info.fullname.substr(i + 1);
//   }
// }

// export function fullName(t: TypeInfo): string {
//   const gen = t.generics != null && !isArray(t) ? t.generics : [];
//   if (gen.length > 0) {
//     return t.fullname + "[" + gen.map((x) => fullName(x)).join(",") + "]";
//   } else {
//     return t.fullname;
//   }
// }

// export function namespace(t: TypeInfo) {
//   const i = t.fullname.lastIndexOf(".");
//   return i === -1 ? "" : t.fullname.substr(0, i);
// }

// export function isArray(t: TypeInfo): boolean {
//   return t.fullname.endsWith("[]");
// }

// export function getElementType(t: TypeInfo): TypeInfo {
//   return isArray(t) ? t.generics[0] : null;
// }

// export function isGenericType(t: TypeInfo) {
//   return t.generics != null && t.generics.length > 0;
// }

// /**
//  * This doesn't replace types for fields (records) or cases (unions)
//  * but it should be enough for type comparison purposes
//  */
// export function getGenericTypeDefinition(t: TypeInfo) {
//   return t.generics == null ? t : new TypeInfo(t.fullname, t.generics.map(() => obj));
// }

// FSharpType

export function getUnionCases(t: NTypeInfo): NUnionCaseInfo[] {
  const cases = t.GetAllMembers().filter((m) => m instanceof NUnionCaseInfo) as NUnionCaseInfo[];
  if (cases.length > 0) {
    return cases;
  } else {
    throw new Error(`${t.fullname} is not an F# union type`);
  }
}

export function getRecordElements(t: NTypeInfo): NPropertyInfo[] {
  const fields = t.GetAllMembers().filter((m) => m instanceof NPropertyInfo && (m as NPropertyInfo).IsFSharp) as NPropertyInfo[];
  if (fields.length > 0) {
    return fields;
  } else {
    throw new Error(`${t.fullname} is not an F# record type`);
  }
}

export function getTupleElements(t: NTypeInfo): NTypeInfo[] {
  if (isTuple(t)) {
    return t.generics;
  } else {
    throw new Error(`${t.fullname} is not a tuple type`);
  }
}

export function getFunctionElements(t: NTypeInfo): [NTypeInfo, NTypeInfo] {
  if (isFunction(t)) {
    const gen = t.generics;
    return [gen[0], gen[1]];
  } else {
    throw new Error(`${t.fullname} is not an F# function type`);
  }
}

export function isUnion(t: any): boolean {
  if (t instanceof NTypeInfo) {
    const idx = (t as NTypeInfo).GetAllMembers().findIndex((m) => m instanceof NUnionCaseInfo);
    return idx >= 0;
  } else {
    return t instanceof Union;
  }
}

export function isRecord(t: any): boolean {
  if (t instanceof NTypeInfo) {
    const idx = t.GetAllMembers().findIndex((m) => m instanceof NPropertyInfo && (m as NPropertyInfo).IsFSharp);
    return idx >= 0;
  } else {
    return t instanceof Record;
  }
}

export function isTuple(t: NTypeInfo): boolean {
  return t.fullname.startsWith("System.Tuple");
}

// In .NET this is false for delegates
export function isFunction(t: NTypeInfo): boolean {
  return t.fullname === "Microsoft.FSharp.Core.FSharpFunc`2";
}

// FSharpValue

export function getUnionFields(v: any, t: NTypeInfo): [NUnionCaseInfo, any[]] {
  const cases = getUnionCases(t);
  const case_ = cases[v.tag];
  if (case_ == null) {
    throw new Error(`Cannot find case ${v.name} in union type`);
  }
  return [case_, v.fields];
}

export function getUnionCaseFields(uci: NUnionCaseInfo): NPropertyInfo[] {
  return uci.Fields.map((nt) => new NPropertyInfo(uci.DeclaringType, nt[0], nt[1], false, true, []));
}

export function getRecordFields(v: any): any[] {
  return Object.keys(v).map((k) => v[k]);
}

export function getRecordField(v: any, field: NPropertyInfo): any {
  return v[field.Name];
}

export function getTupleFields(v: any): any[] {
  return v;
}

export function getTupleField(v: any, i: number): any {
  return v[i];
}

export function makeUnion(uci: NUnionCaseInfo, values: any[]): any {
  return uci.Invoke.apply(null, values);
  // const expectedLength = (uci.fields || []).length;
  // if (values.length !== expectedLength) {
  //   throw new Error(`Expected an array of length ${expectedLength} but got ${values.length}`);
  // }
  // return new uci.declaringType.constructor(uci.tag, uci.name, ...values);
}

export function makeRecord(t: NTypeInfo, values: any[]): any {
  const fields = getRecordElements(t);
  if (fields.length !== values.length) {
    throw new Error(`Expected an array of length ${fields.length} but got ${values.length}`);
  }
  const ctor = t.GetAllMembers().find((m) => m instanceof NConstructorInfo) as NConstructorInfo;
  return ctor.Invoke(...values);
}

export function makeTuple(values: any[], t: NTypeInfo): any {
  return values;
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
  return x.name;
}

export function getCaseFields(x: any): any[] {
  assertUnion(x);
  return x.fields;
}
