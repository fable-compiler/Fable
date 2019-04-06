import { Record, Union } from "./Types";
import { compareArraysWith, equalArraysWith } from "./Util";

// tslint:disable: max-line-length

export enum BindingFlags {
  Instance = 4,
  NonPublic = 32,
  Public = 16,
  Static = 8,
}

export class NParameterInfo {
  constructor(
    public Name: string,
    public ParameterType: NTypeInfo,
  ) {}

  public toString() {
    return this.Name + " : " + this.ParameterType.toShortString();
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
    private invoke: (...args: any[]) => any,
    attributes: CustomAttribute[],
  ) {
    super(DeclaringType, Name, attributes);
  }

  public GetParameters() { return this.Parameters; }

  public get_IsStatic() { return this.IsStatic; }

  public get_IsGenericMethod() {  return false; }

  public GetGenericMethodDefinition() { return this; }

  public Invoke(target: any, ...args: any[]) {
    if (!this.IsStatic) {
      const a = [target, ...args];
      return this.invoke.apply(null, [target, ...args]);
    } else {
      return this.invoke.apply(null, args);
    }
  }
}

export class NMethodInfo extends NMethodBase {
  constructor(
    DeclaringType: NTypeInfo,
    Name: string,
    Parameters: NParameterInfo[],
    public ReturnType: NTypeInfo,
    IsStatic: boolean,
    invoke: (...args: any[]) => any,
    attributes: CustomAttribute[],
  ) {
    super(DeclaringType, Name, Parameters, IsStatic, invoke, attributes);
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

    return attPrefix + prefix + this.Name + "(" + args + ") : " + this.ReturnType.toShortString();
  }
}

export class NConstructorInfo extends NMethodBase {
  constructor(
    DeclaringType: NTypeInfo,
    Name: string,
    Parameters: NParameterInfo[],
    IsStatic: boolean,
    invoke: (...args: any[]) => any,
    attributes: CustomAttribute[],
  ) {
    super(DeclaringType, Name, Parameters, IsStatic, invoke, attributes);
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
    const typ = this.Type.toShortString();
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
    const mems = this.DeclaringType.GetMembers();
    const idx = mems.findIndex((m) => m instanceof NMethodInfo && m.Name === getterName);
    if (idx >= 0) { return mems[idx] as NMethodInfo; } else { return null; }
  }

  public get_SetMethod() {
    const getterName = "set_" + this.Name;
    const mems = this.DeclaringType.GetMembers();
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
    let typ = this.Type.toShortString();
    if (g && g.Parameters.length > 0) {
      const prefix = g.Parameters.map((p) => p.ParameterType.toShortString()).join(" * ");
      typ = prefix + " -> " + typ;
    } else if (s && s.Parameters.length > 0) {
      const prefix = s.Parameters.slice(0, s.Parameters.length - 1).map((p) => p.ParameterType.toShortString()).join(" * ");
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

export class NTypeInfo {
  public static getParameter(i: string) {
    if (NTypeInfo.parameterCache[i]) {
      return NTypeInfo.parameterCache[i];
    } else {
      const p = new NTypeInfo(i, 0, true, [], (_s) => [], () => []);
      this.parameterCache[i] = p;
      return p;
    }
  }
  private static parameterCache: {[i: string]: NTypeInfo} = {};

  private mems: NMemberInfo[] = null;

  constructor(
    public fullname: string,
    public genericCount: number,
    public isGenericParameter: boolean,
    public generics: NTypeInfo[],
    public members: (self: NTypeInfo) => NMemberInfo[],
    public cases: () => CaseInfo[]) {}

    public GetGenericTypeDefinition() {
      if (this.genericCount === 0 || this.generics.length === 0) {
        return this;
    } else {
      return new NTypeInfo(this.fullname, this.genericCount, this.isGenericParameter, [], this.members, this.cases);
    }
  }

  public MakeGenericType(args: NTypeInfo[]) {
    if (args.length !== this.genericCount) { throw new Error("invalid generic argument count "); }
    return new NTypeInfo(this.fullname, this.genericCount, this.isGenericParameter, args, this.members, this.cases);
  }

  public get_FullName() {
    return this.fullname;
  }
  public get_Namespace() {
    const i = this.fullname.lastIndexOf(".");
    return i === -1 ? "" : this.fullname.substr(0, i);
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

  public GetMembers() {
    if (!this.mems) {
      if (this.members) {
        this.mems = this.members(this);
      } else {
        this.mems = [];
      }
    }
    return this.mems;
  }

  public GetProperties() {
    const m = this.GetMembers();
    return m.filter((m) => m instanceof NPropertyInfo) as NPropertyInfo[];
  }

  public GetMethods() {
    const m = this.GetMembers();
    return m.filter((m) => m instanceof NMethodInfo) as NMethodInfo[];
  }

  public GetProperty(name: string) {
      const m = this.GetMembers();
      const prop = m.find((m) => m instanceof NPropertyInfo && m.Name === name) as NPropertyInfo;
      return prop;
  }
  public GetMethod(name: string) {
      const m = this.GetMembers();
      const meth = m.find((m) => m instanceof NMethodInfo && m.Name === name) as NMethodInfo;
      return meth;
  }

  public toShortString() {
    return this.fullname;
  }
  public toString() {
    const members =
      this
      .GetMembers()
      .filter((m) => !(m instanceof NMethodInfo) || !(m.Name.startsWith("get_") || m.Name.startsWith("set_")))
      .map((m) => "    " + m.toString()).join("\n");
    return "type " + this.fullname + "=\n" + members;
  }
}

export enum MemberKind {
  Property = 0,
  Field = 1,
  FSharpField = 2,
  Constructor = 3,
  Method = 4,
}

export type ParameterInfo = [string, TypeInfo];

export type FieldInfo = [string, TypeInfo, boolean, MemberKind, ParameterInfo[], CustomAttribute[], (...args: any) => any];

export interface CustomAttribute {
  AttributeType: string;
  AttributeValue: any;
}

export type Constructor = new(...args: any[]) => any;

export class CaseInfo {
  constructor(public declaringType: TypeInfo,
              public tag: number,
              public name: string,
              public fields?: TypeInfo[]) {
  }
}

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

function mkNTypeInfo(info: TypeInfo): NTypeInfo {
  if (typeCache[info.fullname]) {
    return typeCache[info.fullname];
  } else {
    const gen = info.generics || [];
    const cs = info.cases || (() => []);
    const fields = info.fields || (() => []);

    const res = new NTypeInfo(info.fullname, gen.length, false, gen.map((a) => mkNTypeInfo(a)), mkNMemberInfos(fields), cs);
    typeCache[info.fullname] = res;
    return res;
  }
}

export function declareType(fullname: string, generics: NTypeInfo[], members: () => FieldInfo[], cases: () => CaseInfo[]): NTypeInfo {
  let gen: NTypeInfo = null;
  if (typeCache[fullname]) {
    gen = typeCache[fullname];
  } else {
    gen = new NTypeInfo(fullname, generics.length, false, [], mkNMemberInfos(members), cases);
    typeCache[fullname] = gen;
  }

  if (generics.length > 0 && generics.length === gen.genericCount) {
    return gen.MakeGenericType(generics);
  } else {
    return gen;
  }
}

export function getGenericParamter(name: string) {
  NTypeInfo.getParameter(name);
}


export class TypeInfo {
  public NewInfo: NTypeInfo;
  constructor(public fullname: string,
              public generics?: TypeInfo[],
              public constructor?: Constructor,
              public fields?: () => FieldInfo[],
              public cases?: () => CaseInfo[]) {
    this.NewInfo = mkNTypeInfo(this);
  }
  public toString() {
    return fullName(this);
  }
  public Equals(other: TypeInfo) {
    return equals(this, other);
  }
  public CompareTo(other: TypeInfo) {
    return compare(this, other);
  }
}

export function getGenerics(t: (TypeInfo|NTypeInfo)): Array<(TypeInfo|NTypeInfo)> {
  return t.generics != null ? t.generics : [];
}

export function equals(t1: (TypeInfo|NTypeInfo), t2: (TypeInfo|NTypeInfo)): boolean {
  return t1.fullname === t2.fullname
    && equalArraysWith(getGenerics(t1), getGenerics(t2), equals);
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

export function type(fullname: string, generics?: TypeInfo[], fields?: () => FieldInfo[]): TypeInfo {
  const gen = generics || [];
  const f = fields || (() => []);
  return new TypeInfo(fullname, gen, null, f);
}

export function record(fullname: string, generics: TypeInfo[],
                       constructor: Constructor, fields: () => FieldInfo[]): TypeInfo {
  return new TypeInfo(fullname, generics, constructor, fields);
}

export function customAttributes(info: FieldInfo): CustomAttribute[] {
  return info[5];
}

export type CaseInfoInput = string | [string, TypeInfo[]];

export function union(fullname: string, generics: TypeInfo[],
                      constructor: Constructor, cases: () => CaseInfoInput[]): TypeInfo {
  const t: TypeInfo = new TypeInfo(fullname, generics, constructor, null, () => cases().map((x, i) =>
    typeof x === "string" ? new CaseInfo(t, i, x) : new CaseInfo(t, i, x[0], x[1])));
  return t;
}

export function tuple(...generics: TypeInfo[]): TypeInfo {
  return new TypeInfo("System.Tuple`" + generics.length, generics);
}

export function delegate(...generics: TypeInfo[]): TypeInfo {
  return new TypeInfo("System.Func`" + generics.length, generics);
}

export function lambda(argType: TypeInfo, returnType: TypeInfo): TypeInfo {
  return new TypeInfo("Microsoft.FSharp.Core.FSharpFunc`2", [argType, returnType]);
}

export function option(generic: TypeInfo): TypeInfo {
  return new TypeInfo("Microsoft.FSharp.Core.FSharpOption`1", [generic]);
}

export function list(generic: TypeInfo): TypeInfo {
  return new TypeInfo("Microsoft.FSharp.Collections.FSharpList`1", [generic]);
}

export function array(generic: TypeInfo): TypeInfo {
  return new TypeInfo(generic.fullname + "[]", [generic]);
}

export const obj: TypeInfo = new TypeInfo("System.Object");
export const unit: TypeInfo = new TypeInfo("Microsoft.FSharp.Core.Unit");
export const char: TypeInfo = new TypeInfo("System.Char");
export const string: TypeInfo = new TypeInfo("System.String");
export const bool: TypeInfo = new TypeInfo("System.Boolean");
export const int8: TypeInfo = new TypeInfo("System.SByte");
export const uint8: TypeInfo = new TypeInfo("System.Byte");
export const int16: TypeInfo = new TypeInfo("System.Int16");
export const uint16: TypeInfo = new TypeInfo("System.UInt16");
export const int32: TypeInfo = new TypeInfo("System.Int32");
export const uint32: TypeInfo = new TypeInfo("System.UInt32");
export const float32: TypeInfo = new TypeInfo("System.Single");
export const float64: TypeInfo = new TypeInfo("System.Double");
export const decimal: TypeInfo = new TypeInfo("System.Decimal");

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

export function getElementType(t: TypeInfo): TypeInfo {
  return isArray(t) ? t.generics[0] : null;
}

export function isGenericType(t: TypeInfo) {
  return t.generics != null && t.generics.length > 0;
}

/**
 * This doesn't replace types for fields (records) or cases (unions)
 * but it should be enough for type comparison purposes
 */
export function getGenericTypeDefinition(t: TypeInfo) {
  return t.generics == null ? t : new TypeInfo(t.fullname, t.generics.map(() => obj));
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
    return t.fields().filter((arr) => arr[3] === MemberKind.FSharpField);
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
    const gen = t.generics;
    return [gen[0], gen[1]];
  } else {
    throw new Error(`${t.fullname} is not an F# function type`);
  }
}

export function getTypeProperties(t: TypeInfo): FieldInfo[] {
  if (t.fields) {
      return t.fields()
              .filter((arr) => arr[3] === MemberKind.Property || arr[3] === MemberKind.FSharpField);
  } else {
    return [];
  }
}

export function isStatic(i: FieldInfo) {
  return i[2];
}

export function getTypeFields(t: TypeInfo): FieldInfo[] {
  if (t.fields) {
      return t.fields()
              .filter((arr) => arr[3] === MemberKind.Field);
  } else {
    return [];
  }
}

export function getTypeMembers(t: TypeInfo): FieldInfo[] {
  if (t.fields) {
      return t.fields();
  } else {
    return [];
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
  return uci.fields == null ? [] : uci.fields.map(( t, i ) => ["Data" + i, t, false, MemberKind.FSharpField, [], [], null] as FieldInfo);
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
  const expectedLength = (uci.fields || []).length;
  if (values.length !== expectedLength) {
    throw new Error(`Expected an array of length ${expectedLength} but got ${values.length}`);
  }
  return new uci.declaringType.constructor(uci.tag, uci.name, ...values);
}

export function makeRecord(t: TypeInfo, values: any[]): any {
  const fields = getRecordElements(t);
  if (fields.length !== values.length) {
    throw new Error(`Expected an array of length ${fields.length} but got ${values.length}`);
  }
  return new t.constructor(...values);
}

export function makeTuple(values: any[], t: TypeInfo): any {
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
