import { anonRecord as makeAnonRecord, Record, Union } from "./Types";
import { combineHashCodes, compareArraysWith, equalArraysWith, stringHash } from "./Util";

// tslint:disable: max-line-length

export enum NReflKind {
  Property = 0,
  Field = 1,
  MethodBase = 2,
  Method = 3,
  Constructor = 4,
  UnionCase = 5,
  Type = 6,
}

function compareStrings(l: string, r: string) {
  if (l === r) { return 0; }
  return l < r ? -1 : 1;
}

export class NParameterInfo {
  constructor(
    public Name: string,
    public ParameterType: NTypeInfo,
  ) { }

  public toPrettyString() {
    return this.Name + " : " + this.ParameterType.toFullString();
  }

  public toString() {
    return this.ParameterType.toString() + " " + this.Name;
  }

  public get_ParameterType() {
    return this.ParameterType;
  }

  public get_Name() {
    return this.Name;
  }

  public GetHashCode() {
    return combineHashCodes([stringHash(this.Name), this.ParameterType.GetHashCode()]);
  }

  public Equals(o: any) {
    if ("Name" in o && "ParameterType" in o) {
      return this.Name === o.Name && equals(this.ParameterType, o.ParameterType);
    } else {
      return false;
    }
  }

  public CompareTo(o: any) {
    if (o == null) { throw new Error("cannot compare to null"); }

    if ("Name" in o && "ParameterType" in o) {
      const c = compareStrings(this.Name, o.Name);
      if (c !== 0) { return c; }
      return this.ParameterType.CompareTo(o.ParameterType);
    }
    throw new Error(`cannot compare to ${o}`);
  }

}

export abstract class NMemberInfo {
  public attributes: CustomAttribute[];

  constructor(
    public DeclaringType: NTypeInfo,
    public Name: string,
    att: CustomAttribute[],
    ) {

      if (!Name) { throw new Error(`cannot declare MemberInfo without a name`); }
      if (!DeclaringType) { throw new Error(`${Name} has no declaring type`); }
      this.attributes = att || [];
  }

  public abstract getMemberKind(): NReflKind;
  public abstract toPrettyString(): string;

  public abstract GetHashCode(): number;
  public abstract Equals(o: any): boolean;
  public abstract CompareTo(o: any): number;

  public is(kind: NReflKind) {
    return this.getMemberKind() === kind;
  }

  public get_Name() { return this.Name; }

  public get_DeclaringType() { return this.DeclaringType; }

  public GetCustomAttributes(a: (boolean|NTypeInfo), _?: boolean) {
    if (typeof a === "boolean") {
      return this.attributes.map((att) => att.AttributeValue);
    } else if (a.fullname) {
      return this.attributes.filter((att) => att.AttributeType === a.fullname).map((att) => att.AttributeValue);
    } else {
      return this.attributes.map((att) => att.AttributeValue);
    }
  }
}

export abstract class NMethodBase extends NMemberInfo {
  public Parameters: NParameterInfo[];
  public IsStatic: boolean;

  constructor(
    DeclaringType: NTypeInfo,
    Name: string,
    parameters: NParameterInfo[],
    isStatic: boolean,
    attributes: CustomAttribute[],
  ) {
    super(DeclaringType, Name, attributes);
    parameters = parameters || [];
    this.IsStatic = isStatic || false;
    this.Parameters = parameters.map((t) => t.ParameterType.get_ContainsGenericParameters() ? new NParameterInfo(t.Name, DeclaringType.ResolveGeneric(t.ParameterType)) : t);
  }

  public GetParameters() { return this.Parameters; }

  public get_IsStatic() { return this.IsStatic; }

  public ParametersAssignable(ts: NTypeInfo[]) {
    if (this.Parameters.length === ts.length) {
      const broken =
        this.Parameters.findIndex((p, i) => {
            const d = p.ParameterType;
            const a = ts[i];
            return !(d.isGenericParameter || d.fullname === "System.Object" || equals(d, a));
        });
      return broken < 0;
    } else {
      return false;
    }
  }

}

export class NMethodInfo extends NMethodBase {
  public ReturnType: NTypeInfo;
  public GenericArguments: NTypeInfo[];

  private isGenericDef: boolean;
  private genMap: { [name: string]: number } = {};

  constructor(
    declaringType: NTypeInfo,
    genericArguments: NTypeInfo[],
    name: string,
    parameters: NParameterInfo[],
    returnType: NTypeInfo,
    isStatic: boolean,
    private invoke: (target: any, args: any[]) => any,
    attributes: CustomAttribute[],
    private declaration?: NMethodInfo,
  ) {
    super(declaringType, name, parameters.map((a) => a), isStatic, attributes);

    if (!returnType) { throw new Error(`MethodInfo ${declaringType.toFullString()} ${name} does not have a return type`); }

    genericArguments = genericArguments || [];
    const isDef = genericArguments.findIndex((p) => p.isGenericParameter) >= 0;

    this.GenericArguments = genericArguments;
    this.isGenericDef = isDef;
    if (isDef) {
      genericArguments.forEach((v, i) => {
        this.genMap[v.fullname] = i;
      });
    } else if (declaration) {
      this.genMap = declaration.genMap;
    }
    this.Parameters.forEach((p) => { p.ParameterType = this.ResolveGeneric(p.ParameterType); });
    this.ReturnType = returnType.get_ContainsGenericParameters() ? this.ResolveGeneric(returnType) : returnType;
    if (!this.ReturnType) { throw new Error(`MethodInfo ${declaringType.toFullString()} ${name} does not have a return type`); }

  }

  public getMemberKind() {
    return NReflKind.Method;
  }

  public GetHashCode() {
    return combineHashCodes([
      this.DeclaringType.GetHashCode(),
      combineHashCodes(this.GenericArguments.map((a) => a.GetHashCode())),
      stringHash(this.Name),
      combineHashCodes(this.Parameters.map((a) => a.GetHashCode())),
      (this.IsStatic ? 1 : 0),
    ]);
  }

  public CompareTo(oo: any) {
    if (!isMethodInfo(oo)) { throw new Error(`cannot compare MethodInfo to ${oo}`); }
    const o = oo as NMethodInfo;
    let c = 0;
    c = compareStrings(this.Name, o.Name); if (c !== 0) { return c; }
    c = this.IsStatic === o.IsStatic ? 0 : (this.IsStatic < o.IsStatic ? -1 : 1); if (c !== 0) { return c; }
    c = this.DeclaringType.CompareTo(o.DeclaringType); if (c !== 0) { return c; }
    c = compareArraysWith(this.GenericArguments, o.GenericArguments, (l, r) => l.CompareTo(r)); if (c !== 0) { return c; }
    c = compareArraysWith(this.Parameters, o.Parameters, (l, r) => l.CompareTo(r)); if (c !== 0) { return c; }
    return 0;
  }

  public Equals(ob: any) {
    if (isMethodInfo(ob)) {
      return methodEquals(this, ob as NMethodInfo);
    } else {
      return false;
    }
  }

  public ResolveGeneric(t: NTypeInfo): NTypeInfo {
    if (t.isGenericParameter) {
      if (t.fullname in this.genMap) {
        const idx = this.genMap[t.fullname];
        if (idx < 0 || idx > this.GenericArguments.length) {
          throw new Error(`invalid generic index ${idx}`);
        }

        return this.GenericArguments[idx];
      } else {
        return this.DeclaringType.ResolveGeneric(t);
      }
    } else if (t.get_ContainsGenericParameters()) {
      const gen = t.generics.map((t) => this.ResolveGeneric(t));
      return t.MakeGenericType(gen);
      // return new NTypeInfo(t.fullname, t.genericCount, t.isGenericParameter, t.generics.map((t) => this.ResolveType(t)), t.members, t.declaration)
    } else {
      return t;
    }
  }

  public get_IsGenericMethodDefinition() {
    return this.isGenericDef;
  }

  public MakeGenericMethod(types: NTypeInfo[]) {
    return new NMethodInfo(this.DeclaringType, types, this.Name, this.Parameters, this.ReturnType, this.IsStatic, this.invoke, this.attributes, this);
  }

  public get_IsGenericMethod() {  return this.isGenericDef || this.declaration; }

  public GetGenericArguments(): NTypeInfo[] {
    return this.GenericArguments;
  }

  public GetGenericMethodDefinition(): NMethodInfo {
    if (this.declaration) {
      return this.declaration;
    } else {
      return this;
    }
  }

  public get_ReturnType() { return this.ReturnType; }

  public get_ReturnParameter() {
    return new NParameterInfo("Return", this.ReturnType);
  }

  public toPrettyString(): string {
    const args = this.Parameters.map((p) => p.toPrettyString()).join(", ");

    let attPrefix = "";
    const atts = this.GetCustomAttributes(true);
    if (atts.length > 0) {
      attPrefix = "[<" + atts.map((a) => a.toString()).join("; ") + ">] ";
    }
    let prefix = "member ";
    if (this.IsStatic) { prefix = "static " + prefix; }

    return attPrefix + prefix + this.Name + "(" + args + ") : " + this.ReturnType.toFullString();
  }

  public toString() {
    return this.toPrettyString();
  }

  public Invoke(target: any, args: any[]) {
    args = args || [];
    if (!this.IsStatic) {
      if (!target) { throw new Error(`MethodInfo ${this.toPrettyString()} cannot be called without a this argument`); }
      return this.invoke(target, args);
    } else {
      return this.invoke(null, args);
    }
  }
}

export class NConstructorInfo extends NMethodBase {
  constructor(
    declaringType: NTypeInfo,
    parameters: NParameterInfo[],
    private invoke: (args: any[]) => any,
    attributes: CustomAttribute[],
  ) {
    super(declaringType, ".ctor", parameters, true, attributes);
  }

  public toPrettyString() {
    const args = this.Parameters.map((p) => p.toPrettyString()).join(", ");

    let attPrefix = "";
    const atts = this.GetCustomAttributes(true);
    if (atts.length > 0) {
      attPrefix = "[<" + atts.map((a) => a.toString()).join("; ") + ">] ";
    }

    return attPrefix + "new(" + args + ")";
  }

  public getMemberKind() {
    return NReflKind.Constructor;
  }

  public GetHashCode() {
    return combineHashCodes([
      this.DeclaringType.GetHashCode(),
      combineHashCodes(this.Parameters.map((p) => p.GetHashCode())),
    ]);
  }

  public Equals(o: any) {
    if (isConstructorInfo(o)) {
      return constructorEquals(this, o as NConstructorInfo);
    } else {
      return false;
    }
  }

  public CompareTo(oo: any) {
    if (!isConstructorInfo(oo)) { throw new Error(`cannot compare ConstructorInfo to ${oo}`); }
    const o = oo as NConstructorInfo;
    let c = 0;
    c = this.DeclaringType.CompareTo(o.DeclaringType); if (c !== 0) { return c; }
    c = compareArraysWith(this.Parameters, o.Parameters, (l, r) => l.CompareTo(r)); if (c !== 0) { return c; }
    return 0;
  }

  public toString() {
    return this.toPrettyString();
  }

  public Invoke(args: any[]) {
    args = args || [];
    return this.invoke(args);
  }
}

export class NFieldInfo extends NMemberInfo {
  public Type: NTypeInfo;
  constructor(
    declaringType: NTypeInfo,
    name: string,
    type: NTypeInfo,
    public IsStatic: boolean,
    attributes: CustomAttribute[],
    private get?: (t: any) => any,
  ) {
    super(declaringType, name, attributes);

    if (!type) { throw new Error(`FieldInfo ${name} does not have a type`); }

    this.Type = type.get_ContainsGenericParameters() ? this.DeclaringType.ResolveGeneric(type) : type;
  }
  public get_FieldType() { return this.Type; }
  public get_IsStatic() { return this.IsStatic; }

  public getMemberKind() {
    return NReflKind.Field;
  }

  public GetHashCode() {
    return combineHashCodes([
      stringHash(this.Name),
      this.DeclaringType.GetHashCode(),
      (this.IsStatic ? 1 : 0),
    ]);
  }

  public Equals(o: any) {
    if (isFieldInfo(o)) {
      return fieldEquals(this, o as NFieldInfo);
    } else {
      return false;
    }
  }

  public CompareTo(oo: any) {
    if (!isFieldInfo(oo)) { throw new Error(`cannot compare FieldInfo to ${oo}`); }
    const o = oo as NFieldInfo;
    let c = 0;
    c = compareStrings(this.Name, o.Name); if (c !== 0) { return c; }
    c = this.IsStatic === o.IsStatic ? 0 : (this.IsStatic < o.IsStatic ? -1 : 1); if (c !== 0) { return c; }
    c = this.DeclaringType.CompareTo(o.DeclaringType); if (c !== 0) { return c; }
    return 0;
  }

  public toPrettyString() {
    const typ = this.Type.toFullString();
    let prefix = "val ";
    if (this.IsStatic) { prefix = "static " + prefix; }

    let attPrefix = "";
    const atts = this.GetCustomAttributes(true);
    if (atts.length > 0) {
      attPrefix = "[<" + atts.map((a) => a.toString()).join("; ") + ">] ";
    }

    return attPrefix + prefix + this.Name + " : " + typ;
  }

  public GetValue(target: any) {
    if (this.get) {
        return this.get(target);
    } else {
      throw new Error("cannot get field " + this.toPrettyString());
    }
  }

  public toString() {
    return this.toPrettyString();
  }

}

export class NPropertyInfo extends NMemberInfo {
  public Type: NTypeInfo;
  constructor(
    DeclaringType: NTypeInfo,
    Name: string,
    type: NTypeInfo,
    public IsStatic: boolean,
    public IsFSharp: boolean,
    attributes: CustomAttribute[],
    private get?: (target: any, index: any[]) => any,
    private set?: (target: any, index: any[], value: any) => void,
  ) {
    super(DeclaringType, Name, attributes);

    if (!type) { throw new Error(`FieldInfo ${Name} does not have a type`); }

    this.Type = type.get_ContainsGenericParameters() ? DeclaringType.ResolveGeneric(type) : type;
  }

  public getMemberKind() {
    return NReflKind.Property;
  }

  public GetHashCode() {
    return combineHashCodes([
      stringHash(this.Name),
      this.DeclaringType.GetHashCode(),
      (this.IsFSharp ? 1 : 0),
      (this.IsStatic ? 1 : 0),
    ]);
  }

  public Equals(o: any) {
    if (isPropertyInfo(o)) {
      return propertyEquals(this, o as NPropertyInfo);
    } else {
      return false;
    }
  }

  public CompareTo(oo: any) {
    if (!isPropertyInfo(oo)) { throw new Error(`cannot compare FieldInfo to ${oo}`); }
    const o = oo as NPropertyInfo;
    let c = 0;
    c = this.IsStatic === o.IsStatic ? 0 : (this.IsStatic < o.IsStatic ? -1 : 1); if (c !== 0) { return c; }
    c = this.IsFSharp === o.IsFSharp ? 0 : (this.IsFSharp < o.IsFSharp ? -1 : 1); if (c !== 0) { return c; }
    c = compareStrings(this.Name, o.Name); if (c !== 0) { return c; }
    c = this.DeclaringType.CompareTo(o.DeclaringType); if (c !== 0) { return c; }
    return 0;
  }

  public get_PropertyType() { return this.Type; }
  public get_IsStatic() { return this.IsStatic; }
  public get_IsFSharp() { return this.IsFSharp; }

  public get_CanRead() {
    return this.get || this.get_GetMethod();
  }
  public get_CanWrite() {
    return this.set || this.get_SetMethod();
  }

  public GetIndexParameters() {
    const m = this.get_GetMethod();
    if (m) { return m.GetParameters(); } else { return []; }
  }

  public get_GetMethod() {
    const getterName = "get_" + this.Name;
    const mems = this.DeclaringType.GetAllMembers();
    const idx = mems.findIndex((m) => isMethodInfo(m) && m.Name === getterName);
    if (idx >= 0) {
      return mems[idx] as NMethodInfo;
    } else {
      return null;
    }
  }

  public get_SetMethod() {
    const getterName = "set_" + this.Name;
    const mems = this.DeclaringType.GetAllMembers();
    const idx = mems.findIndex((m) => isMethodInfo(m) && m.Name === getterName);
    if (idx >= 0) { return mems[idx] as NMethodInfo; } else { return null; }
  }

  public GetValue(target: any, index: any[]): any {
    if (this.get) {
      return this.get(target, index);
    } else if (this.IsFSharp) {
      // TODO: mangled-names????
      if (this.Name in target) {
        return target[this.Name];
      } else {
        throw new Error(`property ${this.Name} not in target`);
      }
    } else {
      const g = this.get_GetMethod();
      if (g === null) { throw new Error(`property ${this.Name} has no GetMethod`); }
      index = index || [];
      return g.Invoke(target, index);
    }
  }

  public SetValue(target: any, value: any, index: any[]) {
    if (this.set) {
      return this.set(target, index, value);
    } else if (this.IsFSharp) {
      target[this.Name] = value;
    } else {
      index = index || [];
      const s = this.get_SetMethod();
      s?.Invoke(target, [...index, value]);
    }
  }

  public toPrettyString() {
    const g = this.get_GetMethod();
    const s = this.get_SetMethod();
    let typ = this.Type.toFullString();
    if (g && g.Parameters.length > 0) {
      const prefix = g.Parameters.map((p) => p.ParameterType.toFullString()).join(" * ");
      typ = prefix + " -> " + typ;
    } else if (s && s.Parameters.length > 0) {
      const prefix = s.Parameters.slice(0, s.Parameters.length - 1).map((p) => p.ParameterType.toFullString()).join(" * ");
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

  public toString() {
    return this.toPrettyString();
  }

}

export class NUnionCaseInfo extends NMemberInfo {
  public Fields: NPropertyInfo[];

  public constructor(
    DeclaringType: NTypeInfo,
    public Tag: number,
    Name: string,
    Attributes: CustomAttribute[],
    fields: Array<[string, NTypeInfo]>,
    public Invoke: (...args: any[]) => any,
  ) {
    super(DeclaringType, Name, Attributes);

    if (typeof Tag !== "number") { throw new Error(`UnionCase ${Name} does not have a tag`); }

    fields = fields || [];
    this.Fields = fields.map ((tup, i) => {
      const name = tup[0];
      const typ = tup[1].get_ContainsGenericParameters() ? DeclaringType.ResolveGeneric(tup[1]) : tup[1];
      return new NPropertyInfo(DeclaringType, name, typ, false, true, [], (t) => t.fields[i], (t, v) => { t.fields[i] = v; });
      // tup[1].get_ContainsGenericParameters() ? [tup[0], DeclaringType.ResolveGeneric(tup[1])] : tup) as Array<[string, NTypeInfo]>;
    });
  }

  public getMemberKind() {
    return NReflKind.UnionCase;
  }

  public GetHashCode() {
    return combineHashCodes([
      stringHash(this.Name),
      this.DeclaringType.GetHashCode(),
    ]);
  }

  public Equals(o: any) {
    if (isUnionCaseInfo(o)) {
      return unionCaseEquals(this, o as NUnionCaseInfo);
    } else {
      return false;
    }
  }

  public CompareTo(oo: any) {
    if (!isUnionCaseInfo(oo)) { throw new Error(`cannot compare UnionCaseInfo to ${oo}`); }
    const o = oo as NUnionCaseInfo;
    let c = 0;
    c = compareStrings(this.Name, o.Name); if (c !== 0) { return c; }
    c = this.DeclaringType.CompareTo(o.DeclaringType); if (c !== 0) { return c; }
    return 0;
  }


  public GetFields() {
    return this.Fields;
  }

  public get_Tag() {
    return this.Tag;
  }

  public toPrettyString() {
    const decl = this.DeclaringType.toFullString();
    const fields = this.Fields.map((tup) => tup.Name + ": " + tup.Type.toFullString()).join(" * ");
    return decl + "." + this.Name + " of " + fields;
  }

  public toString() {
    return this.toPrettyString();
  }

}

export type NEnumCase = [string, number];

export class NTypeInfo {
  public static getParameter(i: string) {
    if (NTypeInfo.parameterCache[i]) {
      return NTypeInfo.parameterCache[i];
    } else {
      const p = new NTypeInfo(i, 0, true, [], (_s) => [], null, null);
      this.parameterCache[i] = p;
      return p;
    }
  }

  public static Simple(name: string) {
    return new NTypeInfo(name, 0, false, [], ((_s) => []), null, null);
  }

  private static parameterCache: {[i: string]: NTypeInfo} = {};
  public generics: NTypeInfo[];
  public GenericDeclaration: NTypeInfo | null;
  public DeclaringType: NTypeInfo | null;

  private cachedMembers: NMemberInfo[] | null = null;
  private instantiations: {[i: string]: NTypeInfo} = {};
  private genericMap: {[name: string]: number} = {};

  constructor(
    public fullname: string,
    public genericCount: number,
    public isGenericParameter: boolean,
    _generics: NTypeInfo[],
    public members: (self: NTypeInfo) => NMemberInfo[],
    genericDeclaration: NTypeInfo | null,
    declaringType: NTypeInfo | null,
    public enumCases?: NEnumCase[]) {
      if (!fullname) { throw new Error("cannot declare type without name"); }
      _generics = _generics || [];
      isGenericParameter = isGenericParameter || false;
      genericCount = genericCount || 0;

      const g = _generics.filter((t) => !t.isGenericParameter);
      if (g.length === genericCount) {
        this.generics = g;
        if (genericDeclaration) {
          this.genericMap = genericDeclaration.genericMap;
        }
      } else {
        _generics.forEach((g, i) => {
          this.genericMap[g.get_Name()] = i;
        });
        this.generics = _generics;
      }
      if (this.generics.length !== this.genericCount) { throw new Error(`${this.fullname} contains ${this.genericCount} generic parameters but only ${this.generics.length} given.`); }
      this.GenericDeclaration = genericDeclaration || null;
      this.DeclaringType = declaringType;
    }

    public get_DeclaringType(): NTypeInfo | null {
      return this.DeclaringType;
    }

    public MakeArrayType(): NTypeInfo {
      return array(this);
    }

    public get_IsGenericParameter() {
      return this.isGenericParameter;
    }

    public ResolveGeneric(t: NTypeInfo): NTypeInfo {
      if (t.isGenericParameter) {
        if (t.fullname in this.genericMap) {
          const idx = this.genericMap[t.fullname];
          return this.generics[idx];
        } else {
          return t;
        }
      } else if (t.genericCount > 0) {
        return new NTypeInfo(t.fullname, t.genericCount, false, t.generics.map((ta) => this.ResolveGeneric(ta)), t.members, t.GenericDeclaration, t.DeclaringType);
      } else {
        return t;
      }
    }

    public GetGenericTypeDefinition() {
      if (this.genericCount === 0 || this.get_IsGenericTypeDefinition()) {
        return this;
      } else if (this.GenericDeclaration) {
        return this.GenericDeclaration;
      } else {
        throw new Error(`${this.fullname} does not have a proper generic definition`);
      }
    }

  public MakeGenericType(args: NTypeInfo[]) {
    // args = args.filter((a) => a);
    if (args.length === 0) { return this; }
    if (args.length !== this.genericCount) { throw new Error(`${this.fullname} contains ${this.genericCount} generic parameters but only ${args.length} given.`); }

    const key = args.map((t) => t.toString()).join(", ");
    if (key in this.instantiations) {
      return this.instantiations[key];
    } else {
      const res = new NTypeInfo(this.fullname, this.genericCount, this.isGenericParameter, args, this.members, this, this.DeclaringType);
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

  public GetElementType(): NTypeInfo | null {
    return this.get_IsArray() ? this.generics[0] : null;
  }

  public get_IsGenericType() {
    return this.genericCount > 0;
  }
  public get_IsGenericTypeDefinition() {
    if (this.genericCount > 0) {
      const idx = this.generics.findIndex((g) => g.isGenericParameter);
      return idx >= 0;
    } else {
      return false;
    }
  }

  public get_ContainsGenericParameters(): boolean {
    return this.isGenericParameter || (this.genericCount > 0 && (this.generics.length === 0 || (this.generics.findIndex((t) => t.get_ContainsGenericParameters()) >= 0)));
  }

  public GetGenericArguments() {
    if (this.genericCount > 0) {
      if (this.generics.length > 0) {
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
    if (!this.cachedMembers) {
      if (this.members) {
        this.cachedMembers = this.members(this);
      } else {
        this.cachedMembers = [];
      }
    }
    return this.cachedMembers;
  }

  public GetMembers() {
    const m = this.GetAllMembers();
    return m.filter((m) => !(isUnionCaseInfo(m)));
  }

  public GetProperties() {
    const m = this.GetAllMembers();
    return m.filter((m) => isPropertyInfo(m)) as NPropertyInfo[];
  }

  public GetMethods() {
    const m = this.GetAllMembers();
    return m.filter((m) => isMethodInfo(m)) as NMethodInfo[];
  }
  public GetConstructors() {
    const m = this.GetAllMembers();
    return m.filter((m) => isConstructorInfo(m)) as NConstructorInfo[];
  }
  public GetFields() {
    const m = this.GetAllMembers();
    return m.filter((m) => isFieldInfo(m)) as NFieldInfo[];
  }
  public GetConstructor(ts?: NTypeInfo[]) {
    if (ts) {
      return this.GetConstructors().find((ctor) => ctor.ParametersAssignable(ts));
    } else {
      const ctors = this.GetConstructors();
      return ctors.length === 1 ? ctors[0] : null;
    }
  }
  public GetField(name: string) {
    const m = this.GetAllMembers();
    return m.find((m) => isFieldInfo(m) && m.Name === name) as NFieldInfo;
  }

  public GetProperty(name: string) {
      const m = this.GetAllMembers();
      const prop = m.find((m) => isPropertyInfo(m) && m.Name === name) as NPropertyInfo;
      return prop;
  }
  public GetMethod(name: string, types?: NTypeInfo[]) {
      const m = this.GetAllMembers();
      if (types) {
        const meths = m.filter((m) => isMethodInfo(m) && m.Name === name && (m as NMethodInfo).ParametersAssignable(types)) as NMethodInfo[];
        return meths.length === 1 ? meths[0] : null;
      } else {
        const meths = m.filter((m) => isMethodInfo(m) && m.Name === name) as NMethodInfo[];
        return meths.length === 1 ? meths[0] : null;
      }
  }
  public toFullString(): string {
    if (this.isGenericParameter) {
      return "'" + this.fullname;
    } else if (this.genericCount > 0) {
      let name = this.fullname;
      const suffix = "`" + this.genericCount;
      if (name.endsWith(suffix)) {
        name = name.substr(0, name.length - suffix.length);
      }
      const args = this.generics.map((t) => t.toFullString()).join(", ");
      return name + "<" + args + ">";
    } else {
      return this.fullname;
    }
  }

  public toString(): string {
    if (this.genericCount > 0) {
      const suffix = "`" + this.genericCount;
      return this.fullname.endsWith(suffix) ? this.fullname : this.fullname + suffix;
    } else {
      return this.fullname;
    }
  }
  public toPrettyString() {
    const members =
      this
      .GetMembers()
      .filter((m) => !(isMethodInfo(m)) || !(m.Name.startsWith("get_") || m.Name.startsWith("set_")))
      .map((m) => "    " + m.toPrettyString()).join("\n");
    return "type " + this.toFullString() + "=\n" + members;
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

export interface CustomAttribute {
  AttributeType: string;
  AttributeValue: any;
}

const typeCache: { [fullname: string]: NTypeInfo } = {};

export function declareNType(fullname: string, generics: number, members: (self: NTypeInfo, gen: NTypeInfo[]) => NMemberInfo[]): NTypeInfo {
  let gen: NTypeInfo;
  if (fullname in typeCache) {
    gen = typeCache[fullname];
  } else {
    const pars = Array.from({ length: generics }, (_, i) => getGenericParameter("a" + i));

    const mems = members || ((_self, _gen) => []);
    gen = new NTypeInfo(fullname, pars.length, false, pars, (s) => mems(s, pars), null, null);
    typeCache[fullname] = gen;
  }
  return gen;
}

export function getGenericParameter(name: string) {
  return NTypeInfo.getParameter(name);
}

export function getGenerics(t: NTypeInfo): NTypeInfo[] {
  const gen = t.generics || [];
  // const badIdx = gen.findIndex((t) => !t);
  // if (badIdx >= 0) { throw new Error("bad generic arg: " + badIdx); }
  return gen;
}

export function equals(t1: NTypeInfo, t2: NTypeInfo): boolean {
  if (t1 === t2) { return true; } else
  if (t1 == null && t2 != null) { return false; } else
  if (t1 != null && t2 == null) { return false; } else {
    if (t1.fullname === t2.fullname) {
      const g1 = getGenerics(t1);
      const g2 = getGenerics(t2);
      try {
        return equalArraysWith(g1, g2, equals);
      } catch (e) {
        throw new Error(t1.fullname + " g1: " + g1 + " g2: " + g2);
      }
    } else {
      return false;
    }
  }
}

function typesEqual(l: NTypeInfo[], r: NTypeInfo[]) {
  if (l.length === r.length) {
    for (let i = 0; i < l.length; i++) {
      if (!equals(l[i], r[i])) { return false; }
    }
    return true;
  } else {
    return false;
  }
}

export function parameterEquals(l: NParameterInfo, r: NParameterInfo) {
  if (l === r) { return true; } else
  if (l == null && r != null) { return false; } else
  if (l != null && r == null) { return false; } else {
    return l.Name === r.Name && equals(l.ParameterType, r.ParameterType);
  }
}

function parametersEqual(l: NParameterInfo[], r: NParameterInfo[]) {
  if (l.length === r.length) {
    for (let i = 0; i < l.length; i++) {
      if (!parameterEquals(l[i], r[i])) { return false; }
    }
    return true;
  } else {
    return false;
  }
}

export function fieldEquals(l: NFieldInfo, r: NFieldInfo) {
  if (l === r) { return true; } else
  if (l == null && r != null) { return false; } else
  if (l != null && r == null) { return false; } else {
    return l.Name === r.Name && l.IsStatic === r.IsStatic && equals(l.DeclaringType, r.DeclaringType);
  }
}

export function propertyEquals(l: NPropertyInfo, r: NPropertyInfo) {
  if (l === r) { return true; } else
  if (l == null && r != null) { return false; } else
  if (l != null && r == null) { return false; } else {
    return l.Name === r.Name && l.IsFSharp === r.IsFSharp && l.IsStatic === r.IsStatic && equals(l.DeclaringType, r.DeclaringType);
  }
}

export function constructorEquals(l: NConstructorInfo, r: NConstructorInfo) {
  if (l === r) { return true; } else
  if (l == null && r != null) { return false; } else
  if (l != null && r == null) { return false; } else {
    return equals(l.DeclaringType, r.DeclaringType) && parametersEqual(l.Parameters, r.Parameters);
  }
}

export function methodEquals(l: NMethodInfo, r: NMethodInfo) {
  if (l === r) { return true; } else
  if (l == null && r != null) { return false; } else
  if (l != null && r == null) { return false; } else {
    return l.Name === r.Name &&
           l.IsStatic === r.IsStatic &&
           equals(l.DeclaringType, r.DeclaringType) &&
           typesEqual(l.GenericArguments, r.GenericArguments) &&
           parametersEqual(l.Parameters, r.Parameters);
  }
}

export function unionCaseEquals(l: NUnionCaseInfo, r: NUnionCaseInfo) {
  if (l === r) { return true; } else
  if (l == null && r != null) { return false; } else
  if (l != null && r == null) { return false; } else {
    return l.Name === r.Name && equals(l.DeclaringType, r.DeclaringType);
  }
}

export function memberEquals(l: NMemberInfo, r: NMemberInfo) {
  if (l === r) { return true; } else
  if (l == null && r != null) { return false; } else
  if (l != null && r == null) { return false; } else {
    const lk = l.getMemberKind();
    const rk = r.getMemberKind();
    if (lk !== rk) { return false; }

    switch (lk) {
      case NReflKind.Constructor: return constructorEquals(l as NConstructorInfo, r as NConstructorInfo);
      case NReflKind.Method: return methodEquals(l as NMethodInfo, r as NMethodInfo);
      case NReflKind.Field: return fieldEquals(l as NFieldInfo, r as NFieldInfo);
      case NReflKind.Property: return propertyEquals(l as NPropertyInfo, r as NPropertyInfo);
      case NReflKind.UnionCase: return unionCaseEquals(l as NUnionCaseInfo, r as NUnionCaseInfo);
      default: return l === r;
    }
  }
}

export function methodBaseEquals(l: NMemberInfo, r: NMemberInfo) {
  return memberEquals(l, r);
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

export function ntype(fullname: string, genericNames?: string[], generics?: NTypeInfo[], members?: (self: NTypeInfo, pars: NTypeInfo[]) => NMemberInfo[], declaringType?: NTypeInfo): NTypeInfo {
  let gen: NTypeInfo;
  generics = generics || [];
  const a = generics.findIndex((t) => !t);
  if (a >= 0) { throw new Error("bad hate occured"); }

  if (fullname in typeCache) {
    gen = typeCache[fullname];
  } else {
    const _members = members || ((_s, _g) => []);
    genericNames = genericNames || [];
    const b = genericNames.findIndex((t) => !t);
    if (b >= 0) { throw new Error("bad hate occured"); }

    const pars = genericNames.map((n) => getGenericParameter(n));
    gen = new NTypeInfo(fullname, pars.length, false, pars, (s) => _members(s, pars), null, declaringType || null);
    typeCache[fullname] = gen;
  }

  if (generics.length > 0) {
    return gen.MakeGenericType(generics);
  } else {
    return gen;
  }
}
function selectMany<TIn, TOut>(input: TIn[], selectListFn: (t: TIn, i: number) => TOut[]): TOut[] {
  return input.reduce((out, inx, idx) => {
    out.push(...selectListFn(inx, idx));
    return out;
  }, new Array<TOut>());
}

export function tuple(...generics: any[]): NTypeInfo {
  if (generics.length === 1) { generics = generics[0] as NTypeInfo[]; }
  if (generics.length === 0) { throw new Error("empty tuple"); }
  const name = "System.Tuple`" + generics.length;
  const gen =
    declareNType(name, generics.length, (self, gen) => selectMany<NTypeInfo, NMemberInfo>(gen, (t, i) => [
      new NPropertyInfo(self, "Item" + i, t, false, false, []),
      new NMethodInfo(self, [], "get_Item" + i, [], t, false, ((a) => a[i]), []),
    ]));
  return gen.MakeGenericType(generics);
}

export function delegate(...generics: NTypeInfo[]): NTypeInfo {
  const name = "System.Func`" + generics.length;
  const gen =
    declareNType(name, generics.length, (self, _gen) => {
      const ret = generics[generics.length - 1];
      const args = generics.slice(0, generics.length - 1).map((t, i) => new NParameterInfo("arg" + i, t));
      return [
        new NMethodInfo(self, [], "Invoke", args, ret, false, ((target, ...args) => target(...args)), []),
      ];
    });
  return gen.MakeGenericType(generics);
}

export function lambda(argType: NTypeInfo, returnType: NTypeInfo): NTypeInfo {
  const name = "Microsoft.FSharp.Core.FSharpFunc`2";
  const gen =
    declareNType(name, 2, (self, gen) => [
      new NMethodInfo(self, [], "Invoke", [new NParameterInfo("arg", gen[0])], gen[1], false, ((target, ...args) => target(...args)), []),
    ]);
  return gen.MakeGenericType([argType, returnType]);
}

export function option(generic: NTypeInfo): NTypeInfo {
  const name = "Microsoft.FSharp.Core.FSharpOption`1";
  const gen =
    declareNType(name, 1, (self, gen) => [
      new NUnionCaseInfo(self, 0, "None", [], [], ((_) => null)),
      new NUnionCaseInfo(self, 1, "Some", [], [["Value", gen[0]]], ((args, ..._rest) => args)),
    ]);
  return gen.MakeGenericType([generic]);
}

export function list(generic: NTypeInfo): NTypeInfo {
  const gen = declareNType("Microsoft.FSharp.Collections.FSharpList`1", 1, (self, gen) => [
    new NPropertyInfo(self, "Head", gen[0], false, false, []),
    new NMethodInfo(self, [], "get_Head", [], gen[0], false, ((l) => l[0]), []),
  ]);
  return gen.MakeGenericType([generic]);
  // return new NTypeInfo("Microsoft.FSharp.Collections.FSharpList`1", 1, false, [generic], (_s) => []);
}

export function array(generic: NTypeInfo): NTypeInfo {
  const gen = declareNType("_[]", 1, (self, _gen) => [
    new NPropertyInfo(self, "Length", int32, false, false, []),
    new NMethodInfo(self, [], "get_Length", [], int32, false, ((l) => l.length), []),
  ]);
  return gen.MakeGenericType([generic]);
  // generic = generic || getGenericParameter("a");
  // return new NTypeInfo(generic.fullname + "[]", 1, false, [generic], (_s) => []);
}

export function enumType(fullname: string, underlyingType: NTypeInfo, enumCases: NEnumCase[]): NTypeInfo {
  return new NTypeInfo(fullname, 1, false, [underlyingType], (_) => [], null, null, enumCases);
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

export function isType(o: any) {
  return o != null && "fullname" in o;
}
export function isMemberInfo(o: any) {
  return o != null && "getMemberKind" in o;
}
export function isMethodBase(o: any) {
  if (isMemberInfo(o)) {
    const k = o.getMemberKind();
    return k === NReflKind.Method || k === NReflKind.Constructor;
  } else {
    return false;
  }
}
export function isMethodInfo(o: any) {
  return o != null && isMemberInfo(o) && o.getMemberKind() === NReflKind.Method;
}
export function isPropertyInfo(o: any) {
  return o != null && isMemberInfo(o) && o.getMemberKind() === NReflKind.Property;
}
export function isUnionCaseInfo(o: any) {
  return o != null && isMemberInfo(o) && o.getMemberKind() === NReflKind.UnionCase;
}
export function isFieldInfo(o: any) {
  return o != null && isMemberInfo(o) && o.getMemberKind() === NReflKind.Field;
}
export function isConstructorInfo(o: any) {
  return o != null && isMemberInfo(o) && o.getMemberKind() === NReflKind.Constructor;
}

export function isEnum(t: NTypeInfo) {
  return t.enumCases != null && t.enumCases.length > 0;
}

export function getEnumUnderlyingType(t: NTypeInfo) {
  return t.generics[0];
}

export function getEnumValues(t: NTypeInfo): number[] {
  if (t.enumCases) {
    return t.enumCases.map((kv) => kv[1]);
  } else {
    throw new Error(`${t.fullname} is not an enum type`);
  }
}

export function getEnumNames(t: NTypeInfo): string[] {
  if (t.enumCases) {
    return t.enumCases.map((kv) => kv[0]);
  } else {
    throw new Error(`${t.fullname} is not an enum type`);
  }
}

function getEnumCase(t: NTypeInfo, v: number | string): NEnumCase {
  if (t.enumCases) {
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

export function parseEnum(t: NTypeInfo, str: string): number {
  // TODO: better int parsing here, parseInt ceils floats: "4.8" -> 4
  const value = parseInt(str, 10);
  return getEnumCase(t, isNaN(value) ? str : value)[1];
}

export function tryParseEnum(t: NTypeInfo, str: string): [boolean, number] {
  try {
    const v = parseEnum(t, str);
    return [true, v];
  } catch {
    // supress error
  }
  return [false, NaN];
}

export function getEnumName(t: NTypeInfo, v: number): string {
  return getEnumCase(t, v)[0];
}

export function isEnumDefined(t: NTypeInfo, v: string | number): boolean {
  try {
    const kv = getEnumCase(t, v);
    return kv[0] != null && kv[0] !== "";
  } catch {
    // supress error
  }
  return false;
}

// FSharpType

export function getUnionCases(t: NTypeInfo): NUnionCaseInfo[] {
  const cases = t.GetAllMembers().filter((m) => isUnionCaseInfo(m)) as NUnionCaseInfo[];
  if (cases.length > 0) {
    return cases;
  } else {
    throw new Error(`${t.fullname} is not an F# union type`);
  }
}

export function getRecordElements(t: NTypeInfo): NPropertyInfo[] {
  const fields = t.GetAllMembers().filter((m) => isPropertyInfo(m) && (m as NPropertyInfo).IsFSharp) as NPropertyInfo[];
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
  if (isType(t)) {
    const idx = (t as NTypeInfo).GetAllMembers().findIndex((m) => isUnionCaseInfo(m));
    return idx >= 0;
  } else {
    return t instanceof Union;
  }
}

export function isRecord(t: any): boolean {
  if (isType(t)) {
    const idx = (t as NTypeInfo).GetAllMembers().findIndex((m) => isPropertyInfo(m) && (m as NPropertyInfo).IsFSharp);
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
  return uci.Fields;
}

export function getRecordFields(v: any): any[] {
  return Object.keys(v).map((k) => v[k]);
}

export function getRecordField(v: any, field: NPropertyInfo): any {
  return v[field.Name];
}

export function anonRecord(...fields: Array<[string, NTypeInfo]>): NTypeInfo {
  const fullName = fields.map((nt) => nt[0] + nt[1].toFullString()).join("_");
  return declareNType(fullName, 0, (self, _) => {
    const mems = fields.map((tt) => new NPropertyInfo(self, tt[0], tt[1], false, true, [], ((target) => target[tt[0]])) as NMemberInfo);

    const pars = fields.map ((tt) => new NParameterInfo(tt[0], tt[1]));

    function makeObj(args: any[]) {
      if (args.length !== fields.length) { throw new Error(`mismatching argument count for anon record ${args.length} expected ${fields.length}`); }
      const res: any = {};
      for (let i = 0; i < fields.length; i++) {
        res[fields[i][0]] = args[i];
      }
      return res;
    }

    const ctor = new NConstructorInfo(self, pars, (args) => makeAnonRecord(makeObj(args)), []);
    mems.push(ctor as NMemberInfo);

    return mems;
  });
}

export function getTupleFields(v: any): any[] {
  return v;
}

export function getTupleField(v: any, i: number): any {
  return v[i];
}

export function makeUnion(uci: NUnionCaseInfo, values: any[]): any {
  return uci.Invoke.apply(null, values);
}

export function makeRecord(t: NTypeInfo, values: any[]): any {
  const fields = getRecordElements(t);
  if (fields.length !== values.length) {
    throw new Error(`Expected an array of length ${fields.length} but got ${values.length}`);
  }
  const ctor = t.GetAllMembers().find((m) => isConstructorInfo(m)) as NConstructorInfo;
  return ctor.Invoke(values);
}

export function makeTuple(values: any[], _: NTypeInfo): any {
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

export function createMethod(decl: NTypeInfo, name: string, mpars: string[], margs: NTypeInfo[], declaredArgs: NTypeInfo[], ret: NTypeInfo, isStatic: boolean): NMethodInfo {
  const found =
    decl.GetMethods().find((m) =>
      m.Name === name && m.GenericArguments.length === margs.length &&
      m.Parameters.length === declaredArgs.length && m.IsStatic === isStatic &&
      (m.get_IsGenericMethod() ? m.MakeGenericMethod(margs).ParametersAssignable(declaredArgs) : m.ParametersAssignable(declaredArgs)) &&
      equals(m.ReturnType, ret),
    );
  if (found) {
    return found.get_IsGenericMethod() ? found.MakeGenericMethod(margs) : found;
  } else {
    const pp = mpars.map ((n) => getGenericParameter(n));
    const meth = new NMethodInfo(decl, pp, name, declaredArgs.map((a, i) => new NParameterInfo("arg" + i, a)), ret, isStatic, ((_target, _args) => { throw new Error("cannot invoke " + decl.fullname + "." + name); }), []);
    // TODO: Is this necessary? I don't see `createMethod` in use anywhere
    // decl.cachedMembers.push(meth);
    return meth.get_IsGenericMethod() ? meth.MakeGenericMethod(margs) : meth;
  }
}
