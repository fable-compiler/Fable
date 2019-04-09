import { Record, Union } from "./Types";
import { compareArraysWith, equalArraysWith, stringHash } from "./Util";

// tslint:disable: max-line-length

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

  public abstract toPrettyString(): string;
  public get_Name() { return this.Name; }

  public get_DeclaringType() { return this.DeclaringType; }

  public GetCustomAttributes(a: (boolean|NTypeInfo), b?: boolean) {
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
    private invoke: (...args: any[]) => any,
    attributes: CustomAttribute[],
    private declaration?: NMethodInfo,
  ) {
    super(declaringType, name, parameters, isStatic, attributes);

    if (!returnType) { throw new Error(`MethodInfo ${name} does not have a return type`); }

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
  }

  public ResolveGeneric(t: NTypeInfo): NTypeInfo {
    if (t.isGenericParameter) {
      return t.fullname in this.genMap ? this.GenericArguments[this.genMap[t.fullname]] : this.DeclaringType.ResolveGeneric(t);
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
      return this.invoke.apply(null, [target, ...args]);
    } else {
      return this.invoke.apply(null, args);
    }
  }
}

export class NConstructorInfo extends NMethodBase {
  constructor(
    declaringType: NTypeInfo,
    parameters: NParameterInfo[],
    private invoke: (...args: any[]) => any,
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

  public toString() {
    return this.toPrettyString();
  }

  public Invoke(args: any[]) {
    args = args || [];
    return this.invoke.apply(null, args);
  }
}

export class NFieldInfo extends NMemberInfo {
  public Type: NTypeInfo = null;
  constructor(
    declaringType: NTypeInfo,
    name: string,
    type: NTypeInfo,
    public IsStatic: boolean,
    attributes: CustomAttribute[],
  ) {
    super(declaringType, name, attributes);

    if (!type) { throw new Error(`FieldInfo ${name} does not have a type`); }

    this.Type = type.get_ContainsGenericParameters() ? this.DeclaringType.ResolveGeneric(type) : type;
  }
  public get_FieldType() { return this.Type; }
  public get_IsStatic() { return this.IsStatic; }

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

  public toString() {
    return this.toPrettyString();
  }

}

export class NPropertyInfo extends NMemberInfo {
  public Type: NTypeInfo = null;
  constructor(
    DeclaringType: NTypeInfo,
    Name: string,
    type: NTypeInfo,
    public IsStatic: boolean,
    public IsFSharp: boolean,
    attributes: CustomAttribute[],
  ) {
    super(DeclaringType, Name, attributes);

    if (!type) { throw new Error(`FieldInfo ${Name} does not have a type`); }

    this.Type = type.get_ContainsGenericParameters() ? DeclaringType.ResolveGeneric(type) : type;
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

  public GetValue(target: any, index: any[]): any {
    if (this.IsFSharp) {
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
    if (this.IsFSharp) {
      target[this.Name] = value;
    } else {
      index = index || [];
      const s = this.get_SetMethod();
      s.Invoke(target, [...index, value]);
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
  public Fields: Array<[string, NTypeInfo]> = null;

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
    this.Fields = fields.map ((tup) => tup[1].get_ContainsGenericParameters() ? [tup[0], DeclaringType.ResolveGeneric(tup[1])] : tup) as Array<[string, NTypeInfo]>;
  }

  public GetFields() {
    return this.Fields.map((nt) => new NPropertyInfo(this.DeclaringType, nt[0], nt[1], false, true, []));
  }

  public get_Tag() {
    return this.Tag;
  }

  public toPrettyString() {
    const decl = this.DeclaringType.toFullString();
    const fields = this.Fields.map((tup) => tup[0] + ": " + tup[1].toFullString()).join(" * ");
    return decl + "." + this.Name + " of " + fields;
  }

  public toString() {
    return this.toPrettyString();
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
  public declaration: NTypeInfo = null;

  private instantiations: {[i: string]: NTypeInfo} = {};
  private mems: NMemberInfo[] = null;
  private genericMap: {[name: string]: number} = {};

  constructor(
    public fullname: string,
    public genericCount: number,
    public isGenericParameter: boolean,
    _generics: NTypeInfo[],
    public members: (self: NTypeInfo) => NMemberInfo[],
    decl?: NTypeInfo) {
      if (!fullname) { throw new Error("cannot declare type without name"); }
      members = members || ((_s) => []);
      _generics = _generics || [];
      isGenericParameter = isGenericParameter || false;
      genericCount = genericCount || 0;

      const g = _generics.filter((t) => !t.isGenericParameter);
      if (g.length === genericCount) {
        this.generics = g;
        if (decl) {
          this.genericMap = decl.genericMap;
        }
      } else {
        _generics.forEach((g, i) => {
          this.genericMap[g.get_Name()] = i;
        });
        this.generics = _generics;
      }
      if (this.generics.length !== this.genericCount) { throw new Error(`${this.fullname} contains ${this.genericCount} generic parameters but only ${this.generics.length} given.`); }
      this.declaration = decl || null;
    }

    public MakeArrayType(): NTypeInfo {
      return array(this);
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
        return new NTypeInfo(t.fullname, t.genericCount, false, t.generics.map((ta) => this.ResolveGeneric(ta)), t.members, t.declaration);
      } else {
        return t;
      }
    }

    public GetGenericTypeDefinition() {
      if (this.genericCount === 0 || this.get_IsGenericTypeDefinition()) {
        return this;
      } else if (this.declaration) {
        return this.declaration;
      } else {
        throw new Error(`${this.fullname} does not have a proper generic definition`);
      }
    }

  public MakeGenericType(args: NTypeInfo[]) {
    args = args.filter((a) => a);
    if (args.length === 0) { return this; }
    if (args.length !== this.genericCount) { throw new Error(`${this.fullname} contains ${this.genericCount} generic parameters but only ${this.generics.length} given.`); }

    const key = args.map((t) => t.toString()).join(", ");
    if (key in this.instantiations) {
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
  public GetMethod(name: string, types?: NTypeInfo[]) {
      const m = this.GetAllMembers();
      if (types) {
        const meths = m.filter((m) => m instanceof NMethodInfo && m.Name === name && m.ParametersAssignable(types)) as NMethodInfo[];
        return meths.length === 1 ? meths[0] : null;
      } else {
        const meths = m.filter((m) => m instanceof NMethodInfo && m.Name === name) as NMethodInfo[];
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
      .filter((m) => !(m instanceof NMethodInfo) || !(m.Name.startsWith("get_") || m.Name.startsWith("set_")))
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
  let gen: NTypeInfo = null;
  if (fullname in typeCache) {
    gen = typeCache[fullname];
  } else {
    const pars = Array.from({ length: generics }, (_, i) => getGenericParameter("a" + i));

    const mems = members || ((_self, _gen) => []);
    gen = new NTypeInfo(fullname, pars.length, false, pars, (s) => mems(s, pars));
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

// System.Type is not comparable in .NET, but let's implement this
// in case users want to create a dictionary with types as keys
export function compare(t1: NTypeInfo, t2: NTypeInfo): number {
  if (t1.fullname !== t2.fullname) {
    return t1.fullname < t2.fullname ? -1 : 1;
  } else {
    return compareArraysWith(getGenerics(t1), getGenerics(t2), compare);
  }
}

export function ntype(fullname: string, genericNames?: string[], generics?: NTypeInfo[], members?: (self: NTypeInfo, pars: NTypeInfo[]) => NMemberInfo[]): NTypeInfo {
  let gen: NTypeInfo = null;
  generics = generics || [];
  const a = generics.findIndex((t) => !t);
  if (a >= 0) { throw new Error("bad hate occured"); }

  if (fullname in typeCache) {
    gen = typeCache[fullname];
  } else {
    members = members || ((_s, _g) => []);
    genericNames = genericNames || [];
    const b = genericNames.findIndex((t) => !t);
    if (b >= 0) { throw new Error("bad hate occured"); }

    const pars = genericNames.map((n) => getGenericParameter(n));
    gen = new NTypeInfo(fullname, pars.length, false, pars, (s) => members(s, pars));
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

export function tuple(...generics: NTypeInfo[]): NTypeInfo {
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

export function option(generic?: NTypeInfo): NTypeInfo {
  const name = "Microsoft.FSharp.Core.FSharpOption`1";
  const gen =
    declareNType(name, 2, (self, gen) => [
      new NUnionCaseInfo(self, 0, "None", [], [], ((_) => null)),
      new NUnionCaseInfo(self, 1, "Some", [], [["Value", gen[0]]], ((args, ...rest) => args)),
    ]);
  return gen.MakeGenericType([generic]);
}

export function list(generic?: NTypeInfo): NTypeInfo {
  const gen = declareNType("Microsoft.FSharp.Collections.FSharpList`1", 1, (self, gen) => [
    new NPropertyInfo(self, "Head", gen[0], false, false, []),
    new NMethodInfo(self, [], "get_Head", [], gen[0], false, ((l) => l[0]), []),
  ]);
  return gen.MakeGenericType([generic]);
  // return new NTypeInfo("Microsoft.FSharp.Collections.FSharpList`1", 1, false, [generic], (_s) => []);
}

export function array(generic?: NTypeInfo): NTypeInfo {
  const gen = declareNType("_[]", 1, (self, gen) => [
    new NPropertyInfo(self, "Length", int32, false, false, []),
    new NMethodInfo(self, [], "get_Length", [], int32, false, ((l) => l.length), []),
  ]);
  return gen.MakeGenericType([generic]);
  // generic = generic || getGenericParameter("a");
  // return new NTypeInfo(generic.fullname + "[]", 1, false, [generic], (_s) => []);
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
  return ctor.Invoke(values);
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
