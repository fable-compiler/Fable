import FSymbol from "./Symbol"
import { getType } from "./Symbol"
import List from "./List"
import { ofArray as listOfArray } from "./List"
import FSet from "./Set"
import FMap from "./Map"
import { create as mapCreate } from "./Map"
import { create as setCreate } from "./Set"
import { hasInterface } from "./Util"
import { getDefinition } from "./Util"
import { NonDeclaredType } from "./Util"
import { fold } from "./Seq"
import { resolveGeneric, getTypeFullName } from "./Reflection"
import { parse as dateParse } from "./Date"
import { fsFormat } from "./String"

export function toJson(o: any): string {
  return JSON.stringify(o, (k, v) => {
    if (ArrayBuffer.isView(v)) {
      return Array.from(v as any);
    }
    else if (v != null && typeof v === "object") {
      const properties = typeof v[FSymbol.reflection] === "function" ? v[FSymbol.reflection]().properties : null;
      if (v instanceof List || v instanceof FSet || v instanceof Set) {
        return Array.from(v);
      }
      else if (v instanceof FMap || v instanceof Map) {
        return fold((o: any, kv: [any,any]) => {
          return o[toJson(kv[0])] = kv[1], o;
        }, {}, v);
      }
      else if (!hasInterface(v, "FSharpRecord") && properties) {
        return fold((o: any, prop: string) => {
          return o[prop] = v[prop], o;
        }, {}, Object.getOwnPropertyNames(properties));
      }
      else if (hasInterface(v, "FSharpUnion")) {
        if (!v.Fields || !v.Fields.length) {
          return v.Case;
        }
        else if (v.Fields.length === 1) {
          return { [v.Case]: v.Fields[0] };
        }
        else {
          return { [v.Case]: v.Fields };
        }
      }
    }
    return v;
  });
}

function combine(path1: string, path2: number | string): string {
  return typeof path2 === "number"
    ? path1 + "["+path2+"]"
    : (path1 ? path1 + "." : "") + path2;
}

function isNullable(typ: any): boolean {
  if (typeof typ === "string") {
    return typ !== "boolean" && typ !== "number";
  }
  else if (typ instanceof NonDeclaredType) {
    return typ.kind !== "Array" && typ.kind !== "Tuple";
  }
  else {
    const info = typeof typ.prototype[FSymbol.reflection] === "function"
      ? typ.prototype[FSymbol.reflection]() : null;
    return info ? info.nullable : true;
  }
}

function invalidate(val: any, typ: any, path: string) {
  throw new Error(`${fsFormat("%A",val)} ${path ? "("+path+")" : ""} is not of type ${getTypeFullName(typ)}`) ;
}

function inflate(val: any, typ: any, path: string): any {
  function needsInflate(enclosing: List<any>): boolean {
    const typ = enclosing.head;
    if (typeof typ === "string") {
      return false;
    }
    if (typ instanceof NonDeclaredType) {
      switch (typ.kind) {
        case "Option":
        case "Array":
          return needsInflate(new List(typ.generics[0], enclosing));
        case "Tuple":
          return typ.generics.some((x: any) => needsInflate(new List(x, enclosing)));
        case "GenericParam":
          return needsInflate(resolveGeneric(typ.name, enclosing.tail));
        default:
          return false;
      }
    }
    return true;
  }
  function inflateArray(arr: any[], enclosing: List<any>, path: string): any[] {
    if (!Array.isArray) {
      invalidate(arr, "array", path);
    }
    // TODO: Validate non-inflated elements
    return needsInflate(enclosing)
            ? arr.map((x: any, i: number) => inflate(x, enclosing, combine(path, i)))
            : arr;
  }
  function inflateMap(obj: any, keyEnclosing: List<any>, valEnclosing: List<any>, path: string): [any, any][] {
    const inflateKey = keyEnclosing.head !== "string";
    const inflateVal = needsInflate(valEnclosing);
    return Object
      .getOwnPropertyNames(obj)
      .map(k => {
        const key = inflateKey ? inflate(JSON.parse(k), keyEnclosing, combine(path, k)): k;
        const val = inflateVal ? inflate(obj[k], valEnclosing, combine(path, k)) : obj[k];
        return [key, val] as [any, any];
      });
  }
  let enclosing: List<any> = null;
  if (typ instanceof List) {
    enclosing = typ;
    typ = typ.head;
  }
  else {
    enclosing = new List(typ, new List());
  }
  if (val == null) {
    if (!isNullable(typ)) {
      invalidate(val, typ, path);
    }
    return val;
  }
  else if (typeof typ === "string") {
    if ((typ === "boolean" || typ === "number" || typ === "string") && (typeof val !== typ)) {
      invalidate(val, typ, path);
    }
    return val;
  }
  else if (typ instanceof NonDeclaredType) {
    switch (typ.kind) {
      case "Unit":
        return null;
      case "Option":
        return inflate(val, new List(typ.generics[0], enclosing), path);
      case "Array":
        return inflateArray(val, new List(typ.generics[0], enclosing), path);
      case "Tuple":
        return typ.generics.map((x, i) => inflate(val[i], new List(x, enclosing), combine(path, i)));
      case "GenericParam":
        return inflate(val, resolveGeneric(typ.name, enclosing.tail), path);
      // case "Interface": // case "Any":
      default: return val;
    }
  }
  else if (typeof typ === "function") {
    const proto = typ.prototype;
    if (typ === Date) {
      return dateParse(val);
    }
    if (proto instanceof List) {
      return listOfArray(inflateArray(val, resolveGeneric(0, enclosing), path));
    }
    if (proto instanceof FSet) {
      return setCreate(inflateArray(val, resolveGeneric(0, enclosing), path));
    }
    if (proto instanceof Set) {
      return new Set(inflateArray(val, resolveGeneric(0, enclosing), path));
    }
    if (proto instanceof FMap) {
      return mapCreate(inflateMap(val, resolveGeneric(0, enclosing), resolveGeneric(1, enclosing), path));
    }
    if (proto instanceof Map) {
      return new Map(inflateMap(val, resolveGeneric(0, enclosing), resolveGeneric(1, enclosing), path));
    }
    const info = typeof proto[FSymbol.reflection] === "function" ? proto[FSymbol.reflection]() : {};
    // Union types
    if (info.cases) {
      let u: any = { Fields: [] };
      if (typeof val === "string") {
        u.Case = val;
      }
      else {
        const caseName = Object.getOwnPropertyNames(val)[0];
        const fieldTypes: any[] = info.cases[caseName];
        if (Array.isArray(fieldTypes)) {
          const fields = fieldTypes.length > 1 ? val[caseName] : [val[caseName]];
          u.Case = caseName;
          path = combine(path, caseName);
          for (let i = 0; i < fieldTypes.length; i++) {
            u.Fields.push(inflate(fields[i], new List(fieldTypes[i], enclosing), combine(path, i)));
          }
        }
      }
      if (u.Case in info.cases === false) {
        invalidate(val, typ, path);
      }
      return Object.assign(new typ(), u);
    }
    if (info.properties) {
      const properties: {[k:string]:any} = info.properties;
      const ks = Object.getOwnPropertyNames(properties);
      for (let i=0; i < ks.length; i++) {
        let k = ks[i];
        val[k] = inflate(val[k], new List(properties[k], enclosing), combine(path, k));
      }
      return Object.assign(new typ(), val);
    }
    return val;
  }
  throw new Error("Unexpected type when deserializing JSON: " + typ);
}

function inflatePublic(val: any, genArgs: any): any {
  return inflate(val, genArgs ? genArgs.T : null, "");
}
export { inflatePublic as inflate }

export function ofJson(json: any, genArgs: any): any {
  return inflate(JSON.parse(json), genArgs ? genArgs.T : null, "");
}

export function toJsonWithTypeInfo(o: any): string {
  return JSON.stringify(o, (k, v) => {
    if (ArrayBuffer.isView(v)) {
      return Array.from(v as any);
    }
    else if (v != null && typeof v === "object") {
      const typeName = typeof v[FSymbol.reflection] === "function" ? v[FSymbol.reflection]().type : null;
      if (v instanceof List || v instanceof FSet || v instanceof Set) {
        return {
          $type: typeName || "System.Collections.Generic.HashSet",
          $values: Array.from(v) };
      }
      else if (v instanceof FMap || v instanceof Map) {
        return fold(
          (o: ({ [i:string]: any}), kv: [any,any]) => { o[kv[0]] = kv[1]; return o; },
          { $type: typeName || "System.Collections.Generic.Dictionary" }, v);
      }
      else if (typeName) {
        if (hasInterface(v, "FSharpUnion") || hasInterface(v, "FSharpRecord")) {
          return Object.assign({ $type: typeName }, v);
        }
        else {
          const proto = Object.getPrototypeOf(v),
                props = Object.getOwnPropertyNames(proto),
                o = { $type: typeName } as {[k:string]:any};
          for (let i = 0; i < props.length; i++) {
            const prop = Object.getOwnPropertyDescriptor(proto, props[i]);
            if (prop.get)
              o[props[i]] = prop.get.apply(v);
          }
          return o;
        }
      }
    }
    return v;
  });
}

export function ofJsonWithTypeInfo(json: any, genArgs: any): any {
  const parsed = JSON.parse(json, (k, v) => {
    if (v == null)
      return v;
    else if (typeof v === "object" && typeof v.$type === "string") {
      // Remove generic args and assembly info added by Newtonsoft.Json
      let type = v.$type.replace('+', '.'), i = type.indexOf('`');
      if (i > -1) {
        type = type.substr(0,i);
      }
      else {
        i = type.indexOf(',');
        type = i > -1 ? type.substr(0,i) : type;
      }
      if (type === "System.Collections.Generic.List" || (type.indexOf("[]") === type.length - 2)) {
          return v.$values;
      }
      if (type === "Microsoft.FSharp.Collections.FSharpList") {
          return listOfArray(v.$values);
      }
      else if (type == "Microsoft.FSharp.Collections.FSharpSet") {
        return setCreate(v.$values);
      }
      else if (type == "System.Collections.Generic.HashSet") {
        return new Set(v.$values);
      }
      else if (type == "Microsoft.FSharp.Collections.FSharpMap") {
        delete v.$type;
        return mapCreate(Object.getOwnPropertyNames(v)
                                  .map(k => [k, v[k]] as [any,any]));
      }
      else if (type == "System.Collections.Generic.Dictionary") {
        delete v.$type;
        return new Map(Object.getOwnPropertyNames(v)
                              .map(k => [k, v[k]] as [any,any]));
      }
      else {
        const T = getType(type);
        if (T) {
          delete v.$type;
          return Object.assign(new T(), v);
        }
      }
    }
    else if (/^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(?:\.\d+)?(?:[+-]\d{2}:\d{2}|Z)$/.test(v))
      return dateParse(v);
    else
      return v;
  });
  const expected = genArgs ? genArgs.T : null;
  if (parsed != null && typeof expected === "function"
    && !(parsed instanceof getDefinition(expected as FunctionConstructor))) {
    throw new Error("JSON is not of type " + expected.name + ": " + json);
  }
  return parsed;
}
