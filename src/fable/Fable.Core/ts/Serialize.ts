import FSymbol from "./Symbol"
import { fableGlobal } from "./Symbol"
import List from "./List"
import { ofArray as listOfArray } from "./List"
import FSet from "./Set"
import FMap from "./Map"
import { create as mapCreate } from "./Map"
import { create as setCreate } from "./Set"
import { hasInterface } from "./Util"
import { getDefinition } from "./Util"
import { TypeKind } from "./Util"
import { fold } from "./Seq"
import { resolveGeneric } from "./Reflection"
import { parse as dateParse } from "./Date"

export function toJson(o: any): string {
  return JSON.stringify(o, (k, v) => {
    if (ArrayBuffer.isView(v)) {
      return Array.from(v as any);
    }
    else if (v != null && typeof v === "object") {
      if (v instanceof List || v instanceof FSet || v instanceof Set) {
        return Array.from(v);
      }
      else if (v instanceof FMap || v instanceof Map) {
        return fold((o: any, kv: [any,any]) => {
          return o[toJson(kv[0])] = kv[1], o;
        }, {}, v);
      }
      else if (!hasInterface(v, "FSharpRecord") && v[FSymbol.properties]) {
        return fold((o: any, prop: string) => {
          return o[prop] = v[prop], o;
        }, {}, Object.getOwnPropertyNames(v[FSymbol.properties]()));
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

function inflate(val: any, typ: any): any {
  function needsInflate(enclosing: List<any>): boolean {
    const typ = enclosing.head;
    if (typeof typ === "string") {
      return false;
    }
    if (Array.isArray(typ)) {
      switch (typ[0] as TypeKind) {
        case TypeKind.Option:
        case TypeKind.Array:
          return needsInflate(new List(typ[1], enclosing));
        case TypeKind.Tuple:
          return Array.isArray(typ[1]) && typ[1].some((x: any) => needsInflate(new List(x, enclosing)));
        case TypeKind.GenericParam:
          return needsInflate(resolveGeneric(typ[1], enclosing.tail));
        default:
          return false;
      }
    }
    return true;
  }
  function inflateArray(arr: any[], enclosing: List<any>): any[] {
    return Array.isArray(arr) && needsInflate(enclosing)
            ? arr.map((x: any) => inflate(x, enclosing))
            : arr;
  }
  function inflateMap(obj: any, keyEnclosing: List<any>, valEnclosing: List<any>): [any, any][] {
    const inflateKey = keyEnclosing.head !== "string";
    const inflateVal = needsInflate(valEnclosing);
    return Object
      .getOwnPropertyNames(obj)
      .map(k => {
        const key = inflateKey ? inflate(JSON.parse(k), keyEnclosing): k;
        const val = inflateVal ? inflate(obj[k], valEnclosing) : obj[k];
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
  if (val == null || typeof typ === "string") {
    return val;
  }
  else if (Array.isArray(typ)) {
    switch (typ[0] as TypeKind) {
      case TypeKind.Unit:
        return null;
      case TypeKind.Option:
        return inflate(val, new List(typ[1], enclosing));
      case TypeKind.Array:
        return inflateArray(val, new List(typ[1], enclosing));
      case TypeKind.Tuple:
        return (typ[1] as any[]).map((x, i) => inflate(val[i], new List(x, enclosing)));
      case TypeKind.GenericParam:
        return inflate(val, resolveGeneric(typ[1], enclosing.tail));
      // case TypeKind.Interface: // case TypeKind.Any:
      default: return val;
    }
  }
  else if (typeof typ === "function") {
    if (typ === Date) {
      return dateParse(val);
    }
    if (typ.prototype instanceof List) {
      return listOfArray(inflateArray(val, resolveGeneric(0, enclosing)));
    }
    if (typ.prototype instanceof FSet) {
      return setCreate(inflateArray(val, resolveGeneric(0, enclosing)));
    }
    if (typ.prototype instanceof Set) {
      return new Set(inflateArray(val, resolveGeneric(0, enclosing)));
    }
    if (typ.prototype instanceof FMap) {
      return mapCreate(inflateMap(val, resolveGeneric(0, enclosing), resolveGeneric(1, enclosing)));
    }
    if (typ.prototype instanceof Map) {
      return new Map(inflateMap(val, resolveGeneric(0, enclosing), resolveGeneric(1, enclosing)));
    }
    // Union types (note this condition is not returning)
    if (typ.prototype[FSymbol.cases]) {
      let u: any = { Fields: [] };
      if (typeof val === "string") {
        u.Case = val;
      }
      else {
        const caseName = Object.getOwnPropertyNames(val)[0];
        const fieldTypes: any[] = typ.prototype[FSymbol.cases]()[caseName];
        const fields = fieldTypes.length > 1 ? val[caseName] : [val[caseName]];
        u.Case = caseName;
        for (let i = 0; i < fieldTypes.length; i++) {
          u.Fields.push(inflate(fields[i], new List(fieldTypes[i], enclosing)));
        }
      }
      return Object.assign(new typ(), u);
    }
    if (typ.prototype[FSymbol.properties]) {
      const properties: {[k:string]:any} = typ.prototype[FSymbol.properties]();
      for (let k of Object.getOwnPropertyNames(properties)) {
        val[k] = inflate(val[k], new List(properties[k], enclosing));
      }
      return Object.assign(new typ(), val);
    }
    return val;
  }
  throw "Unexpected type when deserializing JSON: " + typ;
}

function inflatePublic(val: any, genArgs: any): any {
  return inflate(val, genArgs ? genArgs.T : null);
}
export { inflatePublic as inflate }

export function ofJson(json: any, genArgs: any): any {
  return inflate(JSON.parse(json), genArgs ? genArgs.T : null);
}

export function toJsonWithTypeInfo(o: any): string {
  return JSON.stringify(o, (k, v) => {
    if (ArrayBuffer.isView(v)) {
      return Array.from(v as any);
    }
    else if (v != null && typeof v === "object") {
      if (v instanceof List || v instanceof FSet || v instanceof Set) {
        return {
          $type: (v as any)[FSymbol.typeName] ? (v as any)[FSymbol.typeName]() : "System.Collections.Generic.HashSet",
          $values: Array.from(v) };
      }
      else if (v instanceof FMap || v instanceof Map) {
        return fold(
          (o: ({ [i:string]: any}), kv: [any,any]) => { o[kv[0]] = kv[1]; return o; },
          { $type: (v as any)[FSymbol.typeName] ? (v as any)[FSymbol.typeName]() : "System.Collections.Generic.Dictionary" }, v);
      }
      else if (v[FSymbol.typeName]) {
        if (hasInterface(v, "FSharpUnion", "FSharpRecord")) {
          return Object.assign({ $type: v[FSymbol.typeName]() }, v);
        }
        else {
          const proto = Object.getPrototypeOf(v),
                props = Object.getOwnPropertyNames(proto),
                o = { $type: v[FSymbol.typeName]() } as {[k:string]:any};
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
        const T = fableGlobal.types.get(type);
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
    throw "JSON is not of type " + expected.name + ": " + json;
  }
  return parsed;
}
