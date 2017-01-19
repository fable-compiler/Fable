import FableSymbol from "./Symbol"
import { getType } from "./Symbol"
import List from "./List"
import { ofArray as listOfArray } from "./List"
import FableSet from "./Set"
import FableMap from "./Map"
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
      const properties = typeof v[FableSymbol.reflection] === "function" ? v[FableSymbol.reflection]().properties : null;
      if (v instanceof List || v instanceof FableSet || v instanceof Set) {
        return Array.from(v);
      }
      else if (v instanceof FableMap || v instanceof Map) {
        let stringKeys: boolean = null;
        return fold((o: any, kv: [any,any]) => {
          if (stringKeys === null) {
            stringKeys = typeof kv[0] === "string"; 
          }
          o[stringKeys ? kv[0] : toJson(kv[0])] = kv[1];
          return o;
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
          // Prevent undefined assignment from removing case property; see #611:
          const fieldValue = typeof v.Fields[0] === 'undefined' ? null : v.Fields[0];
          return { [v.Case]: fieldValue };
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
    const info = typeof typ.prototype[FableSymbol.reflection] === "function"
      ? typ.prototype[FableSymbol.reflection]() : null;
    return info ? info.nullable : true;
  }
}

function invalidate(val: any, typ: any, path: string) {
  throw new Error(`${fsFormat("%A",val)} ${path ? "("+path+")" : ""} is not of type ${getTypeFullName(typ)}`) ;
}

function needsInflate(enclosing: List<any>): boolean {
  const typ = enclosing.head;
  if (typeof typ === "string") {
    return false;
  }
  if (typ instanceof NonDeclaredType) {
    switch (typ.kind) {
      case "Option":
      case "Array":
        return typ.definition != null || needsInflate(new List(typ.generics, enclosing));
      case "Tuple":
        return (typ.generics as FunctionConstructor[]).some((x: any) =>
          needsInflate(new List(x, enclosing)));
      case "GenericParam":
        return needsInflate(resolveGeneric(typ.definition as string, enclosing.tail));
      case "GenericType":
        return true;
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

function inflateList(val: any, enclosing: List<any>, path: string) {
  let ar = [], li = new List(), cur = val, inf = needsInflate(enclosing);
  while (cur.tail != null) {
    ar.push(inf ? inflate(cur.head, enclosing, path) : cur.head);
    cur = cur.tail;
  }
  ar.reverse();
  for (let i=0; i<ar.length; i++) {
    li = new List(ar[i], li);
  }
  return li;
}

function inflate(val: any, typ: any, path: string): any {
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
        return inflate(val, new List(typ.generics, enclosing), path);
      case "Array":
        if (typ.definition != null) { // Typed arrays
          return new (typ.definition as FunctionConstructor)(val);
        }
        else {
          return inflateArray(val, new List(typ.generics, enclosing), path);
        }
      case "Tuple":
        return (typ.generics as FunctionConstructor[]).map((x, i) =>
          inflate(val[i], new List(x, enclosing), combine(path, i)));
      case "GenericParam":
        return inflate(val, resolveGeneric(typ.definition as string, enclosing.tail), path);
      case "GenericType":
        const def = typ.definition as Function;
        if (def === List) {
          return Array.isArray(val)
            ? listOfArray(inflateArray(val, resolveGeneric(0, enclosing), path))
            : inflateList(val, resolveGeneric(0, enclosing), path);
        }
        // TODO: Should we try to inflate also sets and maps serialized with `JSON.stringify`?
        if (def === FableSet) {
          return setCreate(inflateArray(val, resolveGeneric(0, enclosing), path));
        }
        if (def === Set) {
          return new Set(inflateArray(val, resolveGeneric(0, enclosing), path));
        }
        if (def === FableMap) {
          return mapCreate(inflateMap(val, resolveGeneric(0, enclosing), resolveGeneric(1, enclosing), path));
        }
        if (def === Map) {
          return new Map(inflateMap(val, resolveGeneric(0, enclosing), resolveGeneric(1, enclosing), path));
        }
        return inflate(val, new List(typ.definition, enclosing), path);
      default:  // case "Interface": // case "Any":
        return val;
    }
  }
  else if (typeof typ === "function") {
    if (typ === Date) {
      return dateParse(val);
    }
    const info = typeof typ.prototype[FableSymbol.reflection] === "function" ? typ.prototype[FableSymbol.reflection]() : {};
    // Union types
    if (info.cases) {
      let uCase: string, uFields = [];
      // Cases withouth fields are serialized as strings by `toJson`
      if (typeof val === "string") {
        uCase = val;
      }
      // Same shape as runtime DUs, for example, if they've been serialized with `JSON.stringify`
      else if (typeof val.Case === "string" && Array.isArray(val.Fields)) {
        uCase = val.Case;
        uFields = val.Fields;
      }
      // Non-empty cases are serialized as `{ "MyCase": [1, 2] }` by `toJson`
      else {
        const caseName = Object.getOwnPropertyNames(val)[0];
        const fieldTypes: any[] = info.cases[caseName];
        if (Array.isArray(fieldTypes)) {
          const fields = fieldTypes.length > 1 ? val[caseName] : [val[caseName]];
          uCase = caseName;
          path = combine(path, caseName);
          for (let i = 0; i < fieldTypes.length; i++) {
            uFields.push(inflate(fields[i], new List(fieldTypes[i], enclosing), combine(path, i)));
          }
        }
      }
      // Validate
      if (uCase in info.cases === false) {
        invalidate(val, typ, path);
      }
      return new typ(uCase, uFields);
    }
    if (info.properties) {
      let newObj: any = new typ();
      const properties: {[k:string]:any} = info.properties;
      const ks = Object.getOwnPropertyNames(properties);
      for (let i=0; i < ks.length; i++) {
        let k = ks[i];
        newObj[k] = inflate(val[k], new List(properties[k], enclosing), combine(path, k));
      }
      return newObj;
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
      const typeName = typeof v[FableSymbol.reflection] === "function" ? v[FableSymbol.reflection]().type : null;
      if (v instanceof List || v instanceof FableSet || v instanceof Set) {
        return {
          $type: typeName || "System.Collections.Generic.HashSet",
          $values: Array.from(v) };
      }
      else if (v instanceof FableMap || v instanceof Map) {
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
