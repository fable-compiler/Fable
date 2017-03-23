import FableSymbol from "./Symbol"
import { getType } from "./Symbol"
import List from "./List"
import { ofArray as listOfArray } from "./List"
import FableSet from "./Set"
import FableMap from "./Map"
import { create as mapCreate } from "./Map"
import { create as setCreate } from "./Set"
import { NonDeclaredType, getDefinition } from "./Util"
import { fold } from "./Seq"
import { resolveGeneric, getTypeFullName } from "./Reflection"
import { parse as dateParse } from "./Date"
import { fsFormat } from "./String"

export function deflate(v: any) {
  if (ArrayBuffer.isView(v)) {
    return Array.from(v as any);
  }
  else if (v != null && typeof v === "object") {
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

    const reflectionInfo = typeof v[FableSymbol.reflection] === "function" ? v[FableSymbol.reflection]() : {};
    if (reflectionInfo.properties) {
      return fold((o: any, prop: string) => {
        return o[prop] = v[prop], o;
      }, {}, Object.getOwnPropertyNames(reflectionInfo.properties));
    }
    else if (reflectionInfo.cases) {
      const caseInfo = reflectionInfo.cases[v.tag],
            caseName = caseInfo[0],
            fieldsLength = caseInfo.length - 1;
      if (fieldsLength === 0) {
        return caseName;
      }
      else if (fieldsLength === 1) {
        // Prevent undefined assignment from removing case property; see #611:
        const fieldValue = typeof v.fields[0] === 'undefined' ? null : v.fields[0];
        return { [caseName]: fieldValue };
      }
      else {
        return { [caseName]: v.fields };
      }
    }
  }
  return v;
}

export function toJson(o: any): string {
  return JSON.stringify(o, (k, v) => deflate(v));
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

function inflateUnion(val: any, typ: FunctionConstructor, info: any, path: string, inflateField?: Function) {
  let newVal: any, caseName: string;
  // Same shape as runtime DUs, for example, if they've been serialized with `JSON.stringify`
  if (typeof val.tag === "number") {
    newVal = new typ();
    return Object.assign(newVal, val);
  }
  // Cases without fields are serialized as strings by `toJson`
  else if (typeof val === "string") {
    caseName = val;
  }
  // Non-empty cases are serialized as `{ "MyCase": [1, 2] }` by `toJson`
  else {
    caseName = Object.getOwnPropertyNames(val)[0];
  }
  // Locate case index
  let tag = -1, i = -1;
  while (info.cases[++i] != null) {
    if (info.cases[i][0] === caseName) {
      tag = i;
      break;
    }
  }
  // Validate
  if (tag === -1) {
    invalidate(val, typ, path);
  }
  newVal = new typ(tag as any);
  let caseInfo = info.cases[tag];
  if (caseInfo.length > 1) {
      const fields = caseInfo.length > 2 ? val[caseName] : [val[caseName]];
      path = combine(path, caseName);
      for (let i = 0; i < fields.length; i++) {
          newVal.fields.push(inflateField
            ? inflateField(fields[i], caseInfo[i + 1], combine(path, i))
            : fields[i]);
      }
  }
  return newVal;

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
      return inflateUnion(val, typ, info, path, (fi: any, t: FunctionConstructor, p: string) => inflate(fi, new List(t, enclosing), path));
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
      const info = typeof v[FableSymbol.reflection] === "function" ? v[FableSymbol.reflection]() : {};
      if (v instanceof List || v instanceof FableSet || v instanceof Set) {
        return {
          $type: info.type || "System.Collections.Generic.HashSet",
          $values: Array.from(v) };
      }
      else if (v instanceof FableMap || v instanceof Map) {
        return fold(
          (o: ({ [i:string]: any}), kv: [any,any]) => { o[kv[0]] = kv[1]; return o; },
          { $type: info.type || "System.Collections.Generic.Dictionary" }, v);
      }
      else if (info.properties) {
        return fold((o: any, prop: string) => {
          return o[prop] = v[prop], o;
        }, { $type: info.type }, Object.getOwnPropertyNames(info.properties));
      }
      else if (info.cases) {
        const uci = info.cases[v.tag];
        return {
          [uci[0]]: uci.length <= 2 ? v.fields[0] : v.fields,
          $type: info.type
        };
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
      delete v.$type;
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
        return mapCreate(Object.getOwnPropertyNames(v)
                                  .map(k => [k, v[k]] as [any,any]));
      }
      else if (type == "System.Collections.Generic.Dictionary") {
        return new Map(Object.getOwnPropertyNames(v)
                              .map(k => [k, v[k]] as [any,any]));
      }
      else {
        const typ = getType(type);
        if (typ) {
          const info = typeof (typ.prototype as any)[FableSymbol.reflection] === "function" ?  (typ.prototype as any)[FableSymbol.reflection]() : {};
          if (info.cases) {
             return inflateUnion(v, typ, info, k);
          }
          return Object.assign(new typ(), v);
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
