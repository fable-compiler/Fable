import { TypeKind } from "./Util"
import List from "./List"
import FSymbol from "./Symbol"

export function resolveGeneric(idx: string | number, enclosing: List<any>): List<any> {
  let name: string = null;
  try {
    const t = enclosing.head as FunctionConstructor;
    const generics = (<any>t.prototype)[FSymbol.generics]();
    name = typeof idx === "string"
      ? idx : Object.getOwnPropertyNames(generics)[idx];
    const resolved = generics[name];
    return resolved[0] === TypeKind.GenericParam
      ? resolveGeneric(resolved[1], enclosing.tail)
      : new List(resolved, enclosing);
  }
  catch (err) {
    throw `Cannot resolve generic argument ${name}: ${err}`;
  }
}

export function getType(obj: any): any {
  const t = typeof obj;
  switch (t) {
    case "boolean":
    case "number":
    case "string":
    case "function":
      return t;
    default:
      return Object.getPrototypeOf(obj).constructor
  }
}

// TODO: This needs improvement, check namespace for non-custom types?
export function getTypeFullName(typ: any, option?: string): string {
  function trim(fullName: string, option?: string) {
    if (option === "name") {
      const i = fullName.lastIndexOf('.');
      return fullName.substr(i + 1);
    }
    if (option === "namespace") {
      const i = fullName.lastIndexOf('.');
      return i > -1 ? fullName.substr(0, i) : "";
    }
    return fullName;
  }
  if (typeof typ === "string") {
    return typ;
  }
  else if (Array.isArray(typ)) {
      switch (typ[0] as TypeKind) {
        case TypeKind.Unit:
          return "unit";
        case TypeKind.Option:
          return getTypeFullName(typ[1], option) + " option";
        case TypeKind.Array:
          return getTypeFullName(typ[1], option) + "[]";
        case TypeKind.Tuple:
          return (typ[1] as any[]).map(x => getTypeFullName(x, option)).join(" * ");
        case TypeKind.GenericParam:
        case TypeKind.Interface:
          return typ[1];
        case TypeKind.Any:
        default:
          return "unknown";
      }
  }
  else {
    const proto = Object.getPrototypeOf(typ);
    return proto[FSymbol.typeName]
      ? trim(proto[FSymbol.typeName](), option)
      : typ.name || "unknown";
  }
}    