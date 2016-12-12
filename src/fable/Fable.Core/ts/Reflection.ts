import { NonDeclaredType } from "./Util"
import List from "./List"
import FSymbol from "./Symbol"

export function resolveGeneric(idx: string | number, enclosing: List<any>): List<any> {
  try {
    const t = enclosing.head;
    if (t instanceof NonDeclaredType) {
      return resolveGeneric(idx, enclosing.tail)
    }
    else {
      const generics = (<any>t.prototype)[FSymbol.generics]();
      const name = typeof idx === "string"
        ? idx : Object.getOwnPropertyNames(generics)[idx];
      const resolved = generics[name];
      return resolved instanceof NonDeclaredType && resolved.kind === "GenericParam"
        ? resolveGeneric(resolved.name, enclosing.tail)
        : new List(resolved, enclosing);
    }
  }
  catch (err) {
    throw new Error(`Cannot resolve generic argument ${idx}: ${err}`);
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
    if (typeof fullName !== "string") {
      return "unknown";
    }
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
  else if (typ instanceof NonDeclaredType) {
      switch (typ.kind) {
        case "Unit":
          return "unit";
        case "Option":
          return getTypeFullName(typ.generics[0], option) + " option";
        case "Array":
          return getTypeFullName(typ.generics[0], option) + "[]";
        case "Tuple":
          return typ.generics.map(x => getTypeFullName(x, option)).join(" * ");
        case "GenericParam":
        case "Interface":
          return typ.name;
        case "Any":
        default:
          return "unknown";
      }
  }
  else {
    // Attention: this doesn't work with Object.getPrototypeOf
    const proto = typ.prototype;
    return trim(typeof proto[FSymbol.reflection] === "function"
      ? proto[FSymbol.reflection]().type : null, option);
  }
}