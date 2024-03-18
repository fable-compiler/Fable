import { FSharpRef } from "./Types.js"

export function tryParse(str: string, defValue: FSharpRef<boolean>): boolean {
  if (str != null && str.match(/^\s*true\s*$/i)) {
    defValue.contents = true;
    return true;
  } else if (str != null && str.match(/^\s*false\s*$/i)) {
    defValue.contents = false;
    return true;
  }
  return false;
}

export function parse(str: string): boolean {
  const defValue = new FSharpRef(false);

  if (tryParse(str, defValue)) {
    return defValue.contents;
  } else {
    throw new Error(`String '${str}' was not recognized as a valid Boolean.`)
  }
}
