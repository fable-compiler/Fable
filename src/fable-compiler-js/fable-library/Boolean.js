import { FSharpRef } from "./Types.js";
export function tryParse(str, defValue) {
    if (str.match(/^\s*true\s*$/i)) {
        defValue.contents = true;
        return true;
    }
    else if (str.match(/^\s*false\s*$/i)) {
        defValue.contents = false;
        return true;
    }
    return false;
}
export function parse(str) {
    const defValue = new FSharpRef(false);
    if (tryParse(str, defValue)) {
        return defValue.contents;
    }
    else {
        throw new Error(`String '${str}' was not recognized as a valid Boolean.`);
    }
}
