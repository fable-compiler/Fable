import { equals } from "./Util.js";
import { Union } from "./Types.js";
const CaseRules = {
    None: 0,
    LowerFirst: 1,
    SnakeCase: 2,
    SnakeCaseAllCaps: 3,
    KebabCase: 4,
};
function dashify(str, separator) {
    return str.replace(/[a-z]?[A-Z]/g, (m) => m.length === 1
        ? m.toLowerCase()
        : m.charAt(0) + separator + m.charAt(1).toLowerCase());
}
function changeCase(str, caseRule) {
    switch (caseRule) {
        case CaseRules.LowerFirst:
            return str.charAt(0).toLowerCase() + str.slice(1);
        case CaseRules.SnakeCase:
            return dashify(str, "_");
        case CaseRules.SnakeCaseAllCaps:
            return dashify(str, "_").toUpperCase();
        case CaseRules.KebabCase:
            return dashify(str, "-");
        case CaseRules.None:
        default:
            return str;
    }
}
export function keyValueList(fields, caseRule = CaseRules.None) {
    const obj = {};
    const definedCaseRule = caseRule;
    function fail(kvPair) {
        throw new Error("Cannot infer key and value of " + String(kvPair));
    }
    function assign(key, caseRule, value) {
        key = changeCase(key, caseRule);
        obj[key] = value;
    }
    for (let kvPair of fields) {
        let caseRule = CaseRules.None;
        if (kvPair == null) {
            fail(kvPair);
        }
        // Deflate unions and use the defined case rule
        if (kvPair instanceof Union) {
            const name = kvPair.cases()[kvPair.tag];
            kvPair = kvPair.fields.length === 0 ? name : [name].concat(kvPair.fields);
            caseRule = definedCaseRule;
        }
        if (Array.isArray(kvPair)) {
            switch (kvPair.length) {
                case 0:
                    fail(kvPair);
                    break;
                case 1:
                    assign(kvPair[0], caseRule, true);
                    break;
                case 2:
                    const value = kvPair[1];
                    assign(kvPair[0], caseRule, value);
                    break;
                default:
                    assign(kvPair[0], caseRule, kvPair.slice(1));
            }
        }
        else if (typeof kvPair === "string") {
            assign(kvPair, caseRule, true);
        }
        else {
            fail(kvPair);
        }
    }
    return obj;
}
// TODO: Move these methods to Map and Set modules
export function containsValue(v, map) {
    for (const kv of map) {
        if (equals(v, kv[1])) {
            return true;
        }
    }
    return false;
}
export function tryGetValue(map, key, defaultValue) {
    if (map.has(key)) {
        defaultValue.contents = map.get(key);
        return true;
    }
    return false;
}
export function addToSet(v, set) {
    if (set.has(v)) {
        return false;
    }
    set.add(v);
    return true;
}
export function addToDict(dict, k, v) {
    if (dict.has(k)) {
        throw new Error("An item with the same key has already been added. Key: " + k);
    }
    dict.set(k, v);
}
export function getItemFromDict(map, key) {
    if (map.has(key)) {
        return map.get(key);
    }
    else {
        throw new Error(`The given key '${key}' was not present in the dictionary.`);
    }
}
