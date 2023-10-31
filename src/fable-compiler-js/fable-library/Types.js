import { combineHashCodes, compare, compareArrays, equalArrays, equals, sameConstructor, numberHash, structuralHash } from "./Util.js";
export function seqToString(self) {
    let count = 0;
    let str = "[";
    for (const x of self) {
        if (count === 0) {
            str += toString(x);
        }
        else if (count === 100) {
            str += "; ...";
            break;
        }
        else {
            str += "; " + toString(x);
        }
        count++;
    }
    return str + "]";
}
export function toString(x, callStack = 0) {
    if (x != null && typeof x === "object") {
        if (typeof x.toString === "function") {
            return x.toString();
        }
        else if (Symbol.iterator in x) {
            return seqToString(x);
        }
        else { // TODO: Date?
            const cons = Object.getPrototypeOf(x)?.constructor;
            return cons === Object && callStack < 10
                // Same format as recordToString
                ? "{ " + Object.entries(x).map(([k, v]) => k + " = " + toString(v, callStack + 1)).join("\n  ") + " }"
                : cons?.name ?? "";
        }
    }
    return String(x);
}
export function unionToString(name, fields) {
    if (fields.length === 0) {
        return name;
    }
    else {
        let fieldStr;
        let withParens = true;
        if (fields.length === 1) {
            fieldStr = toString(fields[0]);
            withParens = fieldStr.indexOf(" ") >= 0;
        }
        else {
            fieldStr = fields.map((x) => toString(x)).join(", ");
        }
        return name + (withParens ? " (" : " ") + fieldStr + (withParens ? ")" : "");
    }
}
export class Union {
    get name() {
        return this.cases()[this.tag];
    }
    toJSON() {
        return this.fields.length === 0 ? this.name : [this.name].concat(this.fields);
    }
    toString() {
        return unionToString(this.name, this.fields);
    }
    GetHashCode() {
        const hashes = this.fields.map((x) => structuralHash(x));
        hashes.splice(0, 0, numberHash(this.tag));
        return combineHashCodes(hashes);
    }
    Equals(other) {
        if (this === other) {
            return true;
        }
        else if (!sameConstructor(this, other)) {
            return false;
        }
        else if (this.tag === other.tag) {
            return equalArrays(this.fields, other.fields);
        }
        else {
            return false;
        }
    }
    CompareTo(other) {
        if (this === other) {
            return 0;
        }
        else if (!sameConstructor(this, other)) {
            return -1;
        }
        else if (this.tag === other.tag) {
            return compareArrays(this.fields, other.fields);
        }
        else {
            return this.tag < other.tag ? -1 : 1;
        }
    }
}
function recordToJSON(self) {
    const o = {};
    const keys = Object.keys(self);
    for (let i = 0; i < keys.length; i++) {
        o[keys[i]] = self[keys[i]];
    }
    return o;
}
function recordToString(self) {
    return "{ " + Object.entries(self).map(([k, v]) => k + " = " + toString(v)).join("\n  ") + " }";
}
function recordGetHashCode(self) {
    const hashes = Object.values(self).map((v) => structuralHash(v));
    return combineHashCodes(hashes);
}
function recordEquals(self, other) {
    if (self === other) {
        return true;
    }
    else if (!sameConstructor(self, other)) {
        return false;
    }
    else {
        const thisNames = Object.keys(self);
        for (let i = 0; i < thisNames.length; i++) {
            if (!equals(self[thisNames[i]], other[thisNames[i]])) {
                return false;
            }
        }
        return true;
    }
}
function recordCompareTo(self, other) {
    if (self === other) {
        return 0;
    }
    else if (!sameConstructor(self, other)) {
        return -1;
    }
    else {
        const thisNames = Object.keys(self);
        for (let i = 0; i < thisNames.length; i++) {
            const result = compare(self[thisNames[i]], other[thisNames[i]]);
            if (result !== 0) {
                return result;
            }
        }
        return 0;
    }
}
export class Record {
    toJSON() { return recordToJSON(this); }
    toString() { return recordToString(this); }
    GetHashCode() { return recordGetHashCode(this); }
    Equals(other) { return recordEquals(this, other); }
    CompareTo(other) { return recordCompareTo(this, other); }
}
export class FSharpRef {
    get contents() {
        return this.getter();
    }
    set contents(v) {
        this.setter(v);
    }
    constructor(contentsOrGetter, setter) {
        if (typeof setter === "function") {
            this.getter = contentsOrGetter;
            this.setter = setter;
        }
        else {
            this.getter = () => contentsOrGetter;
            this.setter = (v) => { contentsOrGetter = v; };
        }
    }
}
// EXCEPTIONS
// Exception is intentionally not derived from Error, for performance reasons (see #2160)
export class Exception {
    constructor(message) {
        this.message = message;
    }
}
export function isException(x) {
    return x instanceof Exception || x instanceof Error;
}
export function isPromise(x) {
    return x instanceof Promise;
}
export function ensureErrorOrException(e) {
    // Exceptionally admitting promises as errors for compatibility with React.suspense (see #3298)
    return (isException(e) || isPromise(e)) ? e : new Error(String(e));
}
export class FSharpException extends Exception {
    toJSON() { return recordToJSON(this); }
    toString() { return recordToString(this); }
    GetHashCode() { return recordGetHashCode(this); }
    Equals(other) { return recordEquals(this, other); }
    CompareTo(other) { return recordCompareTo(this, other); }
}
export class MatchFailureException extends FSharpException {
    constructor(arg1, arg2, arg3) {
        super();
        this.arg1 = arg1;
        this.arg2 = arg2 | 0;
        this.arg3 = arg3 | 0;
        this.message = "The match cases were incomplete";
    }
}
export class Attribute {
}
