import { structuralHash, equals, compare } from "./Util.js";
// Using a class here for better compatibility with TS files importing Some
export class Some {
    constructor(value) {
        this.value = value;
    }
    toJSON() {
        return this.value;
    }
    // Don't add "Some" for consistency with erased options
    toString() {
        return String(this.value);
    }
    GetHashCode() {
        return structuralHash(this.value);
    }
    Equals(other) {
        if (other == null) {
            return false;
        }
        else {
            return equals(this.value, other instanceof Some ? other.value : other);
        }
    }
    CompareTo(other) {
        if (other == null) {
            return 1;
        }
        else {
            return compare(this.value, other instanceof Some ? other.value : other);
        }
    }
}
export function value(x) {
    if (x == null) {
        throw new Error("Option has no value");
    }
    else {
        return x instanceof Some ? x.value : x;
    }
}
export function unwrap(opt) {
    return opt instanceof Some ? opt.value : opt;
}
export function some(x) {
    return x == null || x instanceof Some ? new Some(x) : x;
}
export function ofNullable(x) {
    // This will fail with unit probably, an alternative would be:
    // return x === null ? undefined : (x === undefined ? new Some(x) : x);
    return x == null ? undefined : x;
}
export function toNullable(x) {
    return x == null ? null : value(x);
}
export function flatten(x) {
    return x == null ? undefined : value(x);
}
export function toArray(opt) {
    return (opt == null) ? [] : [value(opt)];
}
export function defaultArg(opt, defaultValue) {
    return (opt != null) ? value(opt) : defaultValue;
}
export function defaultArgWith(opt, defThunk) {
    return (opt != null) ? value(opt) : defThunk();
}
export function orElse(opt, ifNone) {
    return opt == null ? ifNone : opt;
}
export function orElseWith(opt, ifNoneThunk) {
    return opt == null ? ifNoneThunk() : opt;
}
export function filter(predicate, opt) {
    return (opt != null) ? (predicate(value(opt)) ? opt : undefined) : opt;
}
export function map(mapping, opt) {
    return (opt != null) ? some(mapping(value(opt))) : undefined;
}
export function map2(mapping, opt1, opt2) {
    return (opt1 != null && opt2 != null) ? mapping(value(opt1), value(opt2)) : undefined;
}
export function map3(mapping, opt1, opt2, opt3) {
    return (opt1 != null && opt2 != null && opt3 != null) ? mapping(value(opt1), value(opt2), value(opt3)) : undefined;
}
export function bind(binder, opt) {
    return opt != null ? binder(value(opt)) : undefined;
}
export function tryOp(op, arg) {
    try {
        return some(op(arg));
    }
    catch {
        return undefined;
    }
}
