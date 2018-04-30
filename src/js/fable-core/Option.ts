import { compare, equals, toString } from "./Util";

// Options are erased in runtime by Fable, but we have
// the `Some` type below to wrap values that would evaluate
// to null in runtime. These two rules must be followed:

// 1- None is always null in runtime, a non-strict null check
//    (`x == null`) is enough to check the case of an option.
// 2- To get the value of an option the `getValue` helper
//    below must **always** be used.

export type Option<T> = T | Some<T>;

export class Some<T> {
    constructor(public value: T) {
    }

    // TODO!!! toJSON
    // We don't prefix it with "Some" for consistency with erased options
    public toString() {
        return toString(this.value);
    }

    public Equals(other: Option<T>) {
        if (other == null) {
            return false;
        } else {
            return equals(this.value, other instanceof Some
                ? other.value : other);
        }
    }

    public CompareTo(other: Option<T>) {
        if (other == null) {
            return 1;
        } else {
            return compare(this.value, other instanceof Some
                ? other.value : other);
        }
    }
}

export function some<T>(x: T): Option<T> {
    return x == null || x instanceof Some ? new Some(x) : x;
}

export function value<T>(x: Option<T>, acceptNull?: boolean): T {
    if (x == null) {
        if (!acceptNull) {
            throw new Error("Option has no value");
        }
        return null;
    } else {
        return x instanceof Some ? x.value : x;
    }
}

export function defaultArg<T, U>(arg: T, defaultValue: T, f?: (x: T) => U) {
    return arg == null ? defaultValue : (f != null ? f(value(arg)) : value(arg));
}

export function defaultArgWith<T, U>(arg: T, defThunk: () => T) {
    return arg == null ? defThunk() : value(arg);
}

export function filter<T>(predicate: (x: T) => boolean, arg: T) {
    return arg != null ? (!predicate(value(arg)) ? null : arg) : arg;
}
