import { compare, equals, toString } from "./Util";

// TODO: Convert Choice-related functions to F# so they don't break
// if we change the DU implementation
export type Choice<T1, T2> = ["Choice1Of2", T1] | ["Choice2Of2", T2];

export function choice1<T1, T2>(x: T1): Choice<T1, T2> {
    return ["Choice1Of2", x];
}

export function choice2<T1, T2>(x: T2): Choice<T1, T2> {
    return ["Choice2Of2", x];
}

export function tryValueIfChoice1<T1, T2>(x: Choice<T1, T2>): Option<T1> {
    return x[0] === "Choice1Of2" ? some(x[1] as T1) : null;
}

export function tryValueIfChoice2<T1, T2>(x: Choice<T1, T2>): Option<T2> {
    return x[0] === "Choice2Of2" ? some(x[1] as T2) : null;
}

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
