import { compare, equals, toString } from "./Util";

// Options are erased in runtime by Fable, but some cases
// (unit, not resolved generics, nested options) are wrapped
// by the following JS type (for Some cases)

// So options in Fable follow these two rules:
// 1- None is always null in runtime, so a non-strict null check
//    (`x == null`) is enough to check the case of an option.
// 2- To get the value of an option the `getValue` helper
//    below must **always** be used.

export class Some<T> {
    constructor(public value: T) {
        this.value = value;
    }

    // We don't prefix it with "Some" for consistency with erased options
    public ToString() {
        return toString(this.value);
    }

    public Equals(other: any) {
        if (other instanceof Some) {
            return equals(this.value, other.value);
        } else if (other == null) {
            return false;
        } else {
            return equals(this.value, other);
        }
    }

    public CompareTo(other: any) {
        if (other instanceof Some) {
            return compare(this.value, other.value);
        } else if (other == null) {
            return 1;
        } else {
            return compare(this.value, other);
        }
    }
}

export function getValue(x: any, force?: boolean): any {
    if (x instanceof Some) {
        return x.value;
    } else if (x == null) {
        if (force) { return x; }
        throw new Error("Option has no value");
    } else {
        return x;
    }
}

export function defaultArg<T, U>(arg: T, defaultValue: T, f?: (x: T) => U) {
    return arg == null ? defaultValue : (f != null ? f(getValue(arg)) : getValue(arg));
}

export function defaultArgWith<T, U>(arg: T, defThunk: () => T) {
    return arg == null ? defThunk() : getValue(arg);
}

export function filter<T>(predicate: (x: T) => boolean, arg: T) {
    return arg != null ? (!predicate(getValue(arg)) ? null : arg) : arg;
}
