import { inherits, Union } from "./Types";
import { compare, equals, hash, toString } from "./Util";

// Options are erased in runtime by Fable, but we have
// the `Some` type below to wrap values that would evaluate
// to null in runtime. These two rules must be followed:

// 1- None is always null in runtime, a non-strict null check
//    (`x == null`) is enough to check the case of an option.
// 2- To get the value of an option the `getValue` helper
//    below must **always** be used.

// export type Option<T> = T | Some<T>;

export class Some {
    constructor(value) {
        this.value = value;
    }

    // Don't add "Some" for consistency with erased options
    toString() {
        return toString(this.value);
    }

    toJSON() {
        return this.value;
    }

    GetHashCode () {
        return hash(this.value);
    }

    Equals(other) {
        return other == null
            ? false
            : equals(this.value, other instanceof Some ? other.value : other);
    }

    CompareTo(other) {
        return other == null
            ? 1
            : compare(this.value, other instanceof Some ? other.value : other);
    }
}

export function some(x) {
    return x == null || x instanceof Some ? new Some(x) : x;
}

export function value(x, acceptNull) {
    if (x == null) {
        if (!acceptNull) {
            throw new Error("Option has no value");
        }
        return null;
    } else {
        return x instanceof Some ? x.value : x;
    }
}

export function defaultArg(arg, defaultValue, f) {
    return arg == null ? defaultValue : (f != null ? f(value(arg)) : value(arg));
}

export function defaultArgWith(arg, defThunk) {
    return arg == null ? defThunk() : value(arg);
}

export function filter(predicate, arg) {
    return arg != null ? (!predicate(value(arg)) ? null : arg) : arg;
}

// CHOICE

export function Choice(tag, name, field) {
    Union.call(this, tag, name, field);
}
inherits(Choice, Union);

export function choice1(x) {
    return new Choice(0, "Choice1Of2", x);
}

export function choice2(x) {
    return new Choice(1, "Choice2Of2", x);
}

export function tryValueIfChoice1(x) {
    return x.tag === 0 ? some(x.fields[0]) : null;
}

export function tryValueIfChoice2(x) {
    return x.tag === 1 ? some(x.fields[0]) : null;
}

// RESULT

export function Result(tag, name, field) {
    Union.call(this, tag, name, field);
}
inherits(Result, Union);

export function ok(x) {
    return new Result(0, "Ok", x);
}

export function error(x) {
    return new Result(1, "Error", x);
}

export function mapOk(f, result) {
    return result.tag === 0 ? ok(f(result.fields[0])) : result;
}

export function mapError(f, result) {
    return result.tag === 1 ? error(f(result.fields[0])) : result;
}

export function bindOk(f, result) {
    return result.tag === 0 ? f(result.fields[0]) : result;
}