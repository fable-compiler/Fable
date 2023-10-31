// tslint:disable:ban-types
export function isArrayLike(x) {
    return Array.isArray(x) || ArrayBuffer.isView(x);
}
export function isIterable(x) {
    return x != null && typeof x === "object" && Symbol.iterator in x;
}
export function isEnumerable(x) {
    return x != null && typeof x.GetEnumerator === "function";
}
export function isComparer(x) {
    return x != null && typeof x.Compare === "function";
}
export function isComparable(x) {
    return x != null && typeof x.CompareTo === "function";
}
export function isEquatable(x) {
    return x != null && typeof x.Equals === "function";
}
export function isHashable(x) {
    return x != null && typeof x.GetHashCode === "function";
}
export function isDisposable(x) {
    return x != null && typeof x.Dispose === "function";
}
export function disposeSafe(x) {
    if (isDisposable(x)) {
        x.Dispose();
    }
}
export function defaultOf() {
    return null;
}
export function sameConstructor(x, y) {
    return Object.getPrototypeOf(x)?.constructor === Object.getPrototypeOf(y)?.constructor;
}
export class Enumerable {
    constructor(en) {
        this.en = en;
    }
    GetEnumerator() { return this.en; }
    "System.Collections.IEnumerable.GetEnumerator"() { return this.en; }
    [Symbol.iterator]() {
        return this;
    }
    next() {
        const hasNext = this.en["System.Collections.IEnumerator.MoveNext"]();
        const current = hasNext ? this.en["System.Collections.Generic.IEnumerator`1.get_Current"]() : undefined;
        return { done: !hasNext, value: current };
    }
}
export class Enumerator {
    constructor(iter) {
        this.iter = iter;
        this.current = defaultOf();
    }
    ["System.Collections.Generic.IEnumerator`1.get_Current"]() {
        return this.current;
    }
    ["System.Collections.IEnumerator.get_Current"]() {
        return this.current;
    }
    ["System.Collections.IEnumerator.MoveNext"]() {
        const cur = this.iter.next();
        this.current = cur.value;
        return !cur.done;
    }
    ["System.Collections.IEnumerator.Reset"]() {
        throw new Error("JS iterators cannot be reset");
    }
    Dispose() {
        return;
    }
}
export function toEnumerable(e) {
    if (isEnumerable(e)) {
        return e;
    }
    else {
        return new Enumerable(new Enumerator(e[Symbol.iterator]()));
    }
}
export function getEnumerator(e) {
    if (isEnumerable(e)) {
        return e.GetEnumerator();
    }
    else {
        return new Enumerator(e[Symbol.iterator]());
    }
}
export function toIterator(en) {
    return {
        next() {
            const hasNext = en["System.Collections.IEnumerator.MoveNext"]();
            const current = hasNext ? en["System.Collections.Generic.IEnumerator`1.get_Current"]() : undefined;
            return { done: !hasNext, value: current };
        },
    };
}
export function enumerableToIterator(e) {
    return toIterator(toEnumerable(e).GetEnumerator());
}
export class Comparer {
    constructor(f) {
        this.Compare = f || compare;
    }
}
export function comparerFromEqualityComparer(comparer) {
    // Sometimes IEqualityComparer also implements IComparer
    if (isComparer(comparer)) {
        return new Comparer(comparer.Compare);
    }
    else {
        return new Comparer((x, y) => {
            const xhash = comparer.GetHashCode(x);
            const yhash = comparer.GetHashCode(y);
            if (xhash === yhash) {
                return comparer.Equals(x, y) ? 0 : -1;
            }
            else {
                return xhash < yhash ? -1 : 1;
            }
        });
    }
}
export function assertEqual(actual, expected, msg) {
    if (!equals(actual, expected)) {
        throw Object.assign(new Error(msg || `Expected: ${expected} - Actual: ${actual}`), {
            actual,
            expected,
        });
    }
}
export function assertNotEqual(actual, expected, msg) {
    if (equals(actual, expected)) {
        throw Object.assign(new Error(msg || `Expected: ${expected} - Actual: ${actual}`), {
            actual,
            expected,
        });
    }
}
export class Lazy {
    constructor(factory) {
        this.factory = factory;
        this.isValueCreated = false;
    }
    get Value() {
        if (!this.isValueCreated) {
            this.createdValue = this.factory();
            this.isValueCreated = true;
        }
        return this.createdValue;
    }
    get IsValueCreated() {
        return this.isValueCreated;
    }
}
export function lazyFromValue(v) {
    return new Lazy(() => v);
}
export function padWithZeros(i, length) {
    let str = i.toString(10);
    while (str.length < length) {
        str = "0" + str;
    }
    return str;
}
export function padLeftAndRightWithZeros(i, lengthLeft, lengthRight) {
    let str = i.toString(10);
    while (str.length < lengthLeft) {
        str = "0" + str;
    }
    while (str.length < lengthRight) {
        str = str + "0";
    }
    return str;
}
export function dateOffset(date) {
    const date1 = date;
    return typeof date1.offset === "number"
        ? date1.offset
        : (date.kind === 1 /* DateKind.UTC */
            ? 0 : date.getTimezoneOffset() * -60000);
}
export function int16ToString(i, radix) {
    i = i < 0 && radix != null && radix !== 10 ? 0xFFFF + i + 1 : i;
    return i.toString(radix);
}
export function int32ToString(i, radix) {
    i = i < 0 && radix != null && radix !== 10 ? 0xFFFFFFFF + i + 1 : i;
    return i.toString(radix);
}
export function int64ToString(i, radix) {
    i = i < 0 && radix != null && radix !== 10 ? 0xffffffffffffffffn + i + 1n : i;
    return i.toString(radix);
}
class ObjectRef {
    static id(o) {
        if (!ObjectRef.idMap.has(o)) {
            ObjectRef.idMap.set(o, ++ObjectRef.count);
        }
        return ObjectRef.idMap.get(o);
    }
}
ObjectRef.idMap = new WeakMap();
ObjectRef.count = 0;
export { ObjectRef };
export function stringHash(s) {
    let i = 0;
    let h = 5381;
    const len = s.length;
    while (i < len) {
        h = (h * 33) ^ s.charCodeAt(i++);
    }
    return h;
}
export function numberHash(x) {
    return x * 2654435761 | 0;
}
export function bigintHash(x) {
    return stringHash(x.toString(32));
}
// From https://stackoverflow.com/a/37449594
export function combineHashCodes(hashes) {
    let h1 = 0;
    const len = hashes.length;
    for (let i = 0; i < len; i++) {
        const h2 = hashes[i];
        h1 = ((h1 << 5) + h1) ^ h2;
    }
    return h1;
}
export function physicalHash(x) {
    if (x == null) {
        return 0;
    }
    switch (typeof x) {
        case "boolean":
            return x ? 1 : 0;
        case "number":
            return numberHash(x);
        case "bigint":
            return bigintHash(x);
        case "string":
            return stringHash(x);
        default:
            return numberHash(ObjectRef.id(x));
    }
}
export function identityHash(x) {
    if (isHashable(x)) {
        return x.GetHashCode();
    }
    else {
        return physicalHash(x);
    }
}
export function dateHash(x) {
    return x.getTime();
}
export function arrayHash(x) {
    const len = x.length;
    const hashes = new Array(len);
    for (let i = 0; i < len; i++) {
        hashes[i] = structuralHash(x[i]);
    }
    return combineHashCodes(hashes);
}
export function structuralHash(x) {
    if (x == null) {
        return 0;
    }
    switch (typeof x) {
        case "boolean":
            return x ? 1 : 0;
        case "number":
            return numberHash(x);
        case "bigint":
            return bigintHash(x);
        case "string":
            return stringHash(x);
        default: {
            if (isHashable(x)) {
                return x.GetHashCode();
            }
            else if (isArrayLike(x)) {
                return arrayHash(x);
            }
            else if (x instanceof Date) {
                return dateHash(x);
            }
            else if (Object.getPrototypeOf(x)?.constructor === Object) {
                // TODO: check call-stack to prevent cyclic objects?
                const hashes = Object.values(x).map((v) => structuralHash(v));
                return combineHashCodes(hashes);
            }
            else {
                // Classes don't implement GetHashCode by default, but must use identity hashing
                return numberHash(ObjectRef.id(x));
                // return stringHash(String(x));
            }
        }
    }
}
// Intended for custom numeric types, like long or decimal
export function fastStructuralHash(x) {
    return stringHash(String(x));
}
// Intended for declared types that may or may not implement GetHashCode
export function safeHash(x) {
    // return x == null ? 0 : isHashable(x) ? x.GetHashCode() : numberHash(ObjectRef.id(x));
    return identityHash(x);
}
export function equalArraysWith(x, y, eq) {
    if (x == null) {
        return y == null;
    }
    if (y == null) {
        return false;
    }
    if (x.length !== y.length) {
        return false;
    }
    for (let i = 0; i < x.length; i++) {
        if (!eq(x[i], y[i])) {
            return false;
        }
    }
    return true;
}
export function equalArrays(x, y) {
    return equalArraysWith(x, y, equals);
}
function equalObjects(x, y) {
    const xKeys = Object.keys(x);
    const yKeys = Object.keys(y);
    if (xKeys.length !== yKeys.length) {
        return false;
    }
    xKeys.sort();
    yKeys.sort();
    for (let i = 0; i < xKeys.length; i++) {
        if (xKeys[i] !== yKeys[i] || !equals(x[xKeys[i]], y[yKeys[i]])) {
            return false;
        }
    }
    return true;
}
export function physicalEquality(x, y) {
    return x === y;
}
export function equals(x, y) {
    if (x === y) {
        return true;
    }
    else if (x == null) {
        return y == null;
    }
    else if (y == null) {
        return false;
    }
    else if (isEquatable(x)) {
        return x.Equals(y);
    }
    else if (isArrayLike(x)) {
        return isArrayLike(y) && equalArrays(x, y);
    }
    else if (typeof x !== "object") {
        return false;
    }
    else if (x instanceof Date) {
        return (y instanceof Date) && compareDates(x, y) === 0;
    }
    else {
        return Object.getPrototypeOf(x)?.constructor === Object && equalObjects(x, y);
    }
}
export function compareDates(x, y) {
    let xtime;
    let ytime;
    // DateTimeOffset and DateTime deals with equality differently.
    if ("offset" in x && "offset" in y) {
        xtime = x.getTime();
        ytime = y.getTime();
    }
    else {
        xtime = x.getTime() + dateOffset(x);
        ytime = y.getTime() + dateOffset(y);
    }
    return xtime === ytime ? 0 : (xtime < ytime ? -1 : 1);
}
export function comparePrimitives(x, y) {
    return x === y ? 0 : (x < y ? -1 : 1);
}
export function compareArraysWith(x, y, comp) {
    if (x == null) {
        return y == null ? 0 : 1;
    }
    if (y == null) {
        return -1;
    }
    if (x.length !== y.length) {
        return x.length < y.length ? -1 : 1;
    }
    for (let i = 0, j = 0; i < x.length; i++) {
        j = comp(x[i], y[i]);
        if (j !== 0) {
            return j;
        }
    }
    return 0;
}
export function compareArrays(x, y) {
    return compareArraysWith(x, y, compare);
}
function compareObjects(x, y) {
    const xKeys = Object.keys(x);
    const yKeys = Object.keys(y);
    if (xKeys.length !== yKeys.length) {
        return xKeys.length < yKeys.length ? -1 : 1;
    }
    xKeys.sort();
    yKeys.sort();
    for (let i = 0, j = 0; i < xKeys.length; i++) {
        const key = xKeys[i];
        if (key !== yKeys[i]) {
            return key < yKeys[i] ? -1 : 1;
        }
        else {
            j = compare(x[key], y[key]);
            if (j !== 0) {
                return j;
            }
        }
    }
    return 0;
}
export function compare(x, y) {
    if (x === y) {
        return 0;
    }
    else if (x == null) {
        return y == null ? 0 : -1;
    }
    else if (y == null) {
        return 1;
    }
    else if (isComparable(x)) {
        return x.CompareTo(y);
    }
    else if (isArrayLike(x)) {
        return isArrayLike(y) ? compareArrays(x, y) : -1;
    }
    else if (typeof x !== "object") {
        return x < y ? -1 : 1;
    }
    else if (x instanceof Date) {
        return y instanceof Date ? compareDates(x, y) : -1;
    }
    else {
        return Object.getPrototypeOf(x)?.constructor === Object ? compareObjects(x, y) : -1;
    }
}
export function min(comparer, x, y) {
    return comparer(x, y) < 0 ? x : y;
}
export function max(comparer, x, y) {
    return comparer(x, y) > 0 ? x : y;
}
export function clamp(comparer, value, min, max) {
    return (comparer(value, min) < 0) ? min : (comparer(value, max) > 0) ? max : value;
}
export function createAtom(value) {
    let atom = value;
    return (...args) => {
        if (args.length === 0) {
            return atom;
        }
        else {
            atom = args[0];
        }
    };
}
export function createObj(fields) {
    const obj = {};
    for (const kv of fields) {
        obj[kv[0]] = kv[1];
    }
    return obj;
}
export function jsOptions(mutator) {
    const opts = {};
    mutator(opts);
    return opts;
}
export function round(value, digits = 0) {
    const m = Math.pow(10, digits);
    const n = +(digits ? value * m : value).toFixed(8);
    const i = Math.floor(n);
    const f = n - i;
    const e = 1e-8;
    const r = (f > 0.5 - e && f < 0.5 + e) ? ((i % 2 === 0) ? i : i + 1) : Math.round(n);
    return digits ? r / m : r;
}
export function sign(x) {
    return x > 0 ? 1 : x < 0 ? -1 : 0;
}
export function unescapeDataString(s) {
    // https://stackoverflow.com/a/4458580/524236
    return decodeURIComponent((s).replace(/\+/g, "%20"));
}
export function escapeDataString(s) {
    return encodeURIComponent(s).replace(/!/g, "%21")
        .replace(/'/g, "%27")
        .replace(/\(/g, "%28")
        .replace(/\)/g, "%29")
        .replace(/\*/g, "%2A");
}
export function escapeUriString(s) {
    return encodeURI(s);
}
// ICollection.Clear and Count members can be called on Arrays
// or Dictionaries so we need a runtime check (see #1120)
export function count(col) {
    if (isArrayLike(col)) {
        return col.length;
    }
    else {
        let count = 0;
        for (const _ of col) {
            count++;
        }
        return count;
    }
}
export function clear(col) {
    if (isArrayLike(col)) {
        col.splice(0);
    }
    else {
        col.clear();
    }
}
const curried = new WeakMap();
export function uncurry2(f) {
    if (f == null) {
        return null;
    }
    const f2 = (a1, a2) => f(a1)(a2);
    curried.set(f2, f);
    return f2;
}
export function curry2(f) {
    return curried.get(f) ?? ((a1) => (a2) => f(a1, a2));
}
export function uncurry3(f) {
    if (f == null) {
        return null;
    }
    const f2 = (a1, a2, a3) => f(a1)(a2)(a3);
    curried.set(f2, f);
    return f2;
}
export function curry3(f) {
    return curried.get(f)
        ?? ((a1) => (a2) => (a3) => f(a1, a2, a3));
}
export function uncurry4(f) {
    if (f == null) {
        return null;
    }
    const f2 = (a1, a2, a3, a4) => f(a1)(a2)(a3)(a4);
    curried.set(f2, f);
    return f2;
}
export function curry4(f) {
    return curried.get(f)
        ?? ((a1) => (a2) => (a3) => (a4) => f(a1, a2, a3, a4));
}
export function uncurry5(f) {
    if (f == null) {
        return null;
    }
    const f2 = (a1, a2, a3, a4, a5) => f(a1)(a2)(a3)(a4)(a5);
    curried.set(f2, f);
    return f2;
}
export function curry5(f) {
    return curried.get(f)
        ?? ((a1) => (a2) => (a3) => (a4) => (a5) => f(a1, a2, a3, a4, a5));
}
export function uncurry6(f) {
    if (f == null) {
        return null;
    }
    const f2 = (a1, a2, a3, a4, a5, a6) => f(a1)(a2)(a3)(a4)(a5)(a6);
    curried.set(f2, f);
    return f2;
}
export function curry6(f) {
    return curried.get(f)
        ?? ((a1) => (a2) => (a3) => (a4) => (a5) => (a6) => f(a1, a2, a3, a4, a5, a6));
}
export function uncurry7(f) {
    if (f == null) {
        return null;
    }
    const f2 = (a1, a2, a3, a4, a5, a6, a7) => f(a1)(a2)(a3)(a4)(a5)(a6)(a7);
    curried.set(f2, f);
    return f2;
}
export function curry7(f) {
    return curried.get(f)
        ?? ((a1) => (a2) => (a3) => (a4) => (a5) => (a6) => (a7) => f(a1, a2, a3, a4, a5, a6, a7));
}
export function uncurry8(f) {
    if (f == null) {
        return null;
    }
    const f2 = (a1, a2, a3, a4, a5, a6, a7, a8) => f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8);
    curried.set(f2, f);
    return f2;
}
export function curry8(f) {
    return curried.get(f)
        ?? ((a1) => (a2) => (a3) => (a4) => (a5) => (a6) => (a7) => (a8) => f(a1, a2, a3, a4, a5, a6, a7, a8));
}
export function uncurry9(f) {
    if (f == null) {
        return null;
    }
    const f2 = (a1, a2, a3, a4, a5, a6, a7, a8, a9) => f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9);
    curried.set(f2, f);
    return f2;
}
export function curry9(f) {
    return curried.get(f)
        ?? ((a1) => (a2) => (a3) => (a4) => (a5) => (a6) => (a7) => (a8) => (a9) => f(a1, a2, a3, a4, a5, a6, a7, a8, a9));
}
export function uncurry10(f) {
    if (f == null) {
        return null;
    }
    const f2 = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) => f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10);
    curried.set(f2, f);
    return f2;
}
export function curry10(f) {
    return curried.get(f)
        ?? ((a1) => (a2) => (a3) => (a4) => (a5) => (a6) => (a7) => (a8) => (a9) => (a10) => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10));
}
export function uncurry11(f) {
    if (f == null) {
        return null;
    }
    const f2 = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) => f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10)(a11);
    curried.set(f2, f);
    return f2;
}
export function curry11(f) {
    return curried.get(f)
        ?? ((a1) => (a2) => (a3) => (a4) => (a5) => (a6) => (a7) => (a8) => (a9) => (a10) => (a11) => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11));
}
export function uncurry12(f) {
    if (f == null) {
        return null;
    }
    const f2 = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) => f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10)(a11)(a12);
    curried.set(f2, f);
    return f2;
}
export function curry12(f) {
    return curried.get(f)
        ?? ((a1) => (a2) => (a3) => (a4) => (a5) => (a6) => (a7) => (a8) => (a9) => (a10) => (a11) => (a12) => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12));
}
export function uncurry13(f) {
    if (f == null) {
        return null;
    }
    const f2 = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) => f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10)(a11)(a12)(a13);
    curried.set(f2, f);
    return f2;
}
export function curry13(f) {
    return curried.get(f)
        ?? ((a1) => (a2) => (a3) => (a4) => (a5) => (a6) => (a7) => (a8) => (a9) => (a10) => (a11) => (a12) => (a13) => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13));
}
export function uncurry14(f) {
    if (f == null) {
        return null;
    }
    const f2 = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) => f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10)(a11)(a12)(a13)(a14);
    curried.set(f2, f);
    return f2;
}
export function curry14(f) {
    return curried.get(f)
        ?? ((a1) => (a2) => (a3) => (a4) => (a5) => (a6) => (a7) => (a8) => (a9) => (a10) => (a11) => (a12) => (a13) => (a14) => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14));
}
export function uncurry15(f) {
    if (f == null) {
        return null;
    }
    const f2 = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) => f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10)(a11)(a12)(a13)(a14)(a15);
    curried.set(f2, f);
    return f2;
}
export function curry15(f) {
    return curried.get(f)
        ?? ((a1) => (a2) => (a3) => (a4) => (a5) => (a6) => (a7) => (a8) => (a9) => (a10) => (a11) => (a12) => (a13) => (a14) => (a15) => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15));
}
export function uncurry16(f) {
    if (f == null) {
        return null;
    }
    const f2 = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16) => f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10)(a11)(a12)(a13)(a14)(a15)(a16);
    curried.set(f2, f);
    return f2;
}
export function curry16(f) {
    return curried.get(f)
        ?? ((a1) => (a2) => (a3) => (a4) => (a5) => (a6) => (a7) => (a8) => (a9) => (a10) => (a11) => (a12) => (a13) => (a14) => (a15) => (a16) => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16));
}
export function uncurry17(f) {
    if (f == null) {
        return null;
    }
    const f2 = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17) => f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10)(a11)(a12)(a13)(a14)(a15)(a16)(a17);
    curried.set(f2, f);
    return f2;
}
export function curry17(f) {
    return curried.get(f)
        ?? ((a1) => (a2) => (a3) => (a4) => (a5) => (a6) => (a7) => (a8) => (a9) => (a10) => (a11) => (a12) => (a13) => (a14) => (a15) => (a16) => (a17) => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17));
}
export function uncurry18(f) {
    if (f == null) {
        return null;
    }
    const f2 = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18) => f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10)(a11)(a12)(a13)(a14)(a15)(a16)(a17)(a18);
    curried.set(f2, f);
    return f2;
}
export function curry18(f) {
    return curried.get(f)
        ?? ((a1) => (a2) => (a3) => (a4) => (a5) => (a6) => (a7) => (a8) => (a9) => (a10) => (a11) => (a12) => (a13) => (a14) => (a15) => (a16) => (a17) => (a18) => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18));
}
export function uncurry19(f) {
    if (f == null) {
        return null;
    }
    const f2 = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19) => f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10)(a11)(a12)(a13)(a14)(a15)(a16)(a17)(a18)(a19);
    curried.set(f2, f);
    return f2;
}
export function curry19(f) {
    return curried.get(f)
        ?? ((a1) => (a2) => (a3) => (a4) => (a5) => (a6) => (a7) => (a8) => (a9) => (a10) => (a11) => (a12) => (a13) => (a14) => (a15) => (a16) => (a17) => (a18) => (a19) => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19));
}
export function uncurry20(f) {
    if (f == null) {
        return null;
    }
    const f2 = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20) => f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10)(a11)(a12)(a13)(a14)(a15)(a16)(a17)(a18)(a19)(a20);
    curried.set(f2, f);
    return f2;
}
export function curry20(f) {
    return curried.get(f)
        ?? ((a1) => (a2) => (a3) => (a4) => (a5) => (a6) => (a7) => (a8) => (a9) => (a10) => (a11) => (a12) => (a13) => (a14) => (a15) => (a16) => (a17) => (a18) => (a19) => (a20) => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20));
}
// More performant method to copy arrays, see #2352
export function copyToArray(source, sourceIndex, target, targetIndex, count) {
    if (ArrayBuffer.isView(source) && ArrayBuffer.isView(target)) {
        target.set(source.subarray(sourceIndex, sourceIndex + count), targetIndex);
    }
    else {
        for (let i = 0; i < count; ++i) {
            target[targetIndex + i] = source[sourceIndex + i];
        }
    }
}
