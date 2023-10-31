export declare const enum DateKind {
    Unspecified = 0,
    UTC = 1,
    Local = 2
}
export interface IDateTime extends Date {
    kind?: DateKind;
}
export interface IDateTimeOffset extends Date {
    offset?: number;
}
export interface IComparable<T> extends IEquatable<T> {
    CompareTo(x: T): number;
}
export interface IEquatable<T> {
    Equals(x: T): boolean;
}
export interface IHashable {
    GetHashCode(): number;
}
export interface IDisposable {
    Dispose(): void;
}
export interface IComparer<T> {
    Compare(x: T, y: T): number;
}
export interface IEqualityComparer<T> {
    Equals(x: T, y: T): boolean;
    GetHashCode(x: T): number;
}
export interface ICollection<T> extends Iterable<T> {
    readonly Count: number;
    readonly IsReadOnly: boolean;
    Add(item: T): void;
    Clear(): void;
    Contains(item: T): boolean;
    CopyTo(array: T[], arrayIndex: number): void;
    Remove(item: T): boolean;
}
export declare function isArrayLike<T>(x: T | ArrayLike<T> | Iterable<T>): x is T[];
export declare function isIterable<T>(x: T | ArrayLike<T> | Iterable<T>): x is Iterable<T>;
export declare function isEnumerable<T>(x: T | Iterable<T>): x is IEnumerable<T>;
export declare function isComparer<T>(x: T | IComparer<T>): x is IComparer<T>;
export declare function isComparable<T>(x: T | IComparable<T>): x is IComparable<T>;
export declare function isEquatable<T>(x: T | IEquatable<T>): x is IEquatable<T>;
export declare function isHashable<T>(x: T | IHashable): x is IHashable;
export declare function isDisposable<T>(x: T | IDisposable): x is IDisposable;
export declare function disposeSafe(x: any): void;
export declare function defaultOf<T>(): T;
export declare function sameConstructor<T>(x: T, y: T): boolean;
export interface IEnumerator<T> extends IDisposable {
    ["System.Collections.Generic.IEnumerator`1.get_Current"](): T;
    ["System.Collections.IEnumerator.get_Current"](): T;
    ["System.Collections.IEnumerator.MoveNext"](): boolean;
    ["System.Collections.IEnumerator.Reset"](): void;
    Dispose(): void;
}
export interface IEnumerable<T> extends Iterable<T> {
    GetEnumerator(): IEnumerator<T>;
    "System.Collections.IEnumerable.GetEnumerator"(): IEnumerator<any>;
}
export declare class Enumerable<T> implements IEnumerable<T> {
    private en;
    constructor(en: IEnumerator<T>);
    GetEnumerator(): IEnumerator<T>;
    "System.Collections.IEnumerable.GetEnumerator"(): IEnumerator<any>;
    [Symbol.iterator](): this;
    next(): IteratorResult<T, any>;
}
export declare class Enumerator<T> implements IEnumerator<T> {
    private iter;
    private current;
    constructor(iter: Iterator<T>);
    ["System.Collections.Generic.IEnumerator`1.get_Current"](): T;
    ["System.Collections.IEnumerator.get_Current"](): T;
    ["System.Collections.IEnumerator.MoveNext"](): boolean;
    ["System.Collections.IEnumerator.Reset"](): void;
    Dispose(): void;
}
export declare function toEnumerable<T>(e: IEnumerable<T> | Iterable<T>): IEnumerable<T>;
export declare function getEnumerator<T>(e: IEnumerable<T> | Iterable<T>): IEnumerator<T>;
export declare function toIterator<T>(en: IEnumerator<T>): Iterator<T>;
export declare function enumerableToIterator<T>(e: IEnumerable<T> | Iterable<T>): Iterator<T>;
export interface ISet<T> {
    add(value: T): ISet<T>;
    clear(): void;
    delete(value: T): boolean;
    forEach(callbackfn: (value: T, value2: T, set: ISet<T>) => void, thisArg?: any): void;
    has(value: T): boolean;
    readonly size: number;
    [Symbol.iterator](): Iterator<T>;
    entries(): Iterable<[T, T]>;
    keys(): Iterable<T>;
    values(): Iterable<T>;
}
export interface IMap<K, V> {
    clear(): void;
    delete(key: K): boolean;
    forEach(callbackfn: (value: V, key: K, map: IMap<K, V>) => void, thisArg?: any): void;
    get(key: K): V | undefined;
    has(key: K): boolean;
    set(key: K, value: V): IMap<K, V>;
    readonly size: number;
    [Symbol.iterator](): Iterator<[K, V]>;
    entries(): Iterable<[K, V]>;
    keys(): Iterable<K>;
    values(): Iterable<V>;
}
export declare class Comparer<T> implements IComparer<T> {
    Compare: (x: T, y: T) => number;
    constructor(f?: (x: T, y: T) => number);
}
export declare function comparerFromEqualityComparer<T>(comparer: IEqualityComparer<T>): IComparer<T>;
export declare function assertEqual<T>(actual: T, expected: T, msg?: string): void;
export declare function assertNotEqual<T>(actual: T, expected: T, msg?: string): void;
export declare class Lazy<T> {
    private factory;
    private isValueCreated;
    private createdValue?;
    constructor(factory: () => T);
    get Value(): T | undefined;
    get IsValueCreated(): boolean;
}
export declare function lazyFromValue<T>(v: T): Lazy<T>;
export declare function padWithZeros(i: number, length: number): string;
export declare function padLeftAndRightWithZeros(i: number, lengthLeft: number, lengthRight: number): string;
export declare function dateOffset(date: IDateTime | IDateTimeOffset): number;
export declare function int16ToString(i: number, radix?: number): string;
export declare function int32ToString(i: number, radix?: number): string;
export declare function int64ToString(i: bigint, radix?: number): string;
export declare abstract class ObjectRef {
    static id(o: any): any;
    private static idMap;
    private static count;
}
export declare function stringHash(s: string): number;
export declare function numberHash(x: number): number;
export declare function bigintHash(x: bigint): number;
export declare function combineHashCodes(hashes: ArrayLike<number>): number;
export declare function physicalHash<T>(x: T): number;
export declare function identityHash<T>(x: T): number;
export declare function dateHash(x: Date): number;
export declare function arrayHash<T>(x: ArrayLike<T>): number;
export declare function structuralHash<T>(x: T): number;
export declare function fastStructuralHash<T>(x: T): number;
export declare function safeHash<T>(x: T): number;
export declare function equalArraysWith<T>(x: ArrayLike<T>, y: ArrayLike<T>, eq: (x: T, y: T) => boolean): boolean;
export declare function equalArrays<T>(x: ArrayLike<T>, y: ArrayLike<T>): boolean;
export declare function physicalEquality<T>(x: T, y: T): boolean;
export declare function equals<T>(x: T, y: T): boolean;
export declare function compareDates(x: Date | IDateTime | IDateTimeOffset, y: Date | IDateTime | IDateTimeOffset): 0 | 1 | -1;
export declare function comparePrimitives<T>(x: T, y: T): number;
export declare function compareArraysWith<T>(x: ArrayLike<T>, y: ArrayLike<T>, comp: (x: T, y: T) => number): number;
export declare function compareArrays<T>(x: ArrayLike<T>, y: ArrayLike<T>): number;
export declare function compare<T>(x: T, y: T): number;
export declare function min<T>(comparer: (x: T, y: T) => number, x: T, y: T): T;
export declare function max<T>(comparer: (x: T, y: T) => number, x: T, y: T): T;
export declare function clamp<T>(comparer: (x: T, y: T) => number, value: T, min: T, max: T): T;
export declare function createAtom<T>(value: T): (<Args extends [] | [T]>(...args: Args) => Args extends [] ? T : void);
export declare function createObj(fields: Iterable<[string, any]>): any;
export declare function jsOptions(mutator: (x: object) => void): object;
export declare function round(value: number, digits?: number): number;
export declare function sign(x: number): number;
export declare function unescapeDataString(s: string): string;
export declare function escapeDataString(s: string): string;
export declare function escapeUriString(s: string): string;
export declare function count<T>(col: Iterable<T>): number;
export declare function clear<T>(col: Iterable<T>): void;
export declare function uncurry2<T1, T2, TResult>(f: (a1: T1) => (a2: T2) => TResult): (a1: T1, a2: T2) => TResult;
export declare function curry2<T1, T2, TResult>(f: (a1: T1, a2: T2) => TResult): (a1: T1) => (a2: T2) => TResult;
export declare function uncurry3<T1, T2, T3, TResult>(f: (a1: T1) => (a2: T2) => (a3: T3) => TResult): (a1: T1, a2: T2, a3: T3) => TResult;
export declare function curry3<T1, T2, T3, TResult>(f: (a1: T1, a2: T2, a3: T3) => TResult): (a1: T1) => (a2: T2) => (a3: T3) => TResult;
export declare function uncurry4<T1, T2, T3, T4, TResult>(f: (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => TResult): (a1: T1, a2: T2, a3: T3, a4: T4) => TResult;
export declare function curry4<T1, T2, T3, T4, TResult>(f: (a1: T1, a2: T2, a3: T3, a4: T4) => TResult): (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => TResult;
export declare function uncurry5<T1, T2, T3, T4, T5, TResult>(f: (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => TResult): (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5) => TResult;
export declare function curry5<T1, T2, T3, T4, T5, TResult>(f: (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5) => TResult): (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => TResult;
export declare function uncurry6<T1, T2, T3, T4, T5, T6, TResult>(f: (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => TResult): (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6) => TResult;
export declare function curry6<T1, T2, T3, T4, T5, T6, TResult>(f: (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6) => TResult): (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => TResult;
export declare function uncurry7<T1, T2, T3, T4, T5, T6, T7, TResult>(f: (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => TResult): (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7) => TResult;
export declare function curry7<T1, T2, T3, T4, T5, T6, T7, TResult>(f: (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7) => TResult): (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => TResult;
export declare function uncurry8<T1, T2, T3, T4, T5, T6, T7, T8, TResult>(f: (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => TResult): (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8) => TResult;
export declare function curry8<T1, T2, T3, T4, T5, T6, T7, T8, TResult>(f: (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8) => TResult): (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => TResult;
export declare function uncurry9<T1, T2, T3, T4, T5, T6, T7, T8, T9, TResult>(f: (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => TResult): (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9) => TResult;
export declare function curry9<T1, T2, T3, T4, T5, T6, T7, T8, T9, TResult>(f: (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9) => TResult): (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => TResult;
export declare function uncurry10<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, TResult>(f: (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => TResult): (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10) => TResult;
export declare function curry10<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, TResult>(f: (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10) => TResult): (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => TResult;
export declare function uncurry11<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, TResult>(f: (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => (a11: T11) => TResult): (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11) => TResult;
export declare function curry11<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, TResult>(f: (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11) => TResult): (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => (a11: T11) => TResult;
export declare function uncurry12<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, TResult>(f: (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => (a11: T11) => (a12: T12) => TResult): (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12) => TResult;
export declare function curry12<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, TResult>(f: (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12) => TResult): (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => (a11: T11) => (a12: T12) => TResult;
export declare function uncurry13<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, TResult>(f: (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => (a11: T11) => (a12: T12) => (a13: T13) => TResult): (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12, a13: T13) => TResult;
export declare function curry13<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, TResult>(f: (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12, a13: T13) => TResult): (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => (a11: T11) => (a12: T12) => (a13: T13) => TResult;
export declare function uncurry14<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, TResult>(f: (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => (a11: T11) => (a12: T12) => (a13: T13) => (a14: T14) => TResult): (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12, a13: T13, a14: T14) => TResult;
export declare function curry14<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, TResult>(f: (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12, a13: T13, a14: T14) => TResult): (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => (a11: T11) => (a12: T12) => (a13: T13) => (a14: T14) => TResult;
export declare function uncurry15<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, TResult>(f: (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => (a11: T11) => (a12: T12) => (a13: T13) => (a14: T14) => (a15: T15) => TResult): (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12, a13: T13, a14: T14, a15: T15) => TResult;
export declare function curry15<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, TResult>(f: (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12, a13: T13, a14: T14, a15: T15) => TResult): (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => (a11: T11) => (a12: T12) => (a13: T13) => (a14: T14) => (a15: T15) => TResult;
export declare function uncurry16<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, TResult>(f: (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => (a11: T11) => (a12: T12) => (a13: T13) => (a14: T14) => (a15: T15) => (a16: T16) => TResult): (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12, a13: T13, a14: T14, a15: T15, a16: T16) => TResult;
export declare function curry16<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, TResult>(f: (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12, a13: T13, a14: T14, a15: T15, a16: T16) => TResult): (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => (a11: T11) => (a12: T12) => (a13: T13) => (a14: T14) => (a15: T15) => (a16: T16) => TResult;
export declare function uncurry17<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, TResult>(f: (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => (a11: T11) => (a12: T12) => (a13: T13) => (a14: T14) => (a15: T15) => (a16: T16) => (a17: T17) => TResult): (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12, a13: T13, a14: T14, a15: T15, a16: T16, a17: T17) => TResult;
export declare function curry17<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, TResult>(f: (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12, a13: T13, a14: T14, a15: T15, a16: T16, a17: T17) => TResult): (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => (a11: T11) => (a12: T12) => (a13: T13) => (a14: T14) => (a15: T15) => (a16: T16) => (a17: T17) => TResult;
export declare function uncurry18<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, TResult>(f: (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => (a11: T11) => (a12: T12) => (a13: T13) => (a14: T14) => (a15: T15) => (a16: T16) => (a17: T17) => (a18: T18) => TResult): (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12, a13: T13, a14: T14, a15: T15, a16: T16, a17: T17, a18: T18) => TResult;
export declare function curry18<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, TResult>(f: (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12, a13: T13, a14: T14, a15: T15, a16: T16, a17: T17, a18: T18) => TResult): (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => (a11: T11) => (a12: T12) => (a13: T13) => (a14: T14) => (a15: T15) => (a16: T16) => (a17: T17) => (a18: T18) => TResult;
export declare function uncurry19<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, TResult>(f: (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => (a11: T11) => (a12: T12) => (a13: T13) => (a14: T14) => (a15: T15) => (a16: T16) => (a17: T17) => (a18: T18) => (a19: T19) => TResult): (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12, a13: T13, a14: T14, a15: T15, a16: T16, a17: T17, a18: T18, a19: T19) => TResult;
export declare function curry19<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, TResult>(f: (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12, a13: T13, a14: T14, a15: T15, a16: T16, a17: T17, a18: T18, a19: T19) => TResult): (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => (a11: T11) => (a12: T12) => (a13: T13) => (a14: T14) => (a15: T15) => (a16: T16) => (a17: T17) => (a18: T18) => (a19: T19) => TResult;
export declare function uncurry20<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, TResult>(f: (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => (a11: T11) => (a12: T12) => (a13: T13) => (a14: T14) => (a15: T15) => (a16: T16) => (a17: T17) => (a18: T18) => (a19: T19) => (a20: T20) => TResult): (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12, a13: T13, a14: T14, a15: T15, a16: T16, a17: T17, a18: T18, a19: T19, a20: T20) => TResult;
export declare function curry20<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, TResult>(f: (a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12, a13: T13, a14: T14, a15: T15, a16: T16, a17: T17, a18: T18, a19: T19, a20: T20) => TResult): (a1: T1) => (a2: T2) => (a3: T3) => (a4: T4) => (a5: T5) => (a6: T6) => (a7: T7) => (a8: T8) => (a9: T9) => (a10: T10) => (a11: T11) => (a12: T12) => (a13: T13) => (a14: T14) => (a15: T15) => (a16: T16) => (a17: T17) => (a18: T18) => (a19: T19) => (a20: T20) => TResult;
export declare function copyToArray<T>(source: T[], sourceIndex: number, target: T[], targetIndex: number, count: number): void;
