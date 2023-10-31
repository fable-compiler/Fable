export type Nullable<T> = T | null | undefined;
export type Option<T> = T | Some<T> | undefined;
export declare class Some<T> {
    value: T;
    constructor(value: T);
    toJSON(): T;
    toString(): string;
    GetHashCode(): number;
    Equals(other: Option<T>): boolean;
    CompareTo(other: Option<T>): number;
}
export declare function value<T>(x: Option<T>): T;
export declare function unwrap<T>(opt: Option<T>): T | undefined;
export declare function some<T>(x: T): Option<T>;
export declare function ofNullable<T>(x: Nullable<T>): Option<T>;
export declare function toNullable<T>(x: Option<T>): Nullable<T>;
export declare function flatten<T>(x: Option<Option<T>>): T | Some<T> | undefined;
export declare function toArray<T>(opt: Option<T>): T[];
export declare function defaultArg<T>(opt: Option<T>, defaultValue: T): T;
export declare function defaultArgWith<T>(opt: Option<T>, defThunk: () => T): T;
export declare function orElse<T>(opt: Option<T>, ifNone: Option<T>): Option<T>;
export declare function orElseWith<T>(opt: Option<T>, ifNoneThunk: () => Option<T>): Option<T>;
export declare function filter<T>(predicate: (arg: T) => boolean, opt: Option<T>): Option<T>;
export declare function map<T, U>(mapping: (arg: T) => U, opt: Option<T>): Option<U>;
export declare function map2<T1, T2, U>(mapping: (arg1: T1, arg2: T2) => Option<U>, opt1: Option<T1>, opt2: Option<T2>): Option<U>;
export declare function map3<T1, T2, T3, U>(mapping: (arg1: T1, arg2: T2, arg3: T3) => Option<U>, opt1: Option<T1>, opt2: Option<T2>, opt3: Option<T3>): Option<U>;
export declare function bind<T, U>(binder: (arg: T) => Option<U>, opt: Option<T>): Option<U>;
export declare function tryOp<T, U>(op: (x: T) => U, arg: T): Option<U>;
