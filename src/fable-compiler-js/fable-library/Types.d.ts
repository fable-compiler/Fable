import { IComparable, IEquatable, IHashable } from "./Util.js";
export type Result<T> = {
    tag: "ok";
    value: T;
} | {
    tag: "error";
    error: string;
};
export declare function seqToString<T>(self: Iterable<T>): string;
export declare function toString(x: any, callStack?: number): string;
export declare function unionToString(name: string, fields: any[]): string;
export declare abstract class Union<Tag extends number, Name extends string> implements IEquatable<Union<Tag, Name>>, IComparable<Union<Tag, Name>> {
    abstract readonly tag: Tag;
    abstract readonly fields: any[];
    abstract cases(): string[];
    get name(): Name;
    toJSON(): Name | Name[];
    toString(): string;
    GetHashCode(): number;
    Equals(other: Union<Tag, Name>): boolean;
    CompareTo(other: Union<Tag, Name>): number;
}
export declare abstract class Record implements IEquatable<Record>, IComparable<Record>, IHashable {
    toJSON(): any;
    toString(): string;
    GetHashCode(): number;
    Equals(other: Record): boolean;
    CompareTo(other: Record): number;
}
export declare class FSharpRef<T> {
    private readonly getter;
    private readonly setter;
    get contents(): T;
    set contents(v: T);
    constructor(contentsOrGetter: T | (() => T), setter?: (v: T) => void);
}
export declare class Exception {
    message?: string | undefined;
    constructor(message?: string | undefined);
}
export declare function isException(x: any): boolean;
export declare function isPromise(x: any): boolean;
export declare function ensureErrorOrException(e: any): any;
export declare abstract class FSharpException extends Exception implements IEquatable<FSharpException>, IComparable<FSharpException> {
    toJSON(): any;
    toString(): string;
    GetHashCode(): number;
    Equals(other: FSharpException): boolean;
    CompareTo(other: FSharpException): number;
}
export declare class MatchFailureException extends FSharpException {
    arg1: string;
    arg2: number;
    arg3: number;
    constructor(arg1: string, arg2: number, arg3: number);
}
export declare class Attribute {
}
