import { IEnumerator, IEqualityComparer, IComparer } from "./Util.js";
import { int32 } from "./Int32.js";
import { TypeInfo } from "./Reflection.js";
import { FSharpRef } from "./Types.js";
export declare class Comparer$1<T> implements IComparer<T> {
    constructor();
    Compare(x: T, y: T): int32;
}
export declare function Comparer$1_$reflection(gen0: TypeInfo): TypeInfo;
export declare function Comparer$1_$ctor<T>(): Comparer$1<T>;
export declare function Comparer$1_get_Default<T>(): IComparer<T>;
export declare class EqualityComparer$1<T> implements IEqualityComparer<T> {
    constructor();
    Equals(x: T, y: T): boolean;
    GetHashCode(x: T): int32;
}
export declare function EqualityComparer$1_$reflection(gen0: TypeInfo): TypeInfo;
export declare function EqualityComparer$1_$ctor<T>(): EqualityComparer$1<T>;
export declare function EqualityComparer$1_get_Default<T>(): IEqualityComparer<T>;
export declare class Stack$1<T> implements Iterable<T> {
    contents: T[];
    count: int32;
    constructor(initialContents: T[], initialCount: int32);
    GetEnumerator(): IEnumerator<T>;
    [Symbol.iterator](): Iterator<T>;
    "System.Collections.IEnumerable.GetEnumerator"(): IEnumerator<any>;
}
export declare function Stack$1_$reflection(gen0: TypeInfo): TypeInfo;
export declare function Stack$1_$ctor_Z524259A4<T>(initialCapacity: int32): Stack$1<T>;
export declare function Stack$1_$ctor<T>(): Stack$1<T>;
export declare function Stack$1_$ctor_BB573A<T>(xs: Iterable<T>): Stack$1<T>;
export declare function Stack$1__Ensure_Z524259A4<T>(_: Stack$1<T>, newSize: int32): void;
export declare function Stack$1__get_Count<T>(_: Stack$1<T>): int32;
export declare function Stack$1__Pop<T>(_: Stack$1<T>): T;
export declare function Stack$1__Peek<T>(_: Stack$1<T>): T;
export declare function Stack$1__Contains_2B595<T>(_: Stack$1<T>, x: T): boolean;
export declare function Stack$1__TryPeek_1F3DB691<T>(this$: Stack$1<T>, result: FSharpRef<T>): boolean;
export declare function Stack$1__TryPop_1F3DB691<T>(this$: Stack$1<T>, result: FSharpRef<T>): boolean;
export declare function Stack$1__Push_2B595<T>(this$: Stack$1<T>, x: T): void;
export declare function Stack$1__Clear<T>(_: Stack$1<T>): void;
export declare function Stack$1__TrimExcess<T>(this$: Stack$1<T>): void;
export declare function Stack$1__ToArray<T>(_: Stack$1<T>): T[];
export declare class Queue$1<T> implements Iterable<T> {
    contents: T[];
    count: int32;
    head: int32;
    tail: int32;
    constructor(initialContents: T[], initialCount: int32);
    GetEnumerator(): IEnumerator<T>;
    [Symbol.iterator](): Iterator<T>;
    "System.Collections.IEnumerable.GetEnumerator"(): IEnumerator<any>;
}
export declare function Queue$1_$reflection(gen0: TypeInfo): TypeInfo;
export declare function Queue$1_$ctor_Z524259A4<T>(initialCapacity: int32): Queue$1<T>;
export declare function Queue$1_$ctor<T>(): Queue$1<T>;
export declare function Queue$1_$ctor_BB573A<T>(xs: Iterable<T>): Queue$1<T>;
export declare function Queue$1__get_Count<T>(_: Queue$1<T>): int32;
export declare function Queue$1__Enqueue_2B595<T>(_: Queue$1<T>, value: T): void;
export declare function Queue$1__Dequeue<T>(_: Queue$1<T>): T;
export declare function Queue$1__Peek<T>(_: Queue$1<T>): T;
export declare function Queue$1__TryDequeue_1F3DB691<T>(this$: Queue$1<T>, result: FSharpRef<T>): boolean;
export declare function Queue$1__TryPeek_1F3DB691<T>(this$: Queue$1<T>, result: FSharpRef<T>): boolean;
export declare function Queue$1__Contains_2B595<T>(_: Queue$1<T>, x: T): boolean;
export declare function Queue$1__Clear<T>(_: Queue$1<T>): void;
export declare function Queue$1__TrimExcess<T>(_: Queue$1<T>): void;
export declare function Queue$1__ToArray<T>(_: Queue$1<T>): T[];
export declare function Queue$1__CopyTo_Z3B4C077E<T>(_: Queue$1<T>, target: T[], start: int32): void;
export declare function Queue$1__size<T>(this$: Queue$1<T>): int32;
export declare function Queue$1__toIndex_Z524259A4<T>(this$: Queue$1<T>, i: int32): int32;
export declare function Queue$1__ensure_Z524259A4<T>(this$: Queue$1<T>, requiredSize: int32): void;
export declare function Queue$1__toSeq<T>(this$: Queue$1<T>): Iterable<T>;
