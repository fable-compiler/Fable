import { FSharpChoice$2_$union } from "./Choice.js";
import { Option } from "./Option.js";
import { IDisposable } from "./Util.js";
export interface IObserver<T> {
    OnNext: (x: T) => void;
    OnError: (e: any) => void;
    OnCompleted: () => void;
}
export declare class Observer<T> implements IObserver<T> {
    OnNext: (x: T) => void;
    OnError: (e: any) => void;
    OnCompleted: () => void;
    constructor(onNext: (x: T) => void, onError?: (e: any) => void, onCompleted?: () => void);
}
export interface IObservable<T> {
    Subscribe: (o: IObserver<T>) => IDisposable;
}
declare class Observable<T> implements IObservable<T> {
    Subscribe: (o: IObserver<T>) => IDisposable;
    constructor(subscribe: (o: IObserver<T>) => IDisposable);
}
export declare function add<T>(callback: (x: T) => void, source: IObservable<T>): void;
export declare function choose<T, U>(chooser: (x: T) => Option<U>, source: IObservable<T>): Observable<U>;
export declare function filter<T>(predicate: (x: T) => boolean, source: IObservable<T>): IObservable<T>;
export declare function map<T, U>(mapping: (x: T) => U, source: IObservable<T>): IObservable<U>;
export declare function merge<T>(source1: IObservable<T>, source2: IObservable<T>): IObservable<T>;
export declare function pairwise<T>(source: IObservable<T>): IObservable<[T, T]>;
export declare function partition<T>(predicate: (x: T) => boolean, source: IObservable<T>): [Observable<T>, Observable<T>];
export declare function scan<U, T>(collector: (u: U, t: T) => U, state: U, source: IObservable<T>): IObservable<U>;
export declare function split<T, U1, U2>(splitter: (x: T) => FSharpChoice$2_$union<U1, U2>, source: IObservable<T>): [Observable<U1>, Observable<U2>];
export declare function subscribe<T>(callback: (x: T) => void, source: IObservable<T>): IDisposable;
export {};
