import { IObservable } from "./Observable.js";
import { Option } from "./Option.js";
import { FSharpChoice$2_$union } from "./Choice.js";
export type Handler<T> = (sender: any, x: T) => void;
export interface IDelegateEvent<Delegate extends Function> {
    AddHandler(d: Delegate): void;
    RemoveHandler(d: Delegate): void;
}
export interface IEvent$2<Delegate extends Function, Args> extends IDelegateEvent<Delegate>, IObservable<Args> {
}
export type IEvent<T> = IEvent$2<Handler<T>, T>;
export declare class Event$2<Delegate extends Function, Args> {
    private delegates;
    private _add;
    private _remove;
    get Publish(): IEvent$2<Delegate, Args>;
    Trigger(value: Args): void;
    Trigger(sender: any, value: Args): void;
}
export declare class Event<T> extends Event$2<Handler<T>, T> {
}
export declare function add<Del extends Function, T>(callback: (x: T) => void, sourceEvent: IEvent$2<Del, T>): void;
export declare function choose<Del extends Function, T, U>(chooser: (x: T) => Option<U>, sourceEvent: IEvent$2<Del, T>): IEvent<U>;
export declare function filter<Del extends Function, T>(predicate: (x: T) => boolean, sourceEvent: IEvent$2<Del, T>): IEvent<T>;
export declare function map<Del extends Function, T, U>(mapping: (x: T) => U, sourceEvent: IEvent$2<Del, T>): IEvent<U>;
export declare function merge<Del1 extends Function, Del2 extends Function, T>(event1: IEvent$2<Del1, T>, event2: IEvent$2<Del2, T>): IEvent<T>;
export declare function pairwise<Del extends Function, T>(sourceEvent: IEvent$2<Del, T>): IEvent<[T, T]>;
export declare function partition<Del extends Function, T>(predicate: (x: T) => boolean, sourceEvent: IEvent$2<Del, T>): [IEvent<T>, IEvent<T>];
export declare function scan<Del extends Function, U, T>(collector: (u: U, t: T) => U, state: U, sourceEvent: IEvent$2<Del, T>): IEvent<U>;
export declare function split<Del extends Function, T, U1, U2>(splitter: (x: T) => FSharpChoice$2_$union<U1, U2>, sourceEvent: IEvent$2<Del, T>): [IEvent<U1>, IEvent<U2>];
export declare function createEvent<Del extends Function, T>(addHandler: (h: Del) => void, removeHandler: (h: Del) => void): IEvent$2<Del, T>;
