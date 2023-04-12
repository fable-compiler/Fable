import { IObservable, subscribe } from "./Observable.js";
import { Option, some, value } from "./Option.js";
import { FSharpChoice$2_$union, Choice_tryValueIfChoice1Of2, Choice_tryValueIfChoice2Of2 } from "./Choice.js";

export type Handler<T> = (sender: any, x: T) => void;

export interface IDelegateEvent<Delegate extends Function> {
  AddHandler(d: Delegate): void;
  RemoveHandler(d: Delegate): void;
}

export interface IEvent$2<Delegate extends Function, Args> extends IDelegateEvent<Delegate>, IObservable<Args> {
}

export type IEvent<T> = IEvent$2<Handler<T>, T>

export class Event$2<Delegate extends Function, Args> {
  private delegates: Delegate[] = [];

  private _add(d: Delegate) {
    this.delegates.push(d);
  }

  private _remove(d: Delegate) {
    const index = this.delegates.indexOf(d);
    if (index > -1) {
      this.delegates.splice(index, 1);
    }
  }

  get Publish(): IEvent$2<Delegate, Args> {
    return createEvent(h => { this._add(h) }, h => { this._remove(h) });
  }

  public Trigger(value: Args): void;
  public Trigger(sender: any, value: Args): void
  public Trigger(senderOrValue: any, valueOrUndefined?: Args): void {
    let sender: any = null;
    const value = valueOrUndefined === undefined ? senderOrValue as Args : (sender = senderOrValue, valueOrUndefined);
    this.delegates.forEach(f => { f(sender, value) });
  }
}

export class Event<T> extends Event$2<Handler<T>, T> {
}

export function add<Del extends Function, T>(callback: (x: T) => void, sourceEvent: IEvent$2<Del, T>): void {
  subscribe(callback, sourceEvent);
}

export function choose<Del extends Function, T, U>(chooser: (x: T) => Option<U>, sourceEvent: IEvent$2<Del, T>): IEvent<U> {
  const ev = new Event<U>();
  add((t) => {
    const u = chooser(t);
    if (u != null) { ev.Trigger(value(u)); }
  }, sourceEvent);
  return ev.Publish;
}

export function filter<Del extends Function, T>(predicate: (x: T) => boolean, sourceEvent: IEvent$2<Del, T>): IEvent<T> {
  return choose((x) => predicate(x) ? some(x) : undefined, sourceEvent);
}

export function map<Del extends Function, T, U>(mapping: (x: T) => U, sourceEvent: IEvent$2<Del, T>): IEvent<U> {
  const ev = new Event<U>();
  add((t) => ev.Trigger(mapping(t)), sourceEvent);
  return ev.Publish;
}

export function merge<Del1 extends Function, Del2 extends Function, T>(event1: IEvent$2<Del1, T>, event2: IEvent$2<Del2, T>): IEvent<T> {
  const ev = new Event<T>();
  const fn = (x: T) => ev.Trigger(x);
  add(fn, event1);
  add(fn, event2);
  return ev.Publish;
}

export function pairwise<Del extends Function, T>(sourceEvent: IEvent$2<Del, T>): IEvent<[T, T]> {
  const ev = new Event<[T, T]>();
  let last: T;
  let haveLast = false;
  add((next) => {
    if (haveLast) {
      ev.Trigger([last, next]);
    }
    last = next;
    haveLast = true;
  }, sourceEvent);
  return ev.Publish;
}

export function partition<Del extends Function, T>(predicate: (x: T) => boolean, sourceEvent: IEvent$2<Del, T>): [IEvent<T>, IEvent<T>] {
  return [filter(predicate, sourceEvent), filter((x) => !predicate(x), sourceEvent)];
}

export function scan<Del extends Function, U, T>(collector: (u: U, t: T) => U, state: U, sourceEvent: IEvent$2<Del, T>): IEvent<U> {
  return map((t) => state = collector(state, t), sourceEvent);
}

export function split<Del extends Function, T, U1, U2>(splitter: (x: T) => FSharpChoice$2_$union<U1, U2>, sourceEvent: IEvent$2<Del, T>): [IEvent<U1>, IEvent<U2>] {
  return [
    choose((v) => Choice_tryValueIfChoice1Of2(splitter(v)), sourceEvent),
    choose((v) => Choice_tryValueIfChoice2Of2(splitter(v)), sourceEvent),
  ];
}

export function createEvent<Del extends Function, T>(addHandler: (h: Del) => void, removeHandler: (h: Del) => void): IEvent$2<Del, T> {
  return {
    AddHandler(h) { addHandler(h); },
    RemoveHandler(h) { removeHandler(h); },
    Subscribe(r) {
      const h = ((_: any, args: T) => r.OnNext(args)) as unknown as Del;
      addHandler(h);
      return {
        Dispose() { removeHandler(h); }
      };
    }
  };
}
