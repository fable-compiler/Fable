import { IObservable, IObserver, Observer } from "./Observable.js";
import { Option, some, value } from "./Option.js";
import { FSharpChoice$2, Choice_tryValueIfChoice1Of2, Choice_tryValueIfChoice2Of2 } from "./Choice.js";

export type Delegate<T> = (x: T) => void;
export type DotNetDelegate<T> = (sender: any, x: T) => void;
type EventDelegate<T> = Delegate<T> | DotNetDelegate<T>;

export interface IDelegateEvent<T> {
  AddHandler(d: DotNetDelegate<T>): void;
  RemoveHandler(d: DotNetDelegate<T>): void;
}

export interface IEvent<T> extends IObservable<T>, IDelegateEvent<T> {
}

export class Event<T> implements IEvent<T> {
  private delegates: EventDelegate<T>[];

  constructor() {
    this.delegates = [];
  }

  public Add(f: Delegate<T>) {
    this._addHandler(f);
  }

  get Publish() {
    return this;
  }

  public Trigger(value: T): void;
  public Trigger(sender: any, value: T): void;
  public Trigger(senderOrValue: any, valueOrUndefined?: T) {
    let sender: any;
    let value : T;
    if (valueOrUndefined === undefined) {
      sender = null;
      value = senderOrValue;
    } else {
      sender = senderOrValue;
      value = valueOrUndefined;
    }
    this.delegates.forEach((f) => f.length === 1 ? (f as Delegate<T>)(value) : f(sender, value));
  }

  // IDelegateEvent<T> methods

  public AddHandler(handler: DotNetDelegate<T>) {
    this._addHandler(handler);
  }

  public RemoveHandler(handler: DotNetDelegate<T>) {
    this._removeHandler(handler);
  }

  // IObservable<T> methods

  public Subscribe(arg: IObserver<T> | Delegate<T>) {
    const callback = typeof arg === "function"
      ? arg as Delegate<T>
      : (arg as IObserver<T>).OnNext;
    this._addHandler(callback);
    return { Dispose: () => { this._removeHandler(callback); } };
  }

  private _addHandler(f: EventDelegate<T>) {
    this.delegates.push(f);
  }

  private _removeHandler(f: EventDelegate<T>) {
    const index = this.delegates.indexOf(f);
    if (index > -1) {
      this.delegates.splice(index, 1);
    }
  }
}

export function add<T>(callback: (x: T) => void, sourceEvent: IEvent<T>) {
  if (sourceEvent instanceof Event) {
    sourceEvent.Add(callback);
  } else {
    sourceEvent.Subscribe(new Observer(callback));
  }
}

export function choose<T, U>(chooser: (x: T) => Option<U>, sourceEvent: IEvent<T>) {
  const ev = new Event<U>();
  add((t) => {
    const u = chooser(t);
    if (u != null) { ev.Trigger(value(u)); }
  }, sourceEvent);
  return ev;
}

export function filter<T>(predicate: (x: T) => boolean, sourceEvent: IEvent<T>) {
  return choose((x) => predicate(x) ? some(x) : undefined, sourceEvent);
}

export function map<T, U>(mapping: (x: T) => U, sourceEvent: IEvent<T>) {
  const ev = new Event<U>();
  add((t) => ev.Trigger(mapping(t)), sourceEvent);
  return ev;
}

export function merge<T>(event1: IEvent<T>, event2: IEvent<T>) {
  const ev = new Event<T>();
  const fn = (x: T) => ev.Trigger(x);
  add(fn, event1);
  add(fn, event2);
  return ev;
}

export function pairwise<T>(sourceEvent: IEvent<T>) {
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
  return ev;
}

export function partition<T>(predicate: (x: T) => boolean, sourceEvent: IEvent<T>): [IEvent<T>, IEvent<T>] {
  return [filter(predicate, sourceEvent), filter((x) => !predicate(x), sourceEvent)];
}

export function scan<U, T>(collector: (u: U, t: T) => U, state: U, sourceEvent: IEvent<T>) {
  return map((t) => state = collector(state, t), sourceEvent);
}

export function split<T, U1, U2>(splitter: (x: T) => FSharpChoice$2<U1, U2>, sourceEvent: IEvent<T>): [IEvent<U1>, IEvent<U2>] {
  return [
    choose((v) => Choice_tryValueIfChoice1Of2(splitter(v)), sourceEvent),
    choose((v) => Choice_tryValueIfChoice2Of2(splitter(v)), sourceEvent),
  ];
}

export function createEvent<T>(addHandler: (h: DotNetDelegate<T>) => void, removeHandler: (h: DotNetDelegate<T>) => void): IEvent<T> {
  return {
    AddHandler(h) { addHandler(h); },
    RemoveHandler(h) { removeHandler(h); },
    Subscribe(r) {
      const h: DotNetDelegate<T> = (_, args) => r.OnNext(args);
      addHandler(h);
      return {
        Dispose() { removeHandler(h); }
      };
    }
  };
}

export default Event;
