import { IObservable, IObserver, Observer, protect } from "./Observable";
import { Choice, Option, some, tryValueIfChoice1, tryValueIfChoice2, value } from "./Option";
import { iterate as seqIterate } from "./Seq";
import { IDisposable } from "./Util";

export type Delegate<T> = (x: T) => void;
export type DotNetDelegate<T> = (sender: any, x: T) => void;

export interface IDelegateEvent<T> {
  AddHandler(d: DotNetDelegate<T>): void;
  RemoveHandler(d: DotNetDelegate<T>): void;
}

export interface IEvent<T> extends IObservable<T>, IDelegateEvent<T> {
  Publish: IEvent<T>;
  Trigger(x: T): void;
}

export default class Event<T> implements IEvent<T> {
  public delegates: Delegate<T>[];
  private _subscriber?: (o: IObserver<T>) => IDisposable;
  private _dotnetDelegates?: Map<DotNetDelegate<T>, Delegate<T>>;

  constructor(_subscriber?: (o: IObserver<T>) => IDisposable, delegates?: any[]) {
    this._subscriber = _subscriber;
    this.delegates = delegates || new Array<Delegate<T>>();
  }

  public Add(f: Delegate<T>) {
    this._addHandler(f);
  }

  // IEvent<T> methods

  get Publish() {
    return this;
  }

  public Trigger(value: T) {
    seqIterate((f) => f(value), this.delegates);
  }

  // IDelegateEvent<T> methods

  public AddHandler(handler: DotNetDelegate<T>) {
    if (this._dotnetDelegates == null) {
      this._dotnetDelegates = new Map<DotNetDelegate<T>, Delegate<T>>();
    }
    const f = (x: T) => handler(null, x);
    this._dotnetDelegates.set(handler, f);
    this._addHandler(f);
  }

  public RemoveHandler(handler: DotNetDelegate<T>) {
    if (this._dotnetDelegates != null) {
      const f = this._dotnetDelegates.get(handler);
      if (f != null) {
        this._dotnetDelegates.delete(handler);
        this._removeHandler(f);
      }
    }
  }

  // IObservable<T> methods

  public Subscribe(arg: IObserver<T> | Delegate<T>) {
    return typeof arg === "function"
      ? this._subscribeFromCallback(arg as Delegate<T>)
      : this._subscribeFromObserver(arg as IObserver<T>);
  }

  private _addHandler(f: Delegate<T>) {
    this.delegates.push(f);
  }

  private _removeHandler(f: Delegate<T>) {
    const index = this.delegates.indexOf(f);
    if (index > -1) {
      this.delegates.splice(index, 1);
    }
  }

  private _subscribeFromObserver(observer: IObserver<T>): IDisposable {
    if (this._subscriber) {
      return this._subscriber(observer);
    }

    const callback = observer.OnNext;
    this._addHandler(callback);
    return { Dispose: () => { this._removeHandler(callback); } };
  }

  private _subscribeFromCallback(callback: Delegate<T>): IDisposable {
    this._addHandler(callback);
    return { Dispose: () => { this._removeHandler(callback); } };
  }
}

export function add<T>(callback: (x: T) => void, sourceEvent: IEvent<T>) {
  (sourceEvent as Event<T>).Subscribe(new Observer(callback));
}

export function choose<T, U>(chooser: (x: T) => Option<U>, sourceEvent: IEvent<T>) {
  const source = sourceEvent as Event<T>;
  return new Event<U>((observer) =>
    source.Subscribe(new Observer<T>((t) =>
      protect(
        () => chooser(t),
        (u) => { if (u != null) { observer.OnNext(value(u)); } },
        observer.OnError),
      observer.OnError, observer.OnCompleted)),
    source.delegates);
}

export function filter<T>(predicate: (x: T) => boolean, sourceEvent: IEvent<T>) {
  return choose((x) => predicate(x) ? some(x) : null, sourceEvent);
}

export function map<T, U>(mapping: (x: T) => U, sourceEvent: IEvent<T>) {
  const source = sourceEvent as Event<T>;
  return new Event<U>((observer) =>
    source.Subscribe(new Observer<T>((t) =>
      protect(
        () => mapping(t),
        observer.OnNext,
        observer.OnError),
      observer.OnError, observer.OnCompleted)),
    source.delegates);
}

export function merge<T>(event1: IEvent<T>, event2: IEvent<T>) {
  const source1 = event1 as Event<T>;
  const source2 = event2 as Event<T>;
  return new Event<T>((observer) => {
    let stopped = false;
    let completed1 = false;
    let completed2 = false;

    const h1 = source1.Subscribe(new Observer<T>(
      (v) => { if (!stopped) { observer.OnNext(v); } },
      (e) => {
        if (!stopped) {
          stopped = true;
          observer.OnError(e);
        }
      },
      () => {
        if (!stopped) {
          completed1 = true;
          if (completed2) {
            stopped = true;
            observer.OnCompleted();
          }
        }
      }));

    const h2 = source2.Subscribe(new Observer<T>(
      (v) => { if (!stopped) { observer.OnNext(v); } },
      (e) => {
        if (!stopped) {
          stopped = true;
          observer.OnError(e);
        }
      },
      () => {
        if (!stopped) {
          completed2 = true;
          if (completed1) {
            stopped = true;
            observer.OnCompleted();
          }
        }
      }));

    return {
      Dispose() {
        h1.Dispose();
        h2.Dispose();
      },
    } as IDisposable;
  }, source1.delegates.concat(source2.delegates));
}

export function pairwise<T>(sourceEvent: IEvent<T>) {
  const source = sourceEvent as Event<T>;
  return new Event<[T, T]>((observer) => {
    let last: T;
    return source.Subscribe(new Observer<T>((next) => {
      if (last != null) {
        observer.OnNext([last, next]);
      }
      last = next;
    }, observer.OnError, observer.OnCompleted));
  }, source.delegates);
}

export function partition<T>(predicate: (x: T) => boolean, sourceEvent: IEvent<T>): [IEvent<T>, IEvent<T>] {
  return [filter(predicate, sourceEvent), filter((x) => !predicate(x), sourceEvent)];
}

export function scan<U, T>(collector: (u: U, t: T) => U, state: U, sourceEvent: IEvent<T>) {
  const source = sourceEvent as Event<T>;
  return new Event<U>((observer) => {
    return source.Subscribe(new Observer<T>((t) => {
      protect(
        () => collector(state, t),
        (u) => { state = u; observer.OnNext(u); },
        observer.OnError);
    }, observer.OnError, observer.OnCompleted));
  }, source.delegates);
}

export function split<T, U1, U2>(splitter: (x: T) => Choice<U1, U2>, sourceEvent: IEvent<T>): [IEvent<U1>, IEvent<U2>] {
  return [
    choose((v) => tryValueIfChoice1(splitter(v)), sourceEvent),
    choose((v) => tryValueIfChoice2(splitter(v)), sourceEvent),
  ];
}
