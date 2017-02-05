import { IObserver } from "./Observable"
import { IObservable } from "./Observable"
import { IDisposable } from "./Util"
import { createDisposable } from "./Util"
import { iterate as seqIterate } from "./Seq"
import Choice from "./Choice"
import { Observer } from "./Observable"
import { protect } from "./Observable"

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
  private _subscriber: (o: IObserver<T>) => IDisposable;
  private _dotnetDelegates: Map<DotNetDelegate<T>, Delegate<T>>;
  delegates: Array<Delegate<T>>;

  constructor(_subscriber?: (o: IObserver<T>) => IDisposable, delegates?: any[]) {
    this._subscriber = _subscriber;
    this.delegates = delegates || new Array<Delegate<T>>();
  }

  Add(f: Delegate<T>) {
    this._addHandler(f);
  }

  // IEvent<T> methods

  get Publish() {
    return this;
  }

  Trigger(value: T) {
    seqIterate(f => f(value), this.delegates);
  }

  // IDelegateEvent<T> methods

  private _addHandler(f: Delegate<T>) {
    this.delegates.push(f);
  }

  private _removeHandler(f: Delegate<T>) {
    const index = this.delegates.indexOf(f);
    if (index > -1)
      this.delegates.splice(index, 1);
  }

  AddHandler(handler: DotNetDelegate<T>) {
    if (this._dotnetDelegates == null) {
      this._dotnetDelegates = new Map<DotNetDelegate<T>,Delegate<T>>();
    }
    const f = function (x: T) { handler(null, x) };
    this._dotnetDelegates.set(handler, f);
    this._addHandler(f)
  }

  RemoveHandler(handler: DotNetDelegate<T>) {
    if (this._dotnetDelegates != null) {
      const f = this._dotnetDelegates.get(handler);
      if (f != null) {
        this._dotnetDelegates.delete(handler);
        this._removeHandler(f);
      }
    }
  }

  // IObservable<T> methods

  private _subscribeFromObserver(observer: IObserver<T>) {
    if (this._subscriber)
      return this._subscriber(observer);

    const callback = observer.OnNext;
    this._addHandler(callback);
    return createDisposable(() => this._removeHandler(callback));
  }

  private _subscribeFromCallback(callback: Delegate<T>) {
    this._addHandler(callback);
    return createDisposable(() => this._removeHandler(callback));
  }

  Subscribe(arg: IObserver<T> | Delegate<T>) {
    return typeof arg == "function"
      ? this._subscribeFromCallback(<Delegate<T>>arg)
      : this._subscribeFromObserver(<IObserver<T>>arg);
  }
}

export function add<T>(callback: (x: T) => void, sourceEvent: IEvent<T>) {
  (<Event<T>>sourceEvent).Subscribe(new Observer(callback));
}

export function choose<T, U>(chooser: (x: T) => U, sourceEvent: IEvent<T>) {
  const source = <Event<T>>sourceEvent;
  return <IEvent<U>>new Event<U>(observer =>
    source.Subscribe(new Observer<T>(t =>
      protect(
        () => chooser(t),
        u => { if (u != null) observer.OnNext(u); },
        observer.OnError),
      observer.OnError, observer.OnCompleted)),
    source.delegates);
}

export function filter<T>(predicate: (x: T) => boolean, sourceEvent: IEvent<T>) {
  return choose(x => predicate(x) ? x : null, sourceEvent);
}

export function map<T, U>(mapping: (x: T) => U, sourceEvent: IEvent<T>) {
  const source = <Event<T>>sourceEvent;
  return <IEvent<U>>new Event<U>(observer =>
    source.Subscribe(new Observer<T>(t =>
      protect(
        () => mapping(t),
        observer.OnNext,
        observer.OnError),
      observer.OnError, observer.OnCompleted)),
    source.delegates);
}

export function merge<T>(event1: IEvent<T>, event2: IEvent<T>) {
  const source1 = <Event<T>>event1;
  const source2 = <Event<T>>event2;
  return <IEvent<T>>new Event<T>(observer => {
    let stopped = false, completed1 = false, completed2 = false;

    const h1 = source1.Subscribe(new Observer<T>(
      v => { if (!stopped) observer.OnNext(v); },
      e => {
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
      v => { if (!stopped) observer.OnNext(v); },
      e => {
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

    return createDisposable(() => {
      h1.Dispose();
      h2.Dispose();
    });
  }, source1.delegates.concat(source2.delegates));
}

export function pairwise<T>(sourceEvent: IEvent<T>) {
  const source = <Event<T>>sourceEvent;
  return <IEvent<[T, T]>>new Event<[T, T]>(observer => {
    let last: T = null;
    return source.Subscribe(new Observer<T>(next => {
      if (last != null)
        observer.OnNext([last, next]);
      last = next;
    }, observer.OnError, observer.OnCompleted));
  }, source.delegates);
}

export function partition<T>(predicate: (x: T) => boolean, sourceEvent: IEvent<T>) {
  return [filter(predicate, sourceEvent), filter(x => !predicate(x), sourceEvent)];
}

export function scan<U, T>(collector: (u: U, t: T) => U, state: U, sourceEvent: IEvent<T>) {
  const source = <Event<T>>sourceEvent;
  return <IEvent<U>>new Event<U>(observer => {
    return source.Subscribe(new Observer<T>(t => {
      protect(
        () => collector(state, t),
        u => { state = u; observer.OnNext(u); },
        observer.OnError);
    }, observer.OnError, observer.OnCompleted));
  }, source.delegates);
}

export function split<T, U1, U2>(splitter: (x: T) => Choice<U1, U2>, sourceEvent: IEvent<T>) {
  return [choose(v => splitter(v).valueIfChoice1, sourceEvent), choose(v => splitter(v).valueIfChoice2, sourceEvent)];
}