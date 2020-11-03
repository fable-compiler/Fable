
import { FSharpChoice$2, Choice_tryValueIfChoice1Of2, Choice_tryValueIfChoice2Of2 } from "./Choice.js";
import { value } from "./Option.js";
import { IDisposable } from "./Util.js";

export interface IObserver<T> {
  OnNext: (x: T) => void;
  OnError: (e: any) => void;
  OnCompleted: () => void;
}

export class Observer<T> implements IObserver<T> {
  public OnNext: (x: T) => void;
  public OnError: (e: any) => void;
  public OnCompleted: () => void;

  constructor(onNext: (x: T) => void, onError?: (e: any) => void, onCompleted?: () => void) {
    this.OnNext = onNext;
    this.OnError = onError || ((_e: any) => { return; });
    this.OnCompleted = onCompleted || (() => { return; });
  }
}

export interface IObservable<T> {
  Subscribe: (o: IObserver<T>) => IDisposable;
}

class Observable<T> implements IObservable<T> {
  public Subscribe: (o: IObserver<T>) => IDisposable;

  constructor(subscribe: (o: IObserver<T>) => IDisposable) {
    this.Subscribe = subscribe;
  }
}

export function protect<T>(f: () => T, succeed: (x: T) => void, fail: (e: any) => void) {
  try {
    return succeed(f());
  } catch (e) {
    fail(e);
  }
}

export function add<T>(callback: (x: T) => void, source: IObservable<T>) {
  source.Subscribe(new Observer(callback));
}

export function choose<T, U>(chooser: (x: T) => U, source: IObservable<T>) {
  return new Observable<U>((observer) =>
    source.Subscribe(new Observer<T>((t) =>
      protect(
        () => chooser(t),
        (u) => { if (u != null) { observer.OnNext(value(u)); } },
        observer.OnError),
      observer.OnError, observer.OnCompleted))) as IObservable<U>;
}

export function filter<T>(predicate: (x: T) => boolean, source: IObservable<T>) {
  return choose((x) => predicate(x) ? x : null, source);
}

export function map<T, U>(mapping: (x: T) => U, source: IObservable<T>) {
  return new Observable<U>((observer) =>
    source.Subscribe(new Observer<T>((t) => {
      protect(
        () => mapping(t),
        observer.OnNext,
        observer.OnError);
    }, observer.OnError, observer.OnCompleted))) as IObservable<U>;
}

export function merge<T>(source1: IObservable<T>, source2: IObservable<T>) {
  return new Observable((observer) => {
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
  }) as IObservable<T>;
}

export function pairwise<T>(source: IObservable<T>) {
  return new Observable<[T, T]>((observer) => {
    let last: T;
    return source.Subscribe(new Observer<T>((next) => {
      if (last != null) {
        observer.OnNext([last, next]);
      }
      last = next;
    }, observer.OnError, observer.OnCompleted));
  }) as IObservable<[T, T]>;
}

export function partition<T>(predicate: (x: T) => boolean, source: IObservable<T>) {
  return [filter(predicate, source), filter((x) => !predicate(x), source)];
}

export function scan<U, T>(collector: (u: U, t: T) => U, state: U, source: IObservable<T>) {
  return new Observable<U>((observer) => {
    return source.Subscribe(new Observer<T>((t) => {
      protect(
        () => collector(state, t),
        (u) => { state = u; observer.OnNext(u); },
        observer.OnError);
    }, observer.OnError, observer.OnCompleted));
  }) as IObservable<U>;
}

export function split<T, U1, U2>(splitter: (x: T) => FSharpChoice$2<U1, U2>, source: IObservable<T>) {
  return [
    choose((v) => Choice_tryValueIfChoice1Of2(splitter(v)), source),
    choose((v) => Choice_tryValueIfChoice2Of2(splitter(v)), source)
  ];
}

export function subscribe<T>(callback: (x: T) => void, source: IObservable<T>) {
  return source.Subscribe(new Observer(callback));
}
