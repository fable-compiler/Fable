import { IDisposable } from "./Util.ts";
import { zero } from "./Task.ts";

export class TaskBuilder {
  public Bind<T, U>(computation: Promise<T>, binder: (x: T) => Promise<U>): Promise<U> {
    return computation.then(binder);
  }

  public Combine<T>(computation1: Promise<void>, computation2: () => Promise<T>): Promise<T> {
    return computation1.then(computation2);
  }

  public Delay<T>(generator: () => Promise<T>): () => Promise<T> {
    return generator;
  }

  public For<T>(sequence: Iterable<T>, body: (x: T) => Promise<void>): Promise<void> {
    const iter = sequence[Symbol.iterator]();
    let cur = iter.next();
    return this.While(
      () => !cur.done,
      this.Delay(() => {
        const res = body(cur.value!);
        cur = iter.next();
        return res;
      })
    );
  }

  public Return<T>(value?: T): Promise<T | undefined> {
    return Promise.resolve(value);
  }

  public ReturnFrom<T>(computation: Promise<T>): Promise<T> {
    return computation;
  }

  public TryFinally<T>(computation: () => Promise<T>, compensation: () => void): Promise<T> {
    try {
      return computation().finally(compensation);
    } catch (e) {
      compensation();
      return Promise.reject(e);
    }
  }

  public TryWith<T>(computation: () => Promise<T>, catchHandler: (e: unknown) => Promise<T>): Promise<T> {
    try {
      return computation().catch(catchHandler);
    } catch (e) {
      try {
        return catchHandler(e);
      } catch (e2) {
        return Promise.reject(e2);
      }
    }
  }

  public Using<T extends IDisposable, U>(resource: T, binder: (x: T) => Promise<U>): Promise<U> {
    return this.TryFinally(() => binder(resource), () => resource.Dispose());
  }

  public While(guard: () => boolean, computation: () => Promise<void>): Promise<void> {
    return (async () => {
      while (guard()) {
        await computation();
      }
    })();
  }

  public Zero(): Promise<void> {
    return zero();
  }

  public Run<T>(computation: () => Promise<T>): Promise<T> {
    try {
      return computation();
    } catch (e) {
      return Promise.reject(e);
    }
  }
}

export const singleton = new TaskBuilder();
export function task(): TaskBuilder { return singleton; }
