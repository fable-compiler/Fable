import { IDisposable } from "./Util";

export type Continuation<T> = (x: T) => void;

export type Continuations<T> = [
  Continuation<T>,
  Continuation<Error>,
  Continuation<OperationCanceledError>
];

export class CancellationToken {
    private _id: number;
    private _cancelled: boolean;
    private _listeners: Map<number, () => void>;
    constructor(cancelled = false) {
        this._id = 0;
        this._cancelled = cancelled;
        this._listeners = new Map();
    }
    get isCancelled() {
        return this._cancelled;
    }
    cancel() {
        if (!this._cancelled) {
            this._cancelled = true;
            for (const [, listener] of this._listeners) {
                listener();
            }
        }
    }
    addListener(f: () => void) {
        const id = this._id;
        this._listeners.set(this._id++, f);
        return id;
    }
    removeListener(id: number) {
        return this._listeners.delete(id);
    }
    register(f: (state?: any)=>void, state?: any) {
        const $ = this;
        const id = this.addListener(state == null ? f : () => f(state));
        return { Dispose() { $.removeListener(id); } }
    }
}

export class OperationCanceledError extends Error {
  constructor() {
    super("The operation was canceled");
    Object.setPrototypeOf(this, OperationCanceledError.prototype);
  }
}

export class Trampoline {
  static get maxTrampolineCallCount() {
    return 2000;
  }
  private callCount: number;
  constructor() {
    this.callCount = 0;
  }
  public incrementAndCheck() {
    return this.callCount++ > Trampoline.maxTrampolineCallCount;
  }
  public hijack(f: () => void) {
    this.callCount = 0;
    setTimeout(f, 0);
  }
}

export interface IAsyncContext<T> {
  onSuccess: Continuation<T>;
  onError: Continuation<Error>;
  onCancel: Continuation<OperationCanceledError>;

  cancelToken: CancellationToken;
  trampoline: Trampoline;
}

export type IAsync<T> = (x: IAsyncContext<T>) => void;

export function protectedCont<T>(f: IAsync<T>) {
  return (ctx: IAsyncContext<T>) => {
    if (ctx.cancelToken.isCancelled) {
      ctx.onCancel(new OperationCanceledError());
    } else if (ctx.trampoline.incrementAndCheck()) {
      ctx.trampoline.hijack(() => {
        try {
          f(ctx);
        } catch (err) {
          ctx.onError(err);
        }
      });
    } else {
      try {
        f(ctx);
      } catch (err) {
        ctx.onError(err);
      }
    }
  };
}

export function protectedBind<T, U>(computation: IAsync<T>, binder: (x: T) => IAsync<U>) {
  return protectedCont((ctx: IAsyncContext<U>) => {
    computation({
      onSuccess: (x: T) => {
        try {
          binder(x)(ctx);
        } catch (ex) {
          ctx.onError(ex);
        }
      },
      onError: ctx.onError,
      onCancel: ctx.onCancel,
      cancelToken: ctx.cancelToken,
      trampoline: ctx.trampoline,
    });
  });
}

export function protectedReturn<T>(value?: T) {
  return protectedCont((ctx: IAsyncContext<T>) => ctx.onSuccess(value));
}

export class AsyncBuilder {
  public Bind<T, U>(computation: IAsync<T>, binder: (x: T) => IAsync<U>) {
    return protectedBind(computation, binder);
  }

  public Combine<T>(computation1: IAsync<void>, computation2: IAsync<T>) {
    return this.Bind(computation1, () => computation2);
  }

  public Delay<T>(generator: () => IAsync<T>) {
    return protectedCont((ctx: IAsyncContext<T>) => generator()(ctx));
  }

  public For<T>(sequence: Iterable<T>, body: (x: T) => IAsync<void>) {
    const iter = sequence[Symbol.iterator]();
    let cur = iter.next();
    return this.While(() => !cur.done, this.Delay(() => {
      const res = body(cur.value);
      cur = iter.next();
      return res;
    }));
  }

  public Return<T>(value?: T) {
    return protectedReturn(value);
  }

  public ReturnFrom<T>(computation: IAsync<T>) {
    return computation;
  }

  public TryFinally<T>(computation: IAsync<T>, compensation: () => void) {
    return protectedCont((ctx: IAsyncContext<T>) => {
      computation({
        onSuccess: (x: T) => {
          compensation();
          ctx.onSuccess(x);
        },
        onError: (x: any) => {
          compensation();
          ctx.onError(x);
        },
        onCancel: (x: any) => {
          compensation();
          ctx.onCancel(x);
        },
        cancelToken: ctx.cancelToken,
        trampoline: ctx.trampoline,
      });
    });
  }

  public TryWith<T>(computation: IAsync<T>, catchHandler: (e: any) => IAsync<T>) {
    return protectedCont((ctx: IAsyncContext<T>) => {
      computation({
        onSuccess: ctx.onSuccess,
        onCancel: ctx.onCancel,
        cancelToken: ctx.cancelToken,
        trampoline: ctx.trampoline,
        onError: (ex: any) => {
          try {
            catchHandler(ex)(ctx);
          } catch (ex2) {
            ctx.onError(ex2);
          }
        },
      });
    });
  }

  public Using<T extends IDisposable, U>(resource: T, binder: (x: T) => IAsync<U>) {
    return this.TryFinally(binder(resource), () => resource.Dispose());
  }

  public While(guard: () => boolean, computation: IAsync<void>): IAsync<void> {
    if (guard()) {
      return this.Bind(computation, () => this.While(guard, computation));
    } else {
      return this.Return(void 0);
    }
  }

  public Zero() {
    return protectedCont((ctx: IAsyncContext<void>) => ctx.onSuccess(void 0));
  }
}

export const singleton = new AsyncBuilder();
