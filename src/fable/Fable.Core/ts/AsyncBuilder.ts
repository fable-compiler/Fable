import { IDisposable } from "./Util"

export type Continuation<T> = (x: T) => void;

export interface CancellationToken {
  isCancelled: boolean;
}

export class Trampoline {
  static get maxTrampolineCallCount() {
    return 2000;
  }
  private callCount: number;
  constructor() {
    this.callCount = 0;
  }
  incrementAndCheck() {
    return this.callCount++ > Trampoline.maxTrampolineCallCount;
  }
  hijack(f:() => void) {
    this.callCount = 0;
    setTimeout(f, 0);
  }
}

export interface IAsyncContext<T> {
  onSuccess: Continuation<T>;
  onError: Continuation<any>;
  onCancel: Continuation<any>;

  cancelToken: CancellationToken;
  trampoline: Trampoline;
}

export type IAsync<T> = (x: IAsyncContext<T>) => void;

export function protectedCont<T>(f: IAsync<T>) {
    return (ctx: IAsyncContext<T>) => {
        if (ctx.cancelToken.isCancelled)
            ctx.onCancel("cancelled");
        else if (ctx.trampoline.incrementAndCheck())
            ctx.trampoline.hijack(() => {
                try {
                    f(ctx);
                } catch (err) {
                    ctx.onError(err);
                }
            });
        else
            try {
                f(ctx);
            } catch (err) {
                ctx.onError(err);
            }
    };
}

export function protectedBind<T, U>(computation: IAsync<T>, binder: (x: T) => IAsync<U>) {
    return protectedCont((ctx: IAsyncContext<U>) => {
        computation({
            onSuccess: (x: T) => {
              try {
                binder(x)(ctx)
              }
              catch (ex) {
                ctx.onError(ex);
              }
            },
            onError: ctx.onError,
            onCancel: ctx.onCancel,
            cancelToken: ctx.cancelToken,
            trampoline: ctx.trampoline
        });
    });
}

export function protectedReturn<T>(value?: T) {
    return protectedCont((ctx: IAsyncContext<T>) => ctx.onSuccess(value));
}

export class AsyncBuilder {
  Bind<T, U>(computation: IAsync<T>, binder: (x: T) => IAsync<U>) {
    return protectedBind(computation, binder);
  }

  Combine<T>(computation1: IAsync<void>, computation2: IAsync<T>) {
    return this.Bind(computation1, () => computation2);
  }

  Delay<T>(generator: () => IAsync<T>) {
    return protectedCont((ctx: IAsyncContext<T>) => generator()(ctx));
  }

  For<T>(sequence: Iterable<T>, body: (x: T) => IAsync<void>) {
    const iter = sequence[Symbol.iterator]();
    let cur = iter.next();
    return this.While(() => !cur.done, this.Delay(() => {
      const res = body(cur.value);
      cur = iter.next();
      return res;
    }));
  }

  Return<T>(value?: T) {
    return protectedReturn(value);
  }

  ReturnFrom<T>(computation: IAsync<T>) {
    return computation;
  }

  TryFinally<T>(computation: IAsync<T>, compensation: () => void) {
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
        trampoline: ctx.trampoline
      });
    });
  }

  TryWith<T>(computation: IAsync<T>, catchHandler: (e: any) => IAsync<T>) {
    return protectedCont((ctx: IAsyncContext<T>) => {
      computation({
        onSuccess: ctx.onSuccess,
        onCancel: ctx.onCancel,
        cancelToken: ctx.cancelToken,
        trampoline: ctx.trampoline,
        onError: (ex: any) => {
          try {
            catchHandler(ex)(ctx)
          }
          catch (ex2) {
            ctx.onError(ex2);
          }
        }
      });
    });
  }

  Using<T extends IDisposable, U>(resource: T, binder: (x: T) => IAsync<U>) {
    return this.TryFinally(binder(resource), () => resource.Dispose());
  }

  While(guard: () => boolean, computation: IAsync<void>): IAsync<void> {
    if (guard())
      return this.Bind(computation, () => this.While(guard, computation));
    else
      return this.Return(void 0);
  }

  Zero() {
    return protectedCont((ctx: IAsyncContext<void>) => ctx.onSuccess(void 0));
  }
}

export const singleton = new AsyncBuilder();