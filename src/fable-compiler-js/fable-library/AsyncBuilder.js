import { ensureErrorOrException } from './Types.js';
export class CancellationToken {
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
    addListener(f) {
        const id = this._id;
        this._listeners.set(this._id++, f);
        return id;
    }
    removeListener(id) {
        return this._listeners.delete(id);
    }
    register(f, state) {
        const $ = this;
        const id = this.addListener(state == null ? f : () => f(state));
        return { Dispose() { $.removeListener(id); } };
    }
    Dispose() {
        // Implement IDisposable for compatibility but do nothing
        // According to docs, calling Dispose does not trigger cancellation
        // https://docs.microsoft.com/en-us/dotnet/api/system.threading.cancellationtokensource.dispose?view=net-6.0
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
    constructor() {
        this.callCount = 0;
    }
    incrementAndCheck() {
        return this.callCount++ > Trampoline.maxTrampolineCallCount;
    }
    hijack(f) {
        this.callCount = 0;
        setTimeout(f, 0);
    }
}
export function protectedCont(f) {
    return (ctx) => {
        if (ctx.cancelToken.isCancelled) {
            ctx.onCancel(new OperationCanceledError());
        }
        else if (ctx.trampoline.incrementAndCheck()) {
            ctx.trampoline.hijack(() => {
                try {
                    f(ctx);
                }
                catch (err) {
                    ctx.onError(ensureErrorOrException(err));
                }
            });
        }
        else {
            try {
                f(ctx);
            }
            catch (err) {
                ctx.onError(ensureErrorOrException(err));
            }
        }
    };
}
export function protectedBind(computation, binder) {
    return protectedCont((ctx) => {
        computation({
            onSuccess: (x) => {
                try {
                    binder(x)(ctx);
                }
                catch (err) {
                    ctx.onError(ensureErrorOrException(err));
                }
            },
            onError: ctx.onError,
            onCancel: ctx.onCancel,
            cancelToken: ctx.cancelToken,
            trampoline: ctx.trampoline,
        });
    });
}
export function protectedReturn(value) {
    return protectedCont((ctx) => ctx.onSuccess(value));
}
export class AsyncBuilder {
    Bind(computation, binder) {
        return protectedBind(computation, binder);
    }
    Combine(computation1, computation2) {
        return this.Bind(computation1, () => computation2);
    }
    Delay(generator) {
        return protectedCont((ctx) => generator()(ctx));
    }
    For(sequence, body) {
        const iter = sequence[Symbol.iterator]();
        let cur = iter.next();
        return this.While(() => !cur.done, this.Delay(() => {
            const res = body(cur.value);
            cur = iter.next();
            return res;
        }));
    }
    Return(value) {
        return protectedReturn(value);
    }
    ReturnFrom(computation) {
        return computation;
    }
    TryFinally(computation, compensation) {
        return protectedCont((ctx) => {
            computation({
                onSuccess: (x) => {
                    compensation();
                    ctx.onSuccess(x);
                },
                onError: (x) => {
                    compensation();
                    ctx.onError(x);
                },
                onCancel: (x) => {
                    compensation();
                    ctx.onCancel(x);
                },
                cancelToken: ctx.cancelToken,
                trampoline: ctx.trampoline,
            });
        });
    }
    TryWith(computation, catchHandler) {
        return protectedCont((ctx) => {
            computation({
                onSuccess: ctx.onSuccess,
                onCancel: ctx.onCancel,
                cancelToken: ctx.cancelToken,
                trampoline: ctx.trampoline,
                onError: (ex) => {
                    try {
                        catchHandler(ex)(ctx);
                    }
                    catch (err) {
                        ctx.onError(ensureErrorOrException(err));
                    }
                },
            });
        });
    }
    Using(resource, binder) {
        return this.TryFinally(binder(resource), () => resource.Dispose());
    }
    While(guard, computation) {
        if (guard()) {
            return this.Bind(computation, () => this.While(guard, computation));
        }
        else {
            return this.Return(void 0);
        }
    }
    Zero() {
        return protectedCont((ctx) => ctx.onSuccess(void 0));
    }
}
export const singleton = new AsyncBuilder();
