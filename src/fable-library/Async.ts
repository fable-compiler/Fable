import { OperationCanceledError, Trampoline } from "./AsyncBuilder";
import { Continuation, Continuations } from "./AsyncBuilder";
import { CancellationToken } from "./AsyncBuilder";
import { IAsync } from "./AsyncBuilder";
import { IAsyncContext } from "./AsyncBuilder";
import { protectedCont } from "./AsyncBuilder";
import { protectedBind } from "./AsyncBuilder";
import { protectedReturn } from "./AsyncBuilder";
import { choice1Of2, choice2Of2 } from "./Option";
import { map } from "./Seq";

// Implemented just for type references
export class Async<_T> { }

function emptyContinuation<T>(_x: T) {
  // NOP
}

// MakeAsync: body:(AsyncActivation<'T> -> AsyncReturn) -> Async<'T>
export function makeAsync<T>(body: IAsync<T>) {
  return body;
}
// Invoke: computation: Async<'T> -> ctxt:AsyncActivation<'T> -> AsyncReturn
export function invoke<T>(computation: IAsync<T>, ctx: IAsyncContext<T>) {
  return computation(ctx);
}
// CallThenInvoke: ctxt:AsyncActivation<'T> -> result1:'U -> part2:('U -> Async<'T>) -> AsyncReturn
export function callThenInvoke<T, U>(ctx: IAsyncContext<T>, result1: U, part2: (x: U) => IAsync<T>) {
  return part2(result1)(ctx);
}
// Bind: ctxt:AsyncActivation<'T> -> part1:Async<'U> -> part2:('U -> Async<'T>) -> AsyncReturn
export function bind<T, U>(ctx: IAsyncContext<T>, part1: IAsync<U>, part2: (x: U) => IAsync<T>) {
  return protectedBind(part1, part2)(ctx);
}

export function createCancellationToken(arg?: boolean | number): CancellationToken {
  const token = new CancellationToken(typeof arg === "boolean" ? arg : false);
  if (typeof arg === "number") {
    setTimeout(() => { token.cancel(); }, arg);
  }
  return token;
}

export function cancel(token: CancellationToken) {
  token.cancel();
}

export function cancelAfter(token: CancellationToken, ms: number) {
  setTimeout(() => { token.cancel(); }, ms);
}

export function isCancellationRequested(token: CancellationToken) {
  return token != null && token.isCancelled;
}

export function startChild<T>(computation: IAsync<T>): IAsync<IAsync<T>> {
  const promise = startAsPromise(computation);
  // JS Promises are hot, computation has already started
  // but we delay returning the result
  return protectedCont((ctx) =>
    protectedReturn(awaitPromise(promise))(ctx));
}

export function awaitPromise<T>(p: Promise<T>) {
  return fromContinuations((conts: Continuations<T>) =>
    p.then(conts[0]).catch((err) =>
      (err instanceof OperationCanceledError
        ? conts[2] : conts[1])(err)));
}

export function cancellationToken() {
  return protectedCont((ctx: IAsyncContext<CancellationToken>) => ctx.onSuccess(ctx.cancelToken));
}

export const defaultCancellationToken = new CancellationToken();

export function catchAsync<T>(work: IAsync<T>) {
  return protectedCont((ctx: IAsyncContext<any>) => { // ctx: IAsyncContext<Choice<T, Error>>
    work({
      onSuccess: (x) => ctx.onSuccess(choice1Of2(x)),
      onError: (ex) => ctx.onSuccess(choice2Of2(ex)),
      onCancel: ctx.onCancel,
      cancelToken: ctx.cancelToken,
      trampoline: ctx.trampoline,
    });
  });
}

export function fromContinuations<T>(f: (conts: Continuations<T>) => void) {
  return protectedCont((ctx: IAsyncContext<T>) =>
    f([ctx.onSuccess, ctx.onError, ctx.onCancel]));
}

export function ignore<T>(computation: IAsync<T>) {
  return protectedBind(computation, (_x) => protectedReturn(void 0));
}

export function parallel<T>(computations: Iterable<IAsync<T>>) {
  return awaitPromise(Promise.all(map((w) => startAsPromise(w), computations)));
}

export function sleep(millisecondsDueTime: number) {
  return protectedCont((ctx: IAsyncContext<void>) => {
    let tokenId: number;
    const timeoutId = setTimeout(() => {
      ctx.cancelToken.removeListener(tokenId);
      ctx.onSuccess(void 0);
    }, millisecondsDueTime);
    tokenId = ctx.cancelToken.addListener(() => {
      clearTimeout(timeoutId);
      ctx.onCancel(new OperationCanceledError());
    });
  });
}

export function start<T>(computation: IAsync<T>, cancellationToken?: CancellationToken) {
  return startWithContinuations(computation, cancellationToken);
}

export function startImmediate<T>(computation: IAsync<T>, cancellationToken?: CancellationToken) {
  return start(computation, cancellationToken);
}

export function startWithContinuations<T>(
  computation: IAsync<T>,
  continuation?: Continuation<T> | CancellationToken,
  exceptionContinuation?: Continuation<any>,
  cancellationContinuation?: Continuation<any>,
  cancelToken?: CancellationToken) {
  if (typeof continuation !== "function") {
    cancelToken = continuation as CancellationToken;
    continuation = undefined;
  }
  const trampoline = new Trampoline();
  computation({
    onSuccess: continuation ? continuation as Continuation<T> : emptyContinuation,
    onError: exceptionContinuation ? exceptionContinuation : emptyContinuation,
    onCancel: cancellationContinuation ? cancellationContinuation : emptyContinuation,
    cancelToken: cancelToken ? cancelToken : defaultCancellationToken,
    trampoline,
  });
}

export function startAsPromise<T>(computation: IAsync<T>, cancellationToken?: CancellationToken) {
  return new Promise((resolve: Continuation<T>, reject: Continuation<any>) =>
    startWithContinuations(computation, resolve, reject, reject,
      cancellationToken ? cancellationToken : defaultCancellationToken));
}

export default Async;
