import { Trampoline } from "./AsyncBuilder";
import { Continuation } from "./AsyncBuilder";
import { CancellationToken } from "./AsyncBuilder";
import { IAsync } from "./AsyncBuilder";
import { IAsyncContext } from "./AsyncBuilder";
import { protectedCont } from "./AsyncBuilder";
import { protectedBind } from "./AsyncBuilder";
import { protectedReturn } from "./AsyncBuilder";
import Choice from "./Choice";
import { choice1Of2 } from "./Choice";
import { choice2Of2 } from "./Choice";
import { map } from "./Seq";

// Implemented just for type references
export default class Async<T> {
}

function emptyContinuation<T>(x: T) {
  // NOP
}

export function createCancellationToken(arg?: boolean|number): CancellationToken {
  const token = { isCancelled: false };
  if (typeof arg === "number") {
    setTimeout(() => { token.isCancelled = true; }, arg);
  } else if (typeof arg === "boolean") {
    token.isCancelled = arg;
  }
  return token;
}

export function cancel(token: CancellationToken) {
  token.isCancelled = true;
}

export function cancelAfter(token: CancellationToken, ms: number) {
  setTimeout(() => { token.isCancelled = true; }, ms);
}

export function isCancellationRequested(token: CancellationToken) {
  return token != null && token.isCancelled;
}

export function awaitPromise<T>(p: Promise<T>) {
  return fromContinuations((conts: Array<Continuation<T>>) =>
    p.then(conts[0]).catch((err) =>
      (err === "cancelled" ? conts[2] : conts[1])(err)));
}

export function cancellationToken() {
  return protectedCont((ctx: IAsyncContext<CancellationToken>) => ctx.onSuccess(ctx.cancelToken));
}

export const defaultCancellationToken = { isCancelled: false };

export function catchAsync<T>(work: IAsync<T>) {
  return protectedCont((ctx: IAsyncContext<Choice<T, any>>) => {
    work({
      onSuccess: (x) => ctx.onSuccess(choice1Of2<T, any>(x)),
      onError: (ex) => ctx.onSuccess(choice2Of2<T, any>(ex)),
      onCancel: ctx.onCancel,
      cancelToken: ctx.cancelToken,
      trampoline: ctx.trampoline,
    });
  });
}

export function fromContinuations<T>(f: (conts: Array<Continuation<T>>) => void) {
  return protectedCont((ctx: IAsyncContext<T>) => f([ctx.onSuccess, ctx.onError, ctx.onCancel]));
}

export function ignore<T>(computation: IAsync<T>) {
  return protectedBind(computation, (x) => protectedReturn(void 0));
}

export function parallel<T>(computations: Iterable<IAsync<T>>) {
  return awaitPromise(Promise.all(map((w) => startAsPromise(w), computations)));
}

export function sleep(millisecondsDueTime: number) {
  return protectedCont((ctx: IAsyncContext<void>) => {
    setTimeout(() => ctx.cancelToken.isCancelled ?
      ctx.onCancel("cancelled") : ctx.onSuccess(void 0), millisecondsDueTime);
  });
}

export function start<T>(computation: IAsync<void>, cancellationToken?: CancellationToken) {
  return startWithContinuations(computation, cancellationToken);
}

export function startImmediate(computation: IAsync<void>, cancellationToken?: CancellationToken) {
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
    continuation = null;
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
