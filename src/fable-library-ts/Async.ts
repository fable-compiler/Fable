import { OperationCanceledError, Trampoline } from "./AsyncBuilder.js";
import { Continuation, Continuations } from "./AsyncBuilder.js";
import { CancellationToken } from "./AsyncBuilder.js";
import { IAsync } from "./AsyncBuilder.js";
import { IAsyncContext } from "./AsyncBuilder.js";
import { protectedCont } from "./AsyncBuilder.js";
import { protectedBind } from "./AsyncBuilder.js";
import { protectedReturn } from "./AsyncBuilder.js";
import { FSharpChoice$2_$union, Choice_makeChoice1Of2, Choice_makeChoice2Of2 } from "./Choice.js";
import { TimeoutException } from "./SystemException.js";

// Implemented just for type references
export class Async<_T> { }

function emptyContinuation<T>(_x: T) {
  // NOP
}

// see AsyncBuilder.Delay
function delay<T>(generator: () => IAsync<T>) {
  return protectedCont((ctx: IAsyncContext<T>) => generator()(ctx));
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

export function throwIfCancellationRequested(token: CancellationToken) {
  if (token != null && token.isCancelled) {
    throw new Error("Operation is cancelled");
  }
}

function throwAfter(millisecondsDueTime: number) : IAsync<void> {
  return protectedCont((ctx: IAsyncContext<void>) => {
    let tokenId: number;
    const timeoutId = setTimeout(() => {
      ctx.cancelToken.removeListener(tokenId);
      ctx.onError(new TimeoutException() as Error);
    }, millisecondsDueTime);
    tokenId = ctx.cancelToken.addListener(() => {
      clearTimeout(timeoutId);
      ctx.onCancel(new OperationCanceledError());
    });
  });
}

export function startChild<T>(computation: IAsync<T>, ms?: number): IAsync<IAsync<T>> {
  if (ms) {
    const computationWithTimeout = protectedBind(
      parallel2(
        computation,
        throwAfter(ms)),
      xs => protectedReturn(xs[0]));

    return startChild(computationWithTimeout);
  }

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
  return protectedCont((ctx: IAsyncContext<FSharpChoice$2_$union<T, Error>>) => {
    work({
      onSuccess: (x) => ctx.onSuccess(Choice_makeChoice1Of2(x)),
      onError: (ex) => ctx.onSuccess(Choice_makeChoice2Of2(ex)),
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
  return delay(() => awaitPromise(Promise.all(Array.from(computations, (w) => startAsPromise(w)))));
}

function parallel2<T, U>(a: IAsync<T>, b: IAsync<U>): IAsync<[T, U]> {
  return delay(() => awaitPromise(Promise.all([ startAsPromise(a), startAsPromise(b) ])));
}

export function sequential<T>(computations: Iterable<IAsync<T>>) {

  function _sequential<T>(computations: Iterable<IAsync<T>>): Promise<T[]> {
    let pr: Promise<T[]> = Promise.resolve([]);
    for (const c of computations) {
      pr = pr.then(results => startAsPromise(c).then(r => results.concat([r])))
    }
    return pr;
  }

  return delay(() => awaitPromise<T[]>(_sequential<T>(computations)));
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

export function runSynchronously(): never {
  throw new Error("Asynchronous code cannot be run synchronously in JS");
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
