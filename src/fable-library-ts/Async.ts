import { OperationCanceledException, Trampoline } from "./AsyncBuilder.ts";
import { Continuation, Continuations } from "./AsyncBuilder.ts";
import { Async, IAsyncContext, CancellationToken } from "./AsyncBuilder.ts";
import { protectedCont, protectedBind, protectedReturn } from "./AsyncBuilder.ts";
import { FSharpChoice$2_$union, Choice_makeChoice1Of2, Choice_makeChoice2Of2 } from "./Choice.ts";
import { TimeoutException_$ctor } from "./System.ts";
import { Exception } from "./Util.ts";

function emptyContinuation<T>(_x: T) {
  // NOP
}

// see AsyncBuilder.Delay
function delay<T>(generator: () => Async<T>) {
  return protectedCont((ctx: IAsyncContext<T>) => generator()(ctx));
}

// MakeAsync: body:(AsyncActivation<'T> -> AsyncReturn) -> Async<'T>
export function makeAsync<T>(body: Async<T>) {
  return body;
}
// Invoke: computation: Async<'T> -> ctxt:AsyncActivation<'T> -> AsyncReturn
export function invoke<T>(computation: Async<T>, ctx: IAsyncContext<T>) {
  return computation(ctx);
}
// CallThenInvoke: ctxt:AsyncActivation<'T> -> result1:'U -> part2:('U -> Async<'T>) -> AsyncReturn
export function callThenInvoke<T, U>(ctx: IAsyncContext<T>, result1: U, part2: (x: U) => Async<T>) {
  return part2(result1)(ctx);
}
// Bind: ctxt:AsyncActivation<'T> -> part1:Async<'U> -> part2:('U -> Async<'T>) -> AsyncReturn
export function bind<T, U>(ctx: IAsyncContext<T>, part1: Async<U>, part2: (x: U) => Async<T>) {
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
    throw new Exception("Operation is cancelled");
  }
}

export function startChild<T>(computation: Async<T>, ms?: number): Async<Async<T>> {
  const promise = startAsPromise(computation);
  let promiseToRun = promise;

  if (ms) {
    // Race the computation against a timeout: whichever settles first wins.
    promiseToRun = new Promise<T>((resolve, reject) => {
      const timeoutId = setTimeout(() => reject(TimeoutException_$ctor()), ms);
      promise.then(
        value => { clearTimeout(timeoutId); resolve(value); },
        error => { clearTimeout(timeoutId); reject(error); }
      );
    });
  }

  // JS Promises are hot, computation has already started
  // but we delay returning the result
  return protectedCont((ctx) =>
    protectedReturn(awaitPromise(promiseToRun))(ctx));
}

export function awaitPromise<T>(p: Promise<T>) {
  return fromContinuations((conts: Continuations<T>) =>
    p.then(conts[0]).catch((err) =>
      (err instanceof OperationCanceledException
        ? conts[2] : conts[1])(err)));
}

export function cancellationToken() {
  return protectedCont((ctx: IAsyncContext<CancellationToken>) => ctx.onSuccess(ctx.cancelToken));
}

export const defaultCancellationToken = new CancellationToken();

export function catchAsync<T>(work: Async<T>) {
  return protectedCont((ctx: IAsyncContext<FSharpChoice$2_$union<T, Exception>>) => {
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

export function ignore<T>(computation: Async<T>) {
  return protectedBind(computation, (_x) => protectedReturn(void 0));
}

export function parallel<T>(computations: Iterable<Async<T>>) {
  return delay(() => awaitPromise(Promise.all(Array.from(computations, (w) => startAsPromise(w)))));
}

export function sequential<T>(computations: Iterable<Async<T>>) {

  function _sequential<T>(computations: Iterable<Async<T>>): Promise<T[]> {
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
      ctx.onCancel(new OperationCanceledException());
    });
  });
}

export function start<T>(computation: Async<T>, cancellationToken?: CancellationToken) {
  return startWithContinuations(
    computation,
    emptyContinuation,
    function (err) { throw err },
    emptyContinuation,
    cancellationToken
  );
}

export function startImmediate<T>(computation: Async<T>, cancellationToken?: CancellationToken) {
  return start(computation, cancellationToken);
}

export function startWithContinuations<T>(
  computation: Async<T>,
  continuation: Continuation<T>,
  exceptionContinuation: Continuation<any>,
  cancellationContinuation: Continuation<any>,
  cancelToken?: CancellationToken) {

  const trampoline = new Trampoline();
  computation({
    onSuccess: continuation ? continuation as Continuation<T> : emptyContinuation,
    onError: exceptionContinuation,
    onCancel: cancellationContinuation,
    cancelToken: cancelToken ? cancelToken : defaultCancellationToken,
    trampoline,
  });
}

export function startAsPromise<T>(computation: Async<T>, cancellationToken?: CancellationToken) {
  return new Promise((resolve: Continuation<T>, reject: Continuation<any>) =>
    startWithContinuations(computation, resolve, reject, reject,
      cancellationToken ? cancellationToken : defaultCancellationToken));
}

interface IDelegateEventLike<Del extends Function> {
  AddHandler(h: Del): void;
  RemoveHandler(h: Del): void;
}

export function awaitEvent<Del extends Function, T>(event: IDelegateEventLike<Del>, cancelAction?: () => void): Async<T> {
  return protectedCont((ctx: IAsyncContext<T>) => {
    let tokenId: number;
    const handler = ((_sender: unknown, args: T) => {
      ctx.cancelToken.removeListener(tokenId);
      event.RemoveHandler(handler as unknown as Del);
      ctx.onSuccess(args);
    }) as unknown as Del;
    tokenId = ctx.cancelToken.addListener(() => {
      event.RemoveHandler(handler as unknown as Del);
      if (cancelAction != null) {
        cancelAction();
      }
      ctx.onCancel(new OperationCanceledException());
    });
    event.AddHandler(handler);
  });
}
