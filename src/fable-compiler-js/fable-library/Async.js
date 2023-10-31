import { OperationCanceledError, Trampoline } from "./AsyncBuilder.js";
import { CancellationToken } from "./AsyncBuilder.js";
import { protectedCont } from "./AsyncBuilder.js";
import { protectedBind } from "./AsyncBuilder.js";
import { protectedReturn } from "./AsyncBuilder.js";
import { Choice_makeChoice1Of2, Choice_makeChoice2Of2 } from "./Choice.js";
import { TimeoutException } from "./SystemException.js";
// Implemented just for type references
export class Async {
}
function emptyContinuation(_x) {
    // NOP
}
// see AsyncBuilder.Delay
function delay(generator) {
    return protectedCont((ctx) => generator()(ctx));
}
// MakeAsync: body:(AsyncActivation<'T> -> AsyncReturn) -> Async<'T>
export function makeAsync(body) {
    return body;
}
// Invoke: computation: Async<'T> -> ctxt:AsyncActivation<'T> -> AsyncReturn
export function invoke(computation, ctx) {
    return computation(ctx);
}
// CallThenInvoke: ctxt:AsyncActivation<'T> -> result1:'U -> part2:('U -> Async<'T>) -> AsyncReturn
export function callThenInvoke(ctx, result1, part2) {
    return part2(result1)(ctx);
}
// Bind: ctxt:AsyncActivation<'T> -> part1:Async<'U> -> part2:('U -> Async<'T>) -> AsyncReturn
export function bind(ctx, part1, part2) {
    return protectedBind(part1, part2)(ctx);
}
export function createCancellationToken(arg) {
    const token = new CancellationToken(typeof arg === "boolean" ? arg : false);
    if (typeof arg === "number") {
        setTimeout(() => { token.cancel(); }, arg);
    }
    return token;
}
export function cancel(token) {
    token.cancel();
}
export function cancelAfter(token, ms) {
    setTimeout(() => { token.cancel(); }, ms);
}
export function isCancellationRequested(token) {
    return token != null && token.isCancelled;
}
export function throwIfCancellationRequested(token) {
    if (token != null && token.isCancelled) {
        throw new Error("Operation is cancelled");
    }
}
function throwAfter(millisecondsDueTime) {
    return protectedCont((ctx) => {
        let tokenId;
        const timeoutId = setTimeout(() => {
            ctx.cancelToken.removeListener(tokenId);
            ctx.onError(new TimeoutException());
        }, millisecondsDueTime);
        tokenId = ctx.cancelToken.addListener(() => {
            clearTimeout(timeoutId);
            ctx.onCancel(new OperationCanceledError());
        });
    });
}
export function startChild(computation, ms) {
    if (ms) {
        const computationWithTimeout = protectedBind(parallel2(computation, throwAfter(ms)), xs => protectedReturn(xs[0]));
        return startChild(computationWithTimeout);
    }
    const promise = startAsPromise(computation);
    // JS Promises are hot, computation has already started
    // but we delay returning the result
    return protectedCont((ctx) => protectedReturn(awaitPromise(promise))(ctx));
}
export function awaitPromise(p) {
    return fromContinuations((conts) => p.then(conts[0]).catch((err) => (err instanceof OperationCanceledError
        ? conts[2] : conts[1])(err)));
}
export function cancellationToken() {
    return protectedCont((ctx) => ctx.onSuccess(ctx.cancelToken));
}
export const defaultCancellationToken = new CancellationToken();
export function catchAsync(work) {
    return protectedCont((ctx) => {
        work({
            onSuccess: (x) => ctx.onSuccess(Choice_makeChoice1Of2(x)),
            onError: (ex) => ctx.onSuccess(Choice_makeChoice2Of2(ex)),
            onCancel: ctx.onCancel,
            cancelToken: ctx.cancelToken,
            trampoline: ctx.trampoline,
        });
    });
}
export function fromContinuations(f) {
    return protectedCont((ctx) => f([ctx.onSuccess, ctx.onError, ctx.onCancel]));
}
export function ignore(computation) {
    return protectedBind(computation, (_x) => protectedReturn(void 0));
}
export function parallel(computations) {
    return delay(() => awaitPromise(Promise.all(Array.from(computations, (w) => startAsPromise(w)))));
}
function parallel2(a, b) {
    return delay(() => awaitPromise(Promise.all([startAsPromise(a), startAsPromise(b)])));
}
export function sequential(computations) {
    function _sequential(computations) {
        let pr = Promise.resolve([]);
        for (const c of computations) {
            pr = pr.then(results => startAsPromise(c).then(r => results.concat([r])));
        }
        return pr;
    }
    return delay(() => awaitPromise(_sequential(computations)));
}
export function sleep(millisecondsDueTime) {
    return protectedCont((ctx) => {
        let tokenId;
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
export function runSynchronously() {
    throw new Error("Asynchronous code cannot be run synchronously in JS");
}
export function start(computation, cancellationToken) {
    return startWithContinuations(computation, cancellationToken);
}
export function startImmediate(computation, cancellationToken) {
    return start(computation, cancellationToken);
}
export function startWithContinuations(computation, continuation, exceptionContinuation, cancellationContinuation, cancelToken) {
    if (typeof continuation !== "function") {
        cancelToken = continuation;
        continuation = undefined;
    }
    const trampoline = new Trampoline();
    computation({
        onSuccess: continuation ? continuation : emptyContinuation,
        onError: exceptionContinuation ? exceptionContinuation : emptyContinuation,
        onCancel: cancellationContinuation ? cancellationContinuation : emptyContinuation,
        cancelToken: cancelToken ? cancelToken : defaultCancellationToken,
        trampoline,
    });
}
export function startAsPromise(computation, cancellationToken) {
    return new Promise((resolve, reject) => startWithContinuations(computation, resolve, reject, reject, cancellationToken ? cancellationToken : defaultCancellationToken));
}
export default Async;
