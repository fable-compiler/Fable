from threading import Timer
from .async_builder import (
    CancellationToken,
    IAsyncContext,
    OperationCanceledError,
    Trampoline,
    protected_bind,
    protected_cont,
    protected_return,
)
from .choice import Choice_makeChoice1Of2, Choice_makeChoice2Of2  # type: ignore


class Async:
    pass


def empty_continuation(x=None):
    pass


default_cancellation_token = CancellationToken()


def createCancellationToken(arg):
    print("createCancellationToken()", arg)
    cancelled, number = (arg, False) if isinstance(arg, bool) else (False, True)
    token = CancellationToken(cancelled)
    if number:
        timer = Timer(arg / 1000.0, token.cancel)  # type: ignore
        timer.start()

    return token


def cancel(token: CancellationToken):
    print("cancel()")
    token.cancel()


def cancelAfter(token: CancellationToken, ms: int):
    print("cancelAfter()", ms / 1000.0)
    timer = Timer(ms / 1000.0, token.cancel)
    timer.start()


def sleep(millisecondsDueTime: int):
    def cont(ctx: IAsyncContext):
        def cancel():
            timer.cancel()
            ctx.on_cancel(OperationCanceledError())

        token_id = ctx.cancel_token.add_listener(cancel)

        def timeout():
            ctx.cancel_token.remove_listener(token_id)
            ctx.on_success()

        timer = Timer(millisecondsDueTime / 1000.0, timeout)

    return protected_cont(cont)


def ignore(computation):
    return protected_bind(computation, lambda _x: protected_return())


def catchAsync(work):
    def cont(ctx: IAsyncContext):
        def on_success(x):
            ctx.on_success(Choice_makeChoice1Of2(x))

        def on_error(err):
            ctx.on_success(Choice_makeChoice2Of2(err))

        ctx_ = IAsyncContext.create(on_success, on_error, ctx.on_cancel, ctx.trampoline, ctx.cancel_token)
        work(ctx_)

    return protected_cont(cont)


def fromContinuations(f):
    def cont(ctx: IAsyncContext):
        f([ctx.on_success, ctx.on_error, ctx.on_cancel])

    return protected_cont(cont)


def startWithContinuations(
    computation, continuation=None, exception_continuation=None, cancellation_continuation=None, cancellation_token=None
):
    trampoline = Trampoline()

    ctx = IAsyncContext.create(
        continuation or empty_continuation,
        exception_continuation or empty_continuation,
        cancellation_continuation or empty_continuation,
        trampoline,
        cancellation_token or default_cancellation_token,
    )

    return computation(ctx)


def start(computation, cancellation_token=None):
    return startWithContinuations(computation, cancellation_token=cancellation_token)


def startImmediate(computation):
    return start(computation)
