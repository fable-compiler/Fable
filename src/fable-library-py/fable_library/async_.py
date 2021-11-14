from threading import Timer
from .async_builder import (
    CancellationToken,
    IAsyncContext,
    OperationCanceledError,
    Trampoline,
    empty_continuation,
    protected_bind,
    protected_cont,
    protected_return,
)
from .choice import Choice_makeChoice1Of2, Choice_makeChoice2Of2  # type: ignore


class Async:
    pass


default_cancellation_token = CancellationToken()


def create_cancellation_token(arg=None):
    cancelled = arg if isinstance(arg, bool) else False
    token = CancellationToken(cancelled)
    if isinstance(arg, int):
        timer = Timer(arg / 1000.0, token.cancel)  # type: ignore
        timer.start()

    return token


def cancel(token: CancellationToken):
    token.cancel()


def cancel_after(token: CancellationToken, ms: int):
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
        timer.start()

    return protected_cont(cont)


def ignore(computation):
    return protected_bind(computation, lambda _x: protected_return())


def catch_async(work):
    def cont(ctx: IAsyncContext):
        def on_success(x):
            ctx.on_success(Choice_makeChoice1Of2(x))

        def on_error(err):
            ctx.on_success(Choice_makeChoice2Of2(err))

        ctx_ = IAsyncContext.create(on_success, on_error, ctx.on_cancel, ctx.trampoline, ctx.cancel_token)
        work(ctx_)

    return protected_cont(cont)


def from_continuations(f):
    def cont(ctx: IAsyncContext):
        f([ctx.on_success, ctx.on_error, ctx.on_cancel])

    return protected_cont(cont)


def start_with_continuations(
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
    return start_with_continuations(computation, cancellation_token=cancellation_token)


def start_immediate(computation, cancellation_token=None):
    return start(computation, cancellation_token)
