from .async_builder import CancellationToken, IAsyncContext, Trampoline


class Async:
    pass


def empty_continuation(x=None):
    pass


default_cancellation_token = CancellationToken()


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
