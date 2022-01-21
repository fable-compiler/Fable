import asyncio
from asyncio import AbstractEventLoop, Future
from typing import Any, Awaitable, Generator, Optional, TypeVar, cast

_T = TypeVar("_T")


class TaskCompletionSource(Awaitable[_T]):
    def __init__(self) -> None:
        try:
            self.loop: Optional[AbstractEventLoop] = asyncio.get_event_loop()
            self.future: Optional[Future[_T]] = self.loop.create_future()
        except Exception:
            self.loop = None
            self.future = None

        self.has_result = False
        self.result: _T = cast(_T, None)
        self.exception: Optional[Exception] = None
        self.is_cancelled = False

    def SetResult(self, value: _T) -> None:
        """Set result.

        Transitions the underlying Task[TResult] into the
        RanToCompletion state.
        """
        if self.future:
            self.future.set_result(value)
        else:
            self.has_result = True
            self.result = value

    def SetCancelled(self) -> None:
        """Set cancelled.

        Transitions the underlying Task[TResult] into the Canceled
        state.
        """

        if self.future:
            self.future.cancel()
        else:
            self.is_cancelled = True

    def SetException(self, exception: Exception) -> None:
        """Set exception.

        Transitions the underlying Task[TResult] into the Faulted state
        and binds it to a specified exception.
        """
        if self.future:
            self.future.set_exception(exception)
        else:
            self.exception = exception

    def get_task(self) -> Awaitable[_T]:
        return self

    def __await__(self) -> Generator[_T, None, _T]:
        if not self.future:
            loop = asyncio.get_event_loop()
            self.future = loop.create_future()

        if self.has_result:
            print("has_result: ", self.result)
            self.future.set_result(self.result)

        elif self.is_cancelled:
            self.future.cancel()

        elif self.exception:
            self.future.set_exception(self.exception)

        return (yield from self.future.__await__())


async def zero() -> None:
    return


async def from_result(value: _T) -> _T:
    return value


def get_awaiter(value: Awaitable[_T]) -> Awaitable[_T]:
    # Convert awaitable to coroutine
    async def get_value() -> _T:
        return await value

    return get_value()


def get_result(value: Awaitable[_T]) -> _T:
    """Get the result.

    Ends the wait for the completion of the asynchronous task.
    """

    return asyncio.run(value)


def start(computation: Awaitable[Any]) -> None:
    asyncio.create_task(computation)

    return None


__all__ = [
    "get_awaiter",
    "get_result",
    "from_result",
    "start",
    "TaskCompletionSource",
    "zero",
]
