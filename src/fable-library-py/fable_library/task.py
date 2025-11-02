"""Task handling.

This module implements .NET
[tasks](https://docs.microsoft.com/en-us/dotnet/standard/async-in-depth)
using Python async / await.
"""

from __future__ import annotations

import asyncio
from asyncio import AbstractEventLoop, Future
from collections.abc import Awaitable
from typing import Any, TypeVar


_T = TypeVar("_T")


class TaskCompletionSource[T]:
    __slots__ = "future", "loop"

    def __init__(self) -> None:
        self.loop: AbstractEventLoop = asyncio.get_event_loop()
        self.future: Future[T] = self.loop.create_future()

    def SetResult(self, value: T) -> None:
        """Set result.

        Transitions the underlying Task[TResult] into the
        RanToCompletion state.
        """

        def action():
            self.future.set_result(value)

        self.loop.call_soon_threadsafe(action)

    def SetCancelled(self) -> None:
        """Set cancelled.

        Transitions the underlying Task[TResult] into the Canceled
        state.
        """

        def action():
            self.future.cancel()

        self.loop.call_soon_threadsafe(action)

    def SetException(self, exception: Exception) -> None:
        """Set exception.

        Transitions the underlying Task[TResult] into the Faulted state
        and binds it to a specified exception.
        """

        def action():
            self.future.set_exception(exception)

        self.loop.call_soon_threadsafe(action)

    def get_task(self) -> Awaitable[T]:
        return asyncio.ensure_future(self.future)


async def zero() -> None:
    return


async def from_result[T](value: T) -> T:
    return value


def get_awaiter[T](value: Awaitable[T]) -> Awaitable[T]:
    # Wrap awaitable in coroutine (so we can run it using create_task)
    async def get_value() -> T:
        return await asyncio.ensure_future(value)

    return get_value()


def get_result[T](value: Awaitable[T]) -> T:
    """Get the result.

    Ends the wait for the completion of the asynchronous task.
    """

    async def runner() -> T:
        return await value

    return asyncio.run(runner())


_running_tasks = set[Awaitable[Any]]()


def start(task: Awaitable[Any]) -> None:
    async def runner():
        result = await task
        _running_tasks.remove(task)
        return result

    task = asyncio.create_task(runner())
    _running_tasks.add(task)


def run_synchronously(task: Awaitable[Any]) -> None:
    async def runner():
        return await task

    asyncio.run(runner())


__all__ = [
    "TaskCompletionSource",
    "from_result",
    "get_awaiter",
    "get_result",
    "run_synchronously",
    "start",
    "zero",
]
