from __future__ import annotations

from collections.abc import Awaitable, Callable, Iterable
from typing import (
    Any,
    Protocol,
    overload,
)

from .task import from_result, zero
from .util import IDisposable


class Delayed[T](Protocol):
    def __call__(self, __unit: None | None = None) -> Awaitable[T]: ...


class TaskBuilder:
    def Bind[T, U](self, computation: Awaitable[T], binder: Callable[[T], Awaitable[U]]) -> Awaitable[U]:
        async def bind() -> U:
            value = await computation
            return await binder(value)

        return bind()

    def Combine[T](self, computation1: Awaitable[None], computation2: Delayed[T]) -> Awaitable[T]:
        return self.Bind(computation1, computation2)

    def Delay[T](self, generator: Callable[[], Awaitable[T]]) -> Delayed[T]:
        def deferred(_: Any = None) -> Awaitable[T]:
            # print("Delay: deferred: ", generator)
            return generator()

        return deferred

    def For[T](self, sequence: Iterable[T], body: Callable[[T], Awaitable[None]]) -> Awaitable[None]:
        done = False
        it = iter(sequence)
        try:
            cur = next(it)
        except StopIteration:
            done = True

        def delay():
            nonlocal cur, done
            res = body(cur)
            try:
                cur = next(it)
            except StopIteration:
                done = True
            return res

        return self.While(lambda: not done, self.Delay(delay))

    @overload
    def Return(self) -> Awaitable[None]: ...

    @overload
    def Return[T](self, value: T) -> Awaitable[T]: ...

    def Return(self, value: Any = None) -> Awaitable[Any]:
        return from_result(value)

    def ReturnFrom[T](self, computation: Awaitable[T]) -> Awaitable[T]:
        return computation

    def TryFinally[T](self, computation: Delayed[T], compensation: Callable[[], None]) -> Awaitable[T]:
        async def try_finally() -> T:
            try:
                t = await computation()
            finally:
                compensation()
            return t

        return try_finally()

    def TryWith[T](self, computation: Delayed[T], catchHandler: Callable[[Any], Awaitable[T]]) -> Awaitable[T]:
        async def try_with() -> T:
            try:
                t = await computation()
            except Exception as exn:
                t = await catchHandler(exn)
            return t

        return try_with()

    def Using[T: IDisposable, U](self, resource: T, binder: Callable[[T], Awaitable[U]]) -> Awaitable[U]:
        return self.TryFinally(self.Delay(lambda: binder(resource)), lambda: resource.Dispose())

    @overload
    def While(self, guard: Callable[[], bool], computation: Delayed[None]) -> Awaitable[None]: ...

    @overload
    def While[T](self, guard: Callable[[], bool], computation: Delayed[T]) -> Awaitable[T]: ...

    def While(self, guard: Callable[[], bool], computation: Delayed[Any]) -> Awaitable[Any]:
        if guard():
            return self.Bind(computation(), lambda _: self.While(guard, computation))
        else:
            return self.Return()

    def Zero(self) -> Awaitable[None]:
        return zero()

    def Run[T](self, computation: Delayed[T]) -> Awaitable[T]:
        # Make sure we don't execute computation right now, so wrap in a coroutine.
        async def run() -> T:
            return await computation()

        return run()


task = TaskBuilder

__all__ = ["task"]
