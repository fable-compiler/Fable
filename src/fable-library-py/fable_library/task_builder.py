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
    async def Bind[T, U](self, computation: Awaitable[T], binder: Callable[[T], Awaitable[U]]) -> U:
        async def bind() -> U:
            value = await computation
            return await binder(value)

        return await bind()

    async def Combine[T](self, computation1: Awaitable[None], computation2: Delayed[T]) -> T:
        return await self.Bind(computation1, computation2)

    def Delay[T](self, generator: Callable[[], Awaitable[T]]) -> Delayed[T]:
        def deferred(_: Any = None) -> Awaitable[T]:
            # print("Delay: deferred: ", generator)
            return generator()

        return deferred

    async def For[T](self, sequence: Iterable[T], body: Callable[[T], Awaitable[None]]) -> None:
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

        return await self.While(lambda: not done, self.Delay(delay))

    @overload
    async def Return(self) -> None: ...

    @overload
    async def Return[T](self, value: T) -> T: ...

    async def Return(self, value: Any = None) -> Any:
        return await from_result(value)

    async def ReturnFrom[T](self, computation: Awaitable[T]) -> T:
        return await computation

    async def TryFinally[T](self, computation: Delayed[T], compensation: Callable[[], None]) -> T:
        async def try_finally() -> T:
            try:
                t = await computation()
            finally:
                compensation()
            return t

        return await try_finally()

    async def TryWith[T](self, computation: Delayed[T], catchHandler: Callable[[Any], Awaitable[T]]) -> T:
        async def try_with() -> T:
            try:
                t = await computation()
            except Exception as exn:
                t = await catchHandler(exn)
            return t

        return await try_with()

    async def Using[T: IDisposable, U](self, resource: T, binder: Callable[[T], Awaitable[U]]) -> U:
        return await self.TryFinally(self.Delay(lambda: binder(resource)), lambda: resource.Dispose())

    @overload
    async def While(self, guard: Callable[[], bool], computation: Delayed[None]) -> None: ...

    @overload
    async def While[T](self, guard: Callable[[], bool], computation: Delayed[T]) -> T: ...

    async def While(self, guard: Callable[[], bool], computation: Delayed[Any]) -> Any:
        if guard():
            return await self.Bind(computation(), lambda _: self.While(guard, computation))
        else:
            return await self.Return()

    async def Zero(self) -> None:
        return await zero()

    async def Run[T](self, computation: Delayed[T]) -> T:
        # Make sure we don't execute computation right now, so wrap in a coroutine.
        async def run() -> T:
            return await computation()

        return await run()


task = TaskBuilder

__all__ = ["task"]
