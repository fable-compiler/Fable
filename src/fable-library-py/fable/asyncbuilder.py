from typing import Any, Callable, Optional, TypeVar, Awaitable
from expression.core.aiotools import from_result

T = TypeVar("T")
U = TypeVar("U")


class AsyncBuilder:
    def Bind(self, computation: Awaitable[T], binder: Callable[[T], Awaitable[U]]) -> Awaitable[U]:
        async def bind() -> U:
            t = await computation
            return await binder(t)

        return bind()

    def Combine(self, computation1: Awaitable[None], computation2: Awaitable[T]) -> Awaitable[T]:
        return self.Bind(computation1, lambda _: computation2)

    def Delay(self, generator: Callable[[], Awaitable[T]]) -> Awaitable[T]:
        async def deferred() -> T:
            return await generator()

        return deferred()

    def Return(self, value: Optional[T] = None) -> Awaitable[Optional[T]]:
        return from_result(value)

    def ReturnFrom(self, computation: Awaitable[T]) -> Awaitable[T]:
        return computation

    def TryFinally(self, computation: Awaitable[T], compensation: Callable[[], None]) -> Awaitable[T]:
        async def try_finally() -> T:
            try:
                t = await computation
            finally:
                compensation()
            return t

        return try_finally()

    def TryWith(self, computation: Awaitable[T], catchHandler: Callable[[Any], Awaitable[T]]) -> Awaitable[T]:
        async def try_with() -> T:
            try:
                t = await computation
            except Exception as exn:
                t = await catchHandler(exn)
            return t

        return try_with()

    def Using(self, resource: T, binder: Callable[[T], Awaitable[U]]) -> Awaitable[U]:
        return self.TryFinally(binder(resource), lambda: resource.Dispose())

    def While(self, guard: Callable[[], bool], computation: Awaitable[None]) -> Awaitable[None]:
        if guard():
            return self.Bind(computation, lambda _: self.While(guard, computation))
        else:
            return self.Return()

    def Zero(self) -> Awaitable[None]:
        return from_result(None)


singleton = AsyncBuilder()

__all__ = ["singleton"]
