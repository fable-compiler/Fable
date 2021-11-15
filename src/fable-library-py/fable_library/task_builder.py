from typing import Any, Awaitable, Callable, Iterable, Optional, TypeVar, overload

from .task import from_result, zero
from .util import IDisposable

T = TypeVar("T")
U = TypeVar("U")

Delayed = Callable[[], Awaitable[T]]


class TaskBuilder:
    def Bind(self, computation: Awaitable[T], binder: Callable[[T], Awaitable[U]]) -> Awaitable[U]:
        async def bind() -> U:
            value = await computation
            return await binder(value)

        return bind()

    def Combine(self, computation1: Awaitable[None], computation2: Delayed[T]) -> Awaitable[T]:
        return self.Bind(computation1, computation2)

    def Delay(self, generator: Callable[[], Awaitable[T]]) -> Delayed[T]:
        def deferred(_:Any=None) -> Awaitable[T]:
            # print("Delay: deferred: ", generator)
            return generator()

        return deferred

    def For(self, sequence: Iterable[T], body: Callable[[T], Awaitable[U]]) -> Awaitable[U]:
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
    def Return(self) -> Awaitable[None]:
        ...

    @overload
    def Return(self, value: T) -> Awaitable[T]:
        ...

    def Return(self, value: Optional[T] = None) -> Awaitable[Optional[T]]:
        return from_result(value)

    def ReturnFrom(self, computation: Awaitable[T]) -> Awaitable[T]:
        return computation

    def TryFinally(self, computation: Delayed[T], compensation: Callable[[], None]) -> Awaitable[T]:
        async def try_finally() -> T:
            try:
                t = await computation()
            finally:
                compensation()
            return t

        return try_finally()

    def TryWith(self, computation: Delayed[T], catchHandler: Callable[[Any], Awaitable[T]]) -> Awaitable[T]:
        async def try_with() -> T:
            try:
                t = await computation()
            except Exception as exn:
                t = await catchHandler(exn)
            return t

        return try_with()

    def Using(self, resource: T, binder: Callable[[T], Awaitable[U]]) -> Awaitable[U]:
        return self.TryFinally(lambda: binder(resource), lambda: resource.Dispose())

    @overload
    def While(self, guard: Callable[[], bool], computation: Delayed[None]) -> Awaitable[None]:
        ...

    @overload
    def While(self, guard: Callable[[], bool], computation: Delayed[T]) -> Awaitable[T]:
        ...

    def While(self, guard: Callable[[], bool], computation: Delayed[Any]) -> Awaitable[Any]:
        if guard():
            return self.Bind(computation(), lambda _: self.While(guard, computation))
        else:
            return self.Return()

    def Zero(self) -> Awaitable[None]:
        return zero()

    def Run(self, computation: Delayed[T]) -> Awaitable[T]:
        return computation()


task = TaskBuilder

__all__ = ["task"]
