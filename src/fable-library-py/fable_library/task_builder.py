from typing import (
    Any,
    Awaitable,
    Callable,
    Iterable,
    Optional,
    Protocol,
    TypeVar,
    overload,
)

from .task import from_result, zero
from .util import IDisposable


_T = TypeVar("_T")
_T_co = TypeVar("_T_co", covariant=True)
_TD = TypeVar("_TD", bound=IDisposable)
_U = TypeVar("_U")


class Delayed(Protocol[_T_co]):
    def __call__(self, __unit: Optional[None] = None) -> Awaitable[_T_co]:
        ...


class TaskBuilder:
    def Bind(
        self, computation: Awaitable[_T], binder: Callable[[_T], Awaitable[_U]]
    ) -> Awaitable[_U]:
        async def bind() -> _U:
            value = await computation
            return await binder(value)

        return bind()

    def Combine(
        self, computation1: Awaitable[None], computation2: Delayed[_T]
    ) -> Awaitable[_T]:
        return self.Bind(computation1, computation2)

    def Delay(self, generator: Callable[[], Awaitable[_T]]) -> Delayed[_T]:
        def deferred(_: Any = None) -> Awaitable[_T]:
            # print("Delay: deferred: ", generator)
            return generator()

        return deferred

    def For(
        self, sequence: Iterable[_T], body: Callable[[_T], Awaitable[_U]]
    ) -> Awaitable[_U]:
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
    def Return(self, value: _T) -> Awaitable[_T]:
        ...

    def Return(self, value: Any = None) -> Awaitable[Any]:
        return from_result(value)

    def ReturnFrom(self, computation: Awaitable[_T]) -> Awaitable[_T]:
        return computation

    def TryFinally(
        self, computation: Delayed[_T], compensation: Callable[[], None]
    ) -> Awaitable[_T]:
        async def try_finally() -> _T:
            try:
                t = await computation()
            finally:
                compensation()
            return t

        return try_finally()

    def TryWith(
        self, computation: Delayed[_T], catchHandler: Callable[[Any], Awaitable[_T]]
    ) -> Awaitable[_T]:
        async def try_with() -> _T:
            try:
                t = await computation()
            except Exception as exn:
                t = await catchHandler(exn)
            return t

        return try_with()

    def Using(
        self, resource: _TD, binder: Callable[[_TD], Awaitable[_U]]
    ) -> Awaitable[_U]:
        return self.TryFinally(
            self.Delay(lambda: binder(resource)), lambda: resource.Dispose()
        )

    @overload
    def While(
        self, guard: Callable[[], bool], computation: Delayed[None]
    ) -> Awaitable[None]:
        ...

    @overload
    def While(
        self, guard: Callable[[], bool], computation: Delayed[_T]
    ) -> Awaitable[_T]:
        ...

    def While(
        self, guard: Callable[[], bool], computation: Delayed[Any]
    ) -> Awaitable[Any]:
        if guard():
            return self.Bind(computation(), lambda _: self.While(guard, computation))
        else:
            return self.Return()

    def Zero(self) -> Awaitable[None]:
        return zero()

    def Run(self, computation: Delayed[_T]) -> Awaitable[_T]:
        # Make sure we don't execute computation right now, so wrap in a coroutine.
        async def run() -> _T:
            return await computation()

        return run()


task = TaskBuilder

__all__ = ["task"]
