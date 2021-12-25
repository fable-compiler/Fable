from typing import Awaitable, TypeVar, Any
import asyncio

T = TypeVar("T")


async def zero():
    return


async def from_result(value: Any):
    return value


def get_awaiter(value: Awaitable[T]) -> Awaitable[T]:
    return value


def get_result(value: Awaitable[T]) -> T:
    return asyncio.run(value)


__all__ = ["get_awaiter", "get_result", "from_result"]
