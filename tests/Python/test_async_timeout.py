import asyncio
from collections.abc import Callable

import pytest
from fable_library.async_ import start_as_task, start_child, start_child_with_timeout
from fable_library.async_builder import Async, IAsyncContext
from fable_library.system import TimeoutException


@pytest.mark.parametrize(
    ("start_child_fn", "timeout_type"),
    [(start_child, TimeoutError), (start_child_with_timeout, TimeoutException)],
)
def test_start_child_timeout_preserves_exception_contract(
    start_child_fn: Callable[[Async[None], int], Async[Async[None]]], timeout_type: type[Exception]
) -> None:
    def pending(_ctx: IAsyncContext[None]) -> None:
        pass

    async def run() -> None:
        child = await start_as_task(start_child_fn(pending, 0))
        await start_as_task(child)

    with pytest.raises(timeout_type):
        asyncio.run(run())
