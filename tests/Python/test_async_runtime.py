import asyncio
from threading import Barrier, Condition, Event, Thread

import pytest
from fable_library.async_ import await_event, await_task_with_cancellation, sleep, start_with_continuations
from fable_library.async_builder import (
    CancellationCallbackError,
    CancellationToken,
    IAsyncContext,
    OperationCanceledError,
    TaskCanceledError,
    Trampoline,
)
from fable_library.event import Event as FableEvent


def test_registration_handle_and_disposal():
    token = CancellationToken()
    calls: list[str] = []
    first = token.register(lambda: calls.append("first"))
    second = token.register(lambda: calls.append("second"))
    assert first is not None
    first.Dispose()
    first.Dispose()
    token.cancel()
    second.Dispose()
    assert calls == ["second"]
    assert not token.listeners


def test_late_registration_and_falsy_state():
    token = CancellationToken(True)
    calls: list[object] = []
    handle = token.register(lambda state: calls.append(state), 0)
    assert calls == [0]
    token.register(lambda state: calls.append(state), None).Dispose()
    assert calls == [0, None]
    handle.Dispose()
    assert not token.listeners


def test_self_disposal_and_disposal_of_pending_callback():
    token = CancellationToken()
    calls: list[str] = []
    first = token.register(lambda: calls.append("first"))

    def callback():
        second.Dispose()
        first.Dispose()
        calls.append("second")

    second = token.register(callback)
    token.cancel()
    token.cancel()
    assert calls == ["second"]
    assert not token.listeners


def test_callback_failures_do_not_skip_other_callbacks():
    token = CancellationToken()
    calls: list[str] = []
    token.register(lambda: calls.append("other"))

    def failing():
        raise ValueError("callback")

    token.register(failing)
    with pytest.raises(ExceptionGroup) as errors:
        token.cancel()
    assert len(errors.value.exceptions) == 1
    assert calls == ["other"]
    assert not token.listeners


@pytest.mark.parametrize("count", [1, 2])
def test_callback_errors_preserve_all_errors_and_first_inner_exception(count: int) -> None:
    token = CancellationToken()
    errors = [ValueError(str(i)) for i in range(count)]

    def failing(error: Exception) -> None:
        raise error

    for error in errors:
        token.register(failing, error)

    with pytest.raises(CancellationCallbackError) as caught:
        token.cancel()
    assert isinstance(caught.value, ExceptionGroup)
    assert caught.value.exceptions == tuple(reversed(errors))
    assert caught.value.inner_exception is errors[-1]
    assert not token.listeners
    token.cancel()


def test_dispose_waits_for_running_callback_and_late_registration_runs_outside_lock():
    token = CancellationToken()
    entered, release, disposed, late = Event(), Event(), Event(), Event()

    def callback():
        entered.set()
        assert release.wait(5)

    waiting = Event()

    class ObservedCondition(Condition):
        def wait(self, timeout=None):
            waiting.set()
            return super().wait(timeout)

    token._condition = ObservedCondition(token.lock)
    handle = token.register(callback)
    cancel_thread = Thread(target=token.cancel)
    cancel_thread.start()
    assert entered.wait(5)

    # Registration after cancellation must run immediately, even while an earlier callback is blocked.
    register_thread = Thread(target=lambda: token.register(late.set).Dispose())
    register_thread.start()
    assert late.wait(5)
    register_thread.join(5)

    def dispose():
        handle.Dispose()
        disposed.set()

    dispose_thread = Thread(target=dispose)
    dispose_thread.start()
    assert waiting.wait(5)
    assert not disposed.is_set()
    release.set()
    cancel_thread.join(5)
    dispose_thread.join(5)
    assert disposed.is_set()
    assert not token.listeners


def test_concurrent_registration_and_cancellation():
    token = CancellationToken()
    gate = Barrier(17)
    calls = [0] * 16

    def register(index: int):
        gate.wait(5)

        def callback():
            calls[index] += 1

        token.register(callback).Dispose()

    threads = [Thread(target=register, args=(i,)) for i in range(16)]
    for thread in threads:
        thread.start()
    gate.wait(5)
    token.cancel()
    for thread in threads:
        thread.join(5)
        assert not thread.is_alive()
    assert all(count in (0, 1) for count in calls)
    assert not token.listeners
    assert not token._running


def test_sleep_on_cancelled_token_settles_once():
    calls: list[str] = []
    start_with_continuations(
        sleep(10),
        lambda _: calls.append("success"),
        lambda _: calls.append("error"),
        lambda _: calls.append("cancel"),
        CancellationToken(True),
    )
    assert calls == ["cancel"]


@pytest.mark.parametrize("already_cancelled", [False, True])
def test_await_task_cancellation_uses_error_continuation(already_cancelled: bool) -> None:
    async def run() -> None:
        loop = asyncio.get_running_loop()
        unhandled: list[object] = []
        loop.set_exception_handler(lambda _, context: unhandled.append(context.get("exception")))
        task: asyncio.Future[int] = loop.create_future()
        token = CancellationToken()
        calls: list[str] = []
        errors: list[Exception] = []
        if already_cancelled:
            task.cancel()

        def on_error(error: Exception) -> None:
            calls.append("error")
            errors.append(error)

        start_with_continuations(
            await_task_with_cancellation(task),
            lambda _: calls.append("success"),
            on_error,
            lambda _: calls.append("cancel"),
            token,
        )
        await asyncio.sleep(0)
        if not already_cancelled:
            task.cancel()
        await asyncio.sleep(0)
        await asyncio.sleep(0)
        assert calls == ["error"]
        assert len(errors) == 1
        assert isinstance(errors[0], TaskCanceledError)
        assert isinstance(errors[0], OperationCanceledError)
        assert errors[0].inner_exception is None
        assert not token.is_cancelled
        assert not unhandled

    asyncio.run(run())


class _DeferringTrampoline(Trampoline):
    def increment_and_check(self) -> bool:
        return True


def test_await_event_cancelled_before_its_body_runs_does_not_subscribe():
    event: FableEvent[int] = FableEvent()
    token = CancellationToken()
    calls: list[str] = []

    async def run() -> None:
        ctx = IAsyncContext.create(
            _DeferringTrampoline(),
            token,
            lambda value: calls.append(f"success {value}"),
            lambda _: calls.append("error"),
            lambda _: calls.append("cancel"),
        )
        await_event(event)(ctx)
        token.cancel()
        await asyncio.sleep(0)

    asyncio.run(run())
    event.Trigger(42)
    assert calls == ["cancel"]
    assert not event.delegates


def test_sleep_cancelled_while_its_loop_is_stopped_settles_once():
    loop = asyncio.new_event_loop()
    token = CancellationToken()
    calls: list[str] = []

    async def start() -> None:
        start_with_continuations(
            sleep(100_000),
            lambda _: calls.append("success"),
            lambda _: calls.append("error"),
            lambda _: calls.append("cancel"),
            token,
        )

    try:
        loop.run_until_complete(start())
        cancel_thread = Thread(target=token.cancel)
        cancel_thread.start()
        cancel_thread.join(5)
        assert calls == ["cancel"]
        assert not token.listeners
    finally:
        loop.close()
