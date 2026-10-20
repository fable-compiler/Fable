import asyncio
from collections.abc import Callable
from threading import Barrier, Condition, Event, Thread

import pytest
from fable_library.async_ import await_event, await_task_with_cancellation, sleep, start_with_continuations
from fable_library.async_builder import (
    Async,
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


@pytest.mark.parametrize("channel", ["success", "error", "cancel"])
def test_try_finally_propagates_downstream_errors_once(channel: str) -> None:
    from fable_library.async_builder import OperationCanceledError, singleton

    async def run() -> None:
        cleanups = 0
        calls: list[str] = []
        trampoline = Trampoline()
        token = CancellationToken()
        defect = ValueError("downstream")

        def cleanup() -> None:
            nonlocal cleanups
            cleanups += 1

        def terminal(value: object) -> None:
            calls.append(channel)
            raise defect

        def body(ctx: IAsyncContext[int]) -> None:
            assert ctx.trampoline is trampoline
            assert ctx.cancel_token is token
            if channel == "success":
                ctx.on_success(42)
            elif channel == "error":
                ctx.on_error(ValueError("body"))
            else:
                ctx.on_cancel(OperationCanceledError())

        ctx = IAsyncContext.create(trampoline, token, terminal, terminal, terminal)
        with pytest.raises(ValueError) as caught:
            singleton.TryFinally(body, cleanup)(ctx)
        assert caught.value is defect
        assert cleanups == 1
        assert calls == [channel]

    asyncio.run(run())


@pytest.mark.parametrize("channel", ["success", "error", "cancel"])
def test_generic_from_continuations_propagates_terminal_exceptions(channel: str) -> None:
    from fable_library.async_ import from_continuations
    from fable_library.async_builder import Continuations, OperationCanceledError

    async def run() -> None:
        calls: list[str] = []
        defect = ValueError("downstream")
        trampoline = Trampoline()
        token = CancellationToken()

        def terminal(value: object) -> None:
            calls.append(channel)
            raise defect

        def callback(conts: Continuations[int]) -> None:
            ok, error, cancelled = conts
            if channel == "success":
                ok(42)
            elif channel == "error":
                error(ValueError("body"))
            else:
                cancelled(OperationCanceledError())

        ctx = IAsyncContext.create(trampoline, token, terminal, terminal, terminal)
        with pytest.raises(ValueError) as caught:
            from_continuations(callback)(ctx)
        assert caught.value is defect
        assert calls == [channel]
        assert ctx.trampoline is trampoline

    asyncio.run(run())


def test_generic_from_continuations_rejects_duplicate_completion() -> None:
    from fable_library.async_ import from_continuations
    from fable_library.async_builder import Continuations

    calls: list[int] = []

    def callback(conts: Continuations[int]) -> None:
        conts[0](42)
        conts[0](43)

    with pytest.raises(RuntimeError, match="invoked more than once"):
        start_with_continuations(from_continuations(callback), calls.append)
    assert calls == [42]


def test_generic_from_continuations_claims_concurrent_completion_once() -> None:
    from fable_library.async_ import from_continuations
    from fable_library.async_builder import Continuations

    callbacks: Continuations[int] | None = None
    calls: list[str] = []
    rejected: list[Exception] = []
    barrier = Barrier(2)

    def callback(conts: Continuations[int]) -> None:
        nonlocal callbacks
        callbacks = conts

    start_with_continuations(
        from_continuations(callback),
        lambda _: calls.append("success"),
        lambda _: calls.append("error"),
    )
    assert callbacks is not None
    ok, error, _ = callbacks

    def complete(success: bool) -> None:
        barrier.wait(5)
        try:
            if success:
                ok(42)
            else:
                error(ValueError("body"))
        except RuntimeError as duplicate:
            rejected.append(duplicate)

    threads = [Thread(target=complete, args=(success,)) for success in (True, False)]
    for thread in threads:
        thread.start()
    for thread in threads:
        thread.join(5)
    assert all(not thread.is_alive() for thread in threads)
    assert len(calls) == 1
    assert len(rejected) == 1


@pytest.mark.parametrize("combinator", ["try_with", "bind"])
def test_completed_continuation_exceptions_are_not_redirected(combinator: str) -> None:
    from fable_library.async_builder import singleton

    calls: list[str] = []
    defect = ValueError("downstream")

    if combinator == "try_with":
        work = singleton.TryWith(singleton.Return(1), lambda _: singleton.Return(99))
    else:
        work = singleton.Bind(singleton.Return(1), lambda x: singleton.Return(x + 1))

    def success(value: int) -> None:
        calls.append(f"success:{value}")
        raise defect

    with pytest.raises(ValueError) as caught:
        start_with_continuations(work, success, lambda _: calls.append("error"), lambda _: calls.append("cancel"))
    assert caught.value is defect
    assert len(calls) == 1


def test_workflow_errors_still_reach_the_handler_before_completion() -> None:
    from fable_library.async_builder import singleton

    def failing_binder(_: int) -> Async[str]:
        raise ValueError("binder")

    def handler(error: Exception) -> Async[str]:
        return singleton.Return(f"handled:{error}")

    results: list[object] = []
    work = singleton.TryWith(singleton.Bind(singleton.Return(1), failing_binder), handler)
    start_with_continuations(work, results.append, lambda e: results.append(("error", e)))
    assert results == ["handled:binder"]


@pytest.mark.parametrize("channel", ["success", "error", "cancel"])
def test_unit_from_continuations_propagates_terminal_exceptions(channel: str) -> None:
    from fable_library.async_ import from_continuations_unit
    from fable_library.async_builder import OperationCanceledError

    calls: list[str] = []
    defect = ValueError("downstream")

    def terminal(value: object = None) -> None:
        calls.append(channel)
        raise defect

    def callback(
        conts: tuple[Callable[[], None], Callable[[Exception], None], Callable[[OperationCanceledError], None]],
    ) -> None:
        ok, error, cancelled = conts
        if channel == "success":
            ok()
        elif channel == "error":
            error(ValueError("body"))
        else:
            cancelled(OperationCanceledError())

    with pytest.raises(ValueError) as caught:
        start_with_continuations(from_continuations_unit(callback), terminal, terminal, terminal)
    assert caught.value is defect
    assert calls == [channel]


@pytest.mark.parametrize("explicit_unit", [False, True])
def test_unit_from_continuations_accepts_erased_and_explicit_unit(explicit_unit: bool) -> None:
    from fable_library.async_ import from_continuations_unit

    calls: list[object] = []

    def callback(conts: tuple[Callable[..., None], object, object]) -> None:
        if explicit_unit:
            conts[0](None)
        else:
            conts[0]()

    start_with_continuations(from_continuations_unit(callback), calls.append, lambda e: calls.append(("error", e)))
    assert calls == [None]


def test_unit_from_continuations_rejects_duplicate_completion() -> None:
    from fable_library.async_ import from_continuations_unit

    calls: list[object] = []

    def callback(conts: tuple[Callable[[], None], object, object]) -> None:
        conts[0]()
        conts[0]()

    with pytest.raises(RuntimeError, match="invoked more than once"):
        start_with_continuations(from_continuations_unit(callback), calls.append)
    assert calls == [None]


def test_try_finally_claims_concurrent_completion_once() -> None:
    from fable_library.async_builder import OperationCanceledError, singleton

    captured: list[IAsyncContext[int]] = []
    calls: list[str] = []
    cleanups: list[int] = []
    barrier = Barrier(2)

    def body(ctx: IAsyncContext[int]) -> None:
        captured.append(ctx)

    start_with_continuations(
        singleton.TryFinally(body, lambda: cleanups.append(1)),
        lambda _: calls.append("success"),
        lambda _: calls.append("error"),
        lambda _: calls.append("cancel"),
    )
    assert len(captured) == 1
    ctx = captured[0]

    def complete(success: bool) -> None:
        barrier.wait(5)
        if success:
            ctx.on_success(42)
        else:
            ctx.on_cancel(OperationCanceledError())

    threads = [Thread(target=complete, args=(success,)) for success in (True, False)]
    for thread in threads:
        thread.start()
    for thread in threads:
        thread.join(5)
    assert all(not thread.is_alive() for thread in threads)
    assert len(cleanups) == 1
    assert len(calls) == 1


def test_post_and_async_reply_delivers_once_when_reply_races_with_start() -> None:
    from fable_library.mailbox_processor import AsyncReplyChannel, MailboxProcessor

    for _ in range(20):
        channels: list[AsyncReplyChannel[int]] = []
        calls: list[object] = []
        failures: list[BaseException] = []
        barrier = Barrier(2)
        mailbox: MailboxProcessor[AsyncReplyChannel[int]] = MailboxProcessor(lambda _: lambda _: None)

        def build(channel: AsyncReplyChannel[int]) -> AsyncReplyChannel[int]:
            channels.append(channel)
            return channel

        work = mailbox.post_and_async_reply(build)
        assert len(channels) == 1

        def start() -> None:
            barrier.wait(5)
            try:
                start_with_continuations(work, calls.append, lambda e: failures.append(e), lambda e: failures.append(e))
            except BaseException as error:
                failures.append(error)

        def reply() -> None:
            barrier.wait(5)
            try:
                channels[0].reply(42)
            except BaseException as error:
                failures.append(error)

        threads = [Thread(target=start), Thread(target=reply)]
        for thread in threads:
            thread.start()
        for thread in threads:
            thread.join(5)
        assert all(not thread.is_alive() for thread in threads)
        assert failures == []
        assert calls == [42]
        start_with_continuations(work, calls.append, lambda e: failures.append(e), lambda e: failures.append(e))
        assert failures == []
        assert calls == [42, 42]


@pytest.mark.parametrize("value", [None, 42])
@pytest.mark.parametrize("reply_before_start", [False, True])
def test_post_and_async_reply_can_be_reused(value: int | None, reply_before_start: bool) -> None:
    from fable_library.mailbox_processor import AsyncReplyChannel, MailboxProcessor

    channels: list[AsyncReplyChannel[int | None]] = []
    calls: list[object] = []
    mailbox: MailboxProcessor[AsyncReplyChannel[int | None]] = MailboxProcessor(lambda _: lambda _: None)

    def build(channel: AsyncReplyChannel[int | None]) -> AsyncReplyChannel[int | None]:
        channels.append(channel)
        return channel

    work = mailbox.post_and_async_reply(build)
    if reply_before_start:
        channels[0].reply(value)
    start_with_continuations(work, calls.append, lambda e: calls.append(("error", e)))
    if not reply_before_start:
        channels[0].reply(value)
    assert calls == [value]

    start_with_continuations(work, calls.append, lambda e: calls.append(("error", e)))
    assert calls == [value, value]
    assert len(channels) == 1


def test_post_and_async_reply_ignores_a_second_reply() -> None:
    from fable_library.mailbox_processor import AsyncReplyChannel, MailboxProcessor

    channels: list[AsyncReplyChannel[int]] = []
    calls: list[object] = []
    mailbox: MailboxProcessor[AsyncReplyChannel[int]] = MailboxProcessor(lambda _: lambda _: None)

    def build(channel: AsyncReplyChannel[int]) -> AsyncReplyChannel[int]:
        channels.append(channel)
        return channel

    work = mailbox.post_and_async_reply(build)
    start_with_continuations(work, calls.append, lambda e: calls.append(("error", e)))
    channels[0].reply(1)
    channels[0].reply(2)
    assert calls == [1]
