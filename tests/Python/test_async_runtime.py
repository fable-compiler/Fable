import asyncio
from collections.abc import Callable
from threading import Barrier, Condition, Event, Thread

import pytest
from fable_library import async_ as async_runtime
from fable_library.async_builder import CancellationToken, IAsyncContext, Trampoline, singleton
from fable_library.mailbox_processor import MailboxProcessor
from fable_library.util import lock


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


class Timer(asyncio.TimerHandle):
    def __init__(self, callback: Callable[[], None]):
        super().__init__(0, callback, (), asyncio.get_running_loop())
        self.callback = callback
        self.retired = False
        self.cancel_count = 0

    def cancel(self):
        self.retired = True
        self.cancel_count += 1

    def fire(self):
        # Deliberately fire even a retired callback to model an already queued timeout.
        self.callback()


class Scheduler(Trampoline):
    def __init__(self):
        super().__init__()
        self.timers: list[Timer] = []

    def run_later(self, action: Callable[[], None], due_time: float = 0.0):
        timer = Timer(action)
        self.timers.append(timer)
        return timer


@pytest.mark.parametrize("cancel_first", [True, False])
def test_sleep_settles_once_and_cleans_up(cancel_first):
    async def run():
        token, scheduler = CancellationToken(), Scheduler()
        terminal: list[str] = []
        finalizers: list[str] = []
        ctx = IAsyncContext.create(
            scheduler,
            token,
            lambda _: terminal.append("success"),
            lambda _: terminal.append("error"),
            lambda _: terminal.append("cancel"),
        )
        work = singleton.TryFinally(async_runtime.sleep(100), lambda: finalizers.append("finally"))
        work(ctx)
        timer = scheduler.timers[0]
        if cancel_first:
            token.cancel()
            timer.fire()
        else:
            timer.fire()
            token.cancel()
        timer.fire()
        assert terminal == ["cancel" if cancel_first else "success"]
        assert finalizers == ["finally"]
        assert not token.listeners
        assert timer.retired
        assert timer.cancel_count == 1

    asyncio.run(run())


def test_precancelled_work_does_not_execute_or_schedule():
    async def run():
        token, scheduler = CancellationToken(True), Scheduler()
        terminal: list[str] = []
        ctx = IAsyncContext.create(
            scheduler,
            token,
            lambda _: terminal.append("success"),
            lambda _: terminal.append("error"),
            lambda _: terminal.append("cancel"),
        )
        async_runtime.sleep(100)(ctx)
        assert terminal == ["cancel"]
        assert not scheduler.timers
        assert not token.listeners

    asyncio.run(run())


def test_cancellation_finalizer_failure_preserves_cancellation():
    async def run():
        token, scheduler = CancellationToken(), Scheduler()
        terminal: list[str] = []
        finalizers: list[str] = []
        ctx = IAsyncContext.create(
            scheduler,
            token,
            lambda _: terminal.append("success"),
            lambda _: terminal.append("error"),
            lambda _: terminal.append("cancel"),
        )

        def finalizer():
            finalizers.append("finally")
            raise ValueError("finalizer")

        singleton.TryFinally(async_runtime.sleep(100), finalizer)(ctx)
        token.cancel()
        scheduler.timers[0].fire()
        assert terminal == ["cancel"]
        assert finalizers == ["finally"]
        assert not token.listeners

    asyncio.run(run())


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


def test_idle_mailbox_cancellation_requires_an_event():
    # Passing a token alone does not wake Python Receive. A post triggers its cancellation check.
    async def run():
        token = CancellationToken()
        terminal: list[str] = []
        mailbox = MailboxProcessor(lambda _: singleton.Zero(), token)
        ctx = IAsyncContext.create(
            Trampoline(),
            token,
            lambda _: terminal.append("success"),
            lambda _: terminal.append("error"),
            lambda _: terminal.append("cancel"),
        )
        mailbox.receive()(ctx)
        token.cancel()
        assert terminal == []
        assert mailbox.continuation is not None
        mailbox.post(42)
        assert terminal == ["cancel"]
        assert mailbox.continuation is None
        assert not token.listeners

    asyncio.run(run())


def test_foreign_thread_sleep_cancellation_runs_on_owner_loop():
    async def run():
        token = CancellationToken()
        loop = asyncio.get_running_loop()
        finished = loop.create_future()
        terminal: list[str] = []

        def cancelled(_):
            assert asyncio.get_running_loop() is loop
            terminal.append("cancel")
            finished.set_result(None)

        ctx = IAsyncContext.create(
            Trampoline(), token, lambda _: terminal.append("success"), lambda _: terminal.append("error"), cancelled
        )
        async_runtime.sleep(100000)(ctx)
        thread = Thread(target=token.cancel)
        thread.start()
        await asyncio.wait_for(finished, 5)
        thread.join(5)
        assert terminal == ["cancel"]
        assert not token.listeners

    asyncio.run(run())


@pytest.mark.parametrize("winner", ["reply", "cancel", "timeout"])
def test_concurrent_reply_cancel_timeout_settlement(winner):
    async def run():
        token, scheduler = CancellationToken(), Scheduler()
        key = object()
        gate = Barrier(4)
        winner_done = Event()
        outcome: list[str] = []
        terminal: list[str] = []
        finalizers: list[str] = []
        loop = asyncio.get_running_loop()
        finished = loop.create_future()
        ctx = IAsyncContext.create(
            scheduler,
            token,
            lambda _: terminal.append("success"),
            lambda _: terminal.append("error"),
            lambda _: terminal.append("cancel"),
        )

        def deadline():
            def callback():
                finalizers.append("finally")
                choose("timeout")

            return singleton.TryFinally(async_runtime.sleep(100), callback)

        def choose(tag):
            def settle():
                if outcome:
                    return
                outcome.append(tag)
                if tag == "reply":
                    # The winning reply retires its deadline before admitting competing signals.
                    token.cancel()
                winner_done.set()
                loop.call_soon_threadsafe(finished.set_result, None)

            lock(key, settle)

        registration = token.register(lambda: choose("cancel"))
        deadline()(ctx)
        timer = scheduler.timers[0]

        def compete(tag):
            gate.wait(5)
            if tag != winner:
                assert winner_done.wait(5)
            if tag == "reply":
                choose("reply")
            elif tag == "cancel":
                token.cancel()
            else:
                loop.call_soon_threadsafe(timer.fire)

        threads = [Thread(target=compete, args=(tag,)) for tag in ("reply", "cancel", "timeout")]
        for thread in threads:
            thread.start()
        gate.wait(5)
        await asyncio.wait_for(finished, 5)
        for thread in threads:
            thread.join(5)
            assert not thread.is_alive()
        registration.Dispose()
        token.cancel()
        # Retire the deadline after any winner. Cancellation's finalizer must not settle again.
        timer.fire()
        await asyncio.sleep(0)
        assert outcome == [winner]
        assert terminal == (["success"] if winner == "timeout" else ["cancel"])
        assert finalizers == ["finally"]
        assert not token.listeners
        assert timer.cancel_count == 1

    asyncio.run(run())


@pytest.mark.parametrize("cancel_during_install", [True, False])
def test_sleep_installation_releases_resources(cancel_during_install):
    async def run():
        token = CancellationToken()
        terminal: list[str] = []

        class InstallingScheduler(Scheduler):
            def run_later(self, action: Callable[[], None], due_time: float = 0.0):
                if not cancel_during_install:
                    raise ValueError("scheduler")
                timer = super().run_later(action, due_time)
                token.cancel()
                return timer

        scheduler = InstallingScheduler()
        ctx = IAsyncContext.create(
            scheduler,
            token,
            lambda _: terminal.append("success"),
            lambda _: terminal.append("error"),
            lambda _: terminal.append("cancel"),
        )
        async_runtime.sleep(100)(ctx)
        assert terminal == ["cancel" if cancel_during_install else "error"]
        assert not token.listeners
        if cancel_during_install:
            timer = scheduler.timers[0]
            timer.fire()
            assert timer.cancel_count == 1
            assert terminal == ["cancel"]

    asyncio.run(run())


@pytest.mark.parametrize("body_fails", [True, False])
def test_finalizer_failure_overrides_success_or_error_once(body_fails):
    async def run():
        ctx = IAsyncContext.create(
            Trampoline(),
            CancellationToken(),
            lambda _: terminal.append("success"),
            lambda error: terminal.append(str(error)),
            lambda _: terminal.append("cancel"),
        )
        terminal: list[str] = []
        finalizers: list[str] = []

        def body(context):
            if body_fails:
                context.on_error(ValueError("body"))
            else:
                context.on_success(42)

        def finalizer():
            finalizers.append("finally")
            raise ValueError("finalizer")

        singleton.TryFinally(body, finalizer)(ctx)
        assert terminal == ["finalizer"]
        assert finalizers == ["finally"]

    asyncio.run(run())
