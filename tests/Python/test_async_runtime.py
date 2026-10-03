from threading import Barrier, Condition, Event, Thread

import pytest
from fable_library.async_builder import CancellationToken


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
