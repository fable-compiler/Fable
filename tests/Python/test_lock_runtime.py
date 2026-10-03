from threading import Event, RLock, Thread

import pytest
from fable_library import util


class ObservedLock:
    def __init__(self, observed: Event):
        self.lock = RLock()
        self.observed = observed

    def __enter__(self):
        acquired = self.lock.acquire(blocking=False)
        self.observed.set()
        if not acquired:
            self.lock.acquire()
        return self

    def __exit__(self, *args):
        self.lock.release()


def test_real_threads_share_lock_and_release_after_exception(monkeypatch):
    key = []  # Unhashable and not weak-referenceable objects must also work.
    entered, release, attempted = Event(), Event(), Event()
    trace: list[str] = []
    observed = ObservedLock(attempted)
    # Instrument acquisition, so the first thread releases only after the second actually attempts entry.
    monkeypatch.setattr(util, "RLock", lambda: ObservedLock(attempted))
    if hasattr(util, "_locks"):
        monkeypatch.setattr(util, "_locks", (observed,) * util.MAX_LOCKS)

    def first_body():
        trace.append("first-enter")
        entered.set()
        assert release.wait(5)
        trace.append("first-exit")
        raise ValueError("body")

    def first():
        with pytest.raises(ValueError, match="body"):
            util.lock(key, first_body)

    def second():
        assert entered.wait(5)
        util.lock(key, lambda: trace.append("second-enter"))
        trace.append("second-exit")

    first_thread = Thread(target=first)
    first_thread.start()
    assert entered.wait(5)
    attempted.clear()
    second_thread = Thread(target=second)
    second_thread.start()
    assert attempted.wait(5)
    release.set()
    first_thread.join(5)
    second_thread.join(5)
    assert not first_thread.is_alive() and not second_thread.is_alive()
    assert trace == ["first-enter", "first-exit", "second-enter", "second-exit"]
    assert util.lock(key, lambda: 42) == 42


def test_lock_is_reentrant_and_storage_is_bounded():
    key = object()
    assert util.lock(key, lambda: util.lock(key, lambda: 42)) == 42
    if hasattr(util, "_locks"):
        before = util._locks
        for _ in range(util.MAX_LOCKS * 2):
            assert util.lock([], lambda: 1) == 1
        assert util._locks is before
        assert len(util._locks) == util.MAX_LOCKS
