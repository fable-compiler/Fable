"""Thread and acquisition controls for the generated F# lock tests."""

import gc
from collections.abc import Callable
from threading import Event, RLock, Thread
from types import TracebackType
from unittest.mock import patch
from weakref import WeakValueDictionary

from fable_library import util
from fable_library.array_ import Array


class Acquisition:
    def __init__(self) -> None:
        self.attempted = Event()
        self.blocked = False


class ObservedLock:
    def __init__(self, acquisition: Acquisition) -> None:
        self.lock = RLock()
        self.acquisition = acquisition

    def __enter__(self) -> "ObservedLock":
        acquired = self.lock.acquire(blocking=False)
        self.acquisition.blocked = not acquired
        # invariant: the contender's observation follows an actual acquisition attempt
        self.acquisition.attempted.set()
        if not acquired:
            self.lock.acquire()
        return self

    def __exit__(
        self,
        exc_type: type[BaseException] | None,
        exc_value: BaseException | None,
        traceback: TracebackType | None,
    ) -> None:
        self.lock.release()


class Threads:
    def __init__(self) -> None:
        self.entered = Event()
        self.release = Event()
        self._trace: list[str] = []
        self._errors: list[str] = []
        self.first_entered = False
        self.second_attempted = False
        self.second_blocked = False
        self.threads_finished = False

    @property
    def trace(self) -> Array[str]:
        return Array(self._trace)

    @property
    def errors(self) -> Array[str]:
        return Array(self._errors)

    def record(self, entry: str) -> None:
        self._trace.append(entry)
        if entry == "first-enter":
            self.entered.set()

    def wait_release(self) -> bool:
        return self.release.wait(5)

    def run(self, first: Callable[[], None], second: Callable[[], None]) -> None:
        acquisition = Acquisition()

        def invoke(action: Callable[[], None]) -> None:
            try:
                action()
            except Exception as error:
                self._errors.append(str(error))

        first_thread = Thread(target=invoke, args=(first,), daemon=True)
        second_thread = Thread(target=invoke, args=(second,), daemon=True)
        with (
            patch.object(util, "RLock", lambda: ObservedLock(acquisition)),
            patch.object(util, "_locks", WeakValueDictionary()),
        ):
            first_thread.start()
            try:
                self.first_entered = self.entered.wait(5)
                if self.first_entered:
                    acquisition.attempted.clear()
                    second_thread.start()
                    self.second_attempted = acquisition.attempted.wait(5)
                    self.second_blocked = acquisition.blocked
            finally:
                self.release.set()
                first_thread.join(5)
                if second_thread.ident is not None:
                    second_thread.join(5)
                self.threads_finished = not first_thread.is_alive() and not second_thread.is_alive()


class Storage:
    def __init__(self) -> None:
        self.before = getattr(util, "_locks", ())
        self.capacity = util.MAX_LOCKS

    @property
    def stable(self) -> bool:
        return getattr(util, "_locks", None) is self.before

    @property
    def count(self) -> int:
        gc.collect()
        return len(getattr(util, "_locks", ()))


def make_key() -> object:
    # decision: uses an unhashable, non-weak-referenceable key to cover arbitrary lock objects
    return []


def make_colliding_keys() -> Array[object]:
    # decision: reproduces the former stripe collision using distinct, unhashable objects
    keys: dict[int, object] = {}
    for _ in range(util.MAX_LOCKS + 1):
        key: object = []
        stripe = (id(key) >> 4) % util.MAX_LOCKS
        previous = keys.get(stripe)
        if previous is not None:
            return Array([previous, key])
        keys[stripe] = key
    raise AssertionError("Expected a collision in the former fixed stripes")


def make_threads() -> Threads:
    return Threads()


def make_storage() -> Storage:
    return Storage()
