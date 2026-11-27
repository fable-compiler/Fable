from __future__ import annotations

import time

from .core import int64
from .time_span import TimeSpan, from_ticks


class StopWatch:
    def __init__(self) -> None:
        self._started_at: int | None = None
        self._elapsed_ns = 0

    def is_running(self) -> bool:
        return self._started_at is not None

    def start(self) -> None:
        if self._started_at is None:
            self._started_at = time.perf_counter_ns()

    def stop(self) -> None:
        if self._started_at is not None:
            self._elapsed_ns += time.perf_counter_ns() - self._started_at
            self._started_at = None

    def reset(self) -> None:
        self._started_at = None
        self._elapsed_ns = 0

    def restart(self) -> None:
        self._elapsed_ns = 0
        self._started_at = time.perf_counter_ns()

    def _elapsed_nanoseconds(self) -> int:
        if self._started_at is None:
            return self._elapsed_ns
        return self._elapsed_ns + time.perf_counter_ns() - self._started_at

    def elapsed_milliseconds(self) -> int64:
        return int64(self._elapsed_nanoseconds() // 1_000_000)

    def elapsed_ticks(self) -> int64:
        # invariant: ElapsedTicks and GetTimestamp use the units declared by Frequency.
        return int64(self._elapsed_nanoseconds())

    def elapsed(self) -> TimeSpan:
        return from_ticks(self._elapsed_nanoseconds() // 100)


def start_new() -> StopWatch:
    stopwatch = StopWatch()
    stopwatch.start()
    return stopwatch


def frequency() -> int64:
    return int64(1_000_000_000)


def is_high_resolution() -> bool:
    return True


def get_timestamp() -> int64:
    return int64(time.perf_counter_ns())


def get_elapsed_time(starting_timestamp: int64, ending_timestamp: int64 | None = None) -> TimeSpan:
    end = get_timestamp() if ending_timestamp is None else ending_timestamp
    return from_ticks((end - starting_timestamp) // 100)


__all__ = [
    "StopWatch",
    "frequency",
    "get_elapsed_time",
    "get_timestamp",
    "is_high_resolution",
    "start_new",
]
