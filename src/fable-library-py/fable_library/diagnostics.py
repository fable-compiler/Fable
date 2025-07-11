from __future__ import annotations

import time

from .types import int32, int64


class TimerError(Exception):
    """A custom exception used to report errors in use of Timer class"""


class StopWatch:
    def __init__(self) -> None:
        self._start_time: float | None = None
        self._elapsed_time: float | None = None

    def is_running(self) -> bool:
        """Return True if timer is running"""
        return self._start_time is not None

    def start(self) -> None:
        """Start a new timer"""
        if self._start_time is not None:
            raise TimerError("Timer is running. Use .stop() to stop it")

        self._start_time = time.perf_counter()

    def stop(self) -> None:
        """Stop the timer, and report the elapsed time"""
        if self._start_time is None:
            raise TimerError("Timer is not running. Use .start() to start it")

        self._elapsed_time = time.perf_counter() - self._start_time
        self._start_time = None

    def elapsed_milliseconds(self) -> int32:
        """Return the elapsed time in milliseconds"""
        if self._elapsed_time is None:
            raise TimerError("Timer is not running. Use .start() to start it")

        return int32(self._elapsed_time * 1000)

    def elapsed_ticks(self) -> int32:
        """Return the elapsed time in ticks"""
        if self._elapsed_time is None:
            raise TimerError("Timer is not running. Use .start() to start it")

        return int32(self._elapsed_time * 10000000)


def frequency() -> int32:
    return int32(1000000000)


def get_timestamp() -> int64:
    return int64(time.perf_counter_ns())
