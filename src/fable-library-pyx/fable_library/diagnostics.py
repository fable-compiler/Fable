import time
from typing import Optional


class TimerError(Exception):
    """A custom exception used to report errors in use of Timer class"""


class StopWatch:
    def __init__(self) -> None:
        self._start_time: Optional[float] = None
        self._elapsed_time: Optional[float] = None

    def is_running(self) -> bool:
        """Return True if timer is running"""
        return self._start_time is not None

    def start(self) -> None:
        """Start a new timer"""
        if self._start_time is not None:
            raise TimerError(f"Timer is running. Use .stop() to stop it")

        self._start_time = time.perf_counter()

    def stop(self) -> None:
        """Stop the timer, and report the elapsed time"""
        if self._start_time is None:
            raise TimerError(f"Timer is not running. Use .start() to start it")

        self._elapsed_time = time.perf_counter() - self._start_time
        self._start_time = None

    def elapsed_milliseconds(self) -> int:
        """Return the elapsed time in milliseconds"""
        if self._elapsed_time is None:
            raise TimerError(f"Timer is not running. Use .start() to start it")

        return int(self._elapsed_time * 1000)

    def elapsed_ticks(self) -> int:
        """Return the elapsed time in ticks"""
        if self._elapsed_time is None:
            raise TimerError(f"Timer is not running. Use .start() to start it")

        return int(self._elapsed_time * 10000000)

def frequency() -> int:
    return 1000000000


def get_timestamp() -> int:
    return time.perf_counter_ns()
