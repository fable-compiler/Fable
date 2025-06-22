import time

from .time_span import TimeSpan, to_milliseconds


def sleep(milliseconds: int | TimeSpan) -> None:
    milliseconds_value = to_milliseconds(milliseconds)
    time.sleep(milliseconds_value / 1000.0)
