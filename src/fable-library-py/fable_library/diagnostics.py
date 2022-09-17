import time


def frequency() -> int:
    return 1000000000


def get_timestamp() -> int:
    return time.perf_counter_ns()
