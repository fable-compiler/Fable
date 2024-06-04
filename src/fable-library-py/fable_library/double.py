import math
from math import copysign
from typing import Any

from .types import FSharpRef


def abs(x: float) -> float:
    return -x if x < 0 else x


def sign(x: float) -> float:
    return -1 if x < 0 else 1 if x > 0 else 0


def max(x: float, y: float) -> float:
    return x if x > y else y


def min(x: float, y: float) -> float:
    return x if x < y else y


def divide(x: float, y: float) -> float:
    """Divide two numbers.

    Returns nan if y is 0.
    """
    if y == 0:
        if x == 0:
            return float("nan")
        return float("inf") if copysign(1, x) == copysign(1, y) else float("-inf")

    return x / y


def log(x: float) -> float:
    if x == 0:
        return float("-inf")
    elif x < 0:
        return float("nan")
    return math.log(x)


def sqrt(x: float) -> float:
    try:
        return math.sqrt(x)
    except ValueError:
        return float("nan")


def is_positive_inf(value: float) -> bool:
    return math.isinf(value) and value > 0


def is_negative_inf(value: float) -> bool:
    return math.isinf(value) and value < 0


def parse(value: Any) -> float:
    try:
        return float(value)
    except Exception:
        raise ValueError(f"The input string {value} was not in a correct format.")


def try_parse(string: str, def_value: FSharpRef[float]) -> bool:
    try:
        def_value.contents = parse(string)
        return True
    except Exception:
        return False


__all__ = [
    "abs",
    "sign",
    "max",
    "min",
    "parse",
    "try_parse",
    "divide",
    "log",
    "sqrt",
    "is_negative_inf",
    "is_positive_inf",
]
