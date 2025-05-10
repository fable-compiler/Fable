import math
from typing import Any

from .core import float32, float64
from .types import FSharpRef


def abs(x: float) -> float:
    return -x if x < 0 else x


def sign(x: float) -> float:
    return -1 if x < 0 else 1 if x > 0 else 0


def max(x: float, y: float) -> float:
    return x if x > y else y


def min(x: float, y: float) -> float:
    return x if x < y else y


def log(x: float) -> float:
    if x == 0:
        return float("-inf")
    elif x < 0:
        return float("nan")
    return math.log(x)


def sqrt(x: float64) -> float64:
    return x.sqrt()


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
    "float32",
    "float64",
    "is_negative_inf",
    "is_positive_inf",
    "log",
    "max",
    "min",
    "parse",
    "sign",
    "sqrt",
    "try_parse",
]
