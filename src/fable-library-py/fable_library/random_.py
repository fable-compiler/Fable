"""System.Random methods that check their arguments like .NET (util.py cannot import the exception types)."""

from __future__ import annotations

import random

from .system import ArgumentOutOfRangeException__ctor_Z384F8060
from .util import random_int


def next_max(rand: random.Random, max_value: int) -> int:
    """Random.Next(maxValue)."""
    if max_value < 0:
        raise ArgumentOutOfRangeException__ctor_Z384F8060(
            "maxValue", f"maxValue ('{max_value}') must be a non-negative value."
        )
    return random_int(rand, 0, max_value)


def next_range(rand: random.Random, min_value: int, max_value: int) -> int:
    """Random.Next(minValue, maxValue)."""
    if min_value > max_value:
        raise ArgumentOutOfRangeException__ctor_Z384F8060("minValue", "'minValue' cannot be greater than maxValue.")
    return random_int(rand, min_value, max_value)


__all__ = [
    "next_max",
    "next_range",
]
