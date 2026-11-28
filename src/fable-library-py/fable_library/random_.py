"""System.Random methods that check their arguments like .NET (util.py cannot import the exception types).

Random(seed) is a port of .NET's seeded generator, so a seed draws exactly what it draws on .NET. Random() and
Random.Shared stay Python's own generator, since .NET promises nothing about their values.
"""

from __future__ import annotations

import random

from .array_ import Array
from .core import float64, int32, uint8
from .system import ArgumentOutOfRangeException__ctor_Z384F8060
from .util import random_bytes, random_double, random_int


_MBIG = 2147483647  # Int32.MaxValue
_MSEED = 161803398


def _wrap_int32(value: int) -> int:
    """Value as .NET's unchecked Int32 arithmetic leaves it."""
    return (value + 0x80000000) % 0x100000000 - 0x80000000


def _seed_array(seed: int) -> list[int]:
    """The state .NET's seeded generator starts from (Net5CompatSeedImpl)."""
    subtraction = _MBIG if seed == -0x80000000 else abs(seed)
    mj = _MSEED - subtraction
    seed_array = [0] * 56
    seed_array[55] = mj
    mk = 1
    for i in range(1, 55):
        ii = (21 * i) % 55
        seed_array[ii] = mk
        mk = mj - mk
        if mk < 0:
            mk += _MBIG
        mj = seed_array[ii]

    for _ in range(4):
        for i in range(1, 56):
            # A seed above MSEED leaves seed_array[55] negative, and .NET lets this subtraction overflow
            value = _wrap_int32(seed_array[i] - seed_array[1 + (i + 30) % 55])
            seed_array[i] = value + _MBIG if value < 0 else value

    return seed_array


class Seeded:
    """Random(seed): .NET's seeded generator, which draws what the same seed draws on .NET."""

    __slots__ = ("_inext", "_inextp", "_seed_array")

    def __init__(self, seed: int) -> None:
        self._seed_array = _seed_array(seed)
        self._inext = 0
        self._inextp = 21

    def internal_sample(self) -> int:
        """The next value in [0, Int32.MaxValue)."""
        inext = self._inext + 1 if self._inext < 55 else 1
        inextp = self._inextp + 1 if self._inextp < 55 else 1
        value = self._seed_array[inext] - self._seed_array[inextp]
        if value == _MBIG:
            value -= 1
        if value < 0:
            value += _MBIG

        self._seed_array[inext] = value
        self._inext = inext
        self._inextp = inextp
        return value

    def sample(self) -> float:
        """The next value in [0.0, 1.0)."""
        return self.internal_sample() * (1.0 / _MBIG)

    def random(self) -> float:
        """NextDouble() by the name random.Random gives it, which the Rust Array.random*With functions call."""
        return self.sample()

    def large_range_sample(self) -> float:
        """The next value in [0.0, 1.0) for a range wider than Int32.MaxValue, from two samples."""
        value = self.internal_sample()
        if self.internal_sample() % 2 == 0:
            value = -value

        return (value + 2147483646.0) / 4294967293.0


def seeded(seed: int) -> Seeded:
    """Random(seed)."""
    return Seeded(int(seed))


def next_non_negative(rand: random.Random | Seeded) -> int:
    """Random.Next()."""
    if isinstance(rand, Seeded):
        return int32(rand.internal_sample())

    return random_int(rand, 0, _MBIG)


def next_max(rand: random.Random | Seeded, max_value: int) -> int:
    """Random.Next(maxValue)."""
    if max_value < 0:
        raise ArgumentOutOfRangeException__ctor_Z384F8060(
            "maxValue", f"maxValue ('{max_value}') must be a non-negative value."
        )
    if isinstance(rand, Seeded):
        return int32(int(rand.sample() * int(max_value)))

    return random_int(rand, 0, max_value)


def next_range(rand: random.Random | Seeded, min_value: int, max_value: int) -> int:
    """Random.Next(minValue, maxValue)."""
    if min_value > max_value:
        raise ArgumentOutOfRangeException__ctor_Z384F8060("minValue", "'minValue' cannot be greater than maxValue.")
    if isinstance(rand, Seeded):
        return int32(_seeded_range(rand, int(min_value), int(max_value)))

    return random_int(rand, min_value, max_value)


def _seeded_range(rand: Seeded, min_value: int, max_value: int) -> int:
    span = max_value - min_value
    if span <= _MBIG:
        return int(rand.sample() * span) + min_value

    return int(rand.large_range_sample() * span) + min_value


def next_double(rand: random.Random | Seeded) -> float:
    """Random.NextDouble()."""
    if isinstance(rand, Seeded):
        return float64(rand.sample())

    return random_double(rand)


def next_bytes(rand: random.Random | Seeded, buffer: Array[uint8]) -> None:
    """Random.NextBytes(buffer)."""
    if not isinstance(rand, Seeded):
        random_bytes(rand, buffer)
        return

    for i in range(len(buffer)):
        buffer[i] = uint8(rand.internal_sample() & 0xFF)


__all__ = [
    "Seeded",
    "next_bytes",
    "next_double",
    "next_max",
    "next_non_negative",
    "next_range",
    "seeded",
]
