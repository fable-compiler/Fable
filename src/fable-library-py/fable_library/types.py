"""Core type definitions for Fable Python runtime.

This module contains fundamental type definitions used throughout the
Fable runtime library.
"""

from __future__ import annotations

from typing import Any

from .core import (
    FSharpRef,
    byte,
    float32,
    float64,
    int8,
    int16,
    int32,
    int64,
    sbyte,
    uint8,
    uint16,
    uint32,
    uint64,
)


# Unit type for F# unit-typed parameters. Using None mirrors F#'s
# unit type which is essentially a 0-tuple with a single value ().
# This provides a concrete type for strict type checking instead of Any.
type Unit = None

# UNIT is typed as Any so it can be used as a default value for generic type
# parameters like `def foo[T](x: T = UNIT)`. None would not be
# assignable to T, but Any is compatible with all types.
UNIT: Any = None


class Attribute:
    """Base class for F# attributes."""

    ...


# We don't use type aliases here because we need to do isinstance checks
IntegerTypes = int | byte | sbyte | int16 | uint16 | int32 | uint32 | int64 | uint64
FloatTypes = float | float32 | float64


__all__ = [
    "UNIT",
    "Attribute",
    "FSharpRef",
    "FloatTypes",
    "IntegerTypes",
    "Unit",
    "byte",
    "float32",
    "float64",
    "int8",
    "int16",
    "int32",
    "int64",
    "sbyte",
    "uint8",
    "uint16",
    "uint32",
    "uint64",
]
