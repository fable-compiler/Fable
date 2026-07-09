import math
from typing import Any, overload

from .core import FSharpRef, float32, float64, floats


def sign(x: float64) -> float64:
    return float64(-1) if x < 0 else float64(1) if x > 0 else float64(0)


@overload
def max(x: float32, y: float32) -> float32: ...
@overload
def max(x: float64, y: float64) -> float64: ...
def max(x: float32 | float64, y: float32 | float64) -> float32 | float64:
    # IEEE 754 maximum (.NET Math.Max since .NET Core 3.0): NaN propagates (preserving
    # whichever operand was NaN), and +0.0 beats -0.0 even though they compare equal.
    if x != x:
        return x
    if y != y:
        return y
    if x != y:
        return x if x > y else y
    return x if not math.copysign(1.0, x) < 0 else y


@overload
def min(x: float32, y: float32) -> float32: ...
@overload
def min(x: float64, y: float64) -> float64: ...
def min(x: float32 | float64, y: float32 | float64) -> float32 | float64:
    # IEEE 754 minimum (.NET Math.Min since .NET Core 3.0): NaN propagates (preserving
    # whichever operand was NaN), and -0.0 beats +0.0 even though they compare equal.
    if x != x:
        return x
    if y != y:
        return y
    if x != y:
        return x if x < y else y
    return x if math.copysign(1.0, x) < 0 else y


@overload
def clamp(value: float32, min: float32, max: float32) -> float32: ...
@overload
def clamp(value: float64, min: float64, max: float64) -> float64: ...
def clamp(value: float32 | float64, min: float32 | float64, max: float32 | float64) -> float32 | float64:
    return min if value < min else max if value > max else value


def try_parse(string: str, def_value: FSharpRef[float32 | float64 | Any]) -> bool:
    try:
        def_value.contents = parse(string)
        return True
    except Exception:
        return False


float32 = floats.Float32
float64 = floats.Float64
acos = floats.acos
asin = floats.asin
atan = floats.atan
atan2 = floats.atan2
ceil = floats.ceil
cos = floats.cos
cosh = floats.cosh
degrees = floats.degrees
exp = floats.exp
floor = floats.floor
is_infinity = floats.is_infinity
is_nan = floats.is_nan
is_negative_inf = floats.is_negative_infinity
is_positive_inf = floats.is_positive_infinity
log = floats.log
log10 = floats.log10
log2 = floats.log2
pow = floats.pow
radians = floats.radians
sin = floats.sin
sinh = floats.sinh
sqrt = floats.sqrt
tan = floats.tan
tanh = floats.tanh
inf = floats.infinity
nan = floats.nan
negative_inf = floats.negative_infinity
parse = floats.parse
parse_single = floats.parse_single
abs = floats.abs


__all__ = [
    "acos",
    "asin",
    "atan",
    "atan2",
    "ceil",
    "cos",
    "cosh",
    "degrees",
    "exp",
    "float32",
    "float64",
    "floor",
    "is_infinity",
    "is_nan",
    "is_negative_inf",
    "is_positive_inf",
    "log",
    "log2",
    "log10",
    "max",
    "min",
    "parse",
    "parse_single",
    "pow",
    "radians",
    "sign",
    "sin",
    "sinh",
    "sqrt",
    "tan",
    "tanh",
    "try_parse",
]
