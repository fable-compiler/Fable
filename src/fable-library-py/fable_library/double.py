from typing import Any

from .core import floats
from .types import FSharpRef


def abs(x: float) -> float:
    return -x if x < 0 else x


def sign(x: float) -> float:
    return -1 if x < 0 else 1 if x > 0 else 0


def max(x: float, y: float) -> float:
    return x if x > y else y


def min(x: float, y: float) -> float:
    return x if x < y else y


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


__all__ = [
    "abs",
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
