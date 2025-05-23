import math
from typing import Any

from .core import float
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


float32 = float.Float32
float64 = float.Float64
acos = float.acos
asin = float.asin
atan = float.atan
atan2 = float.atan2
ceil = float.ceil
cos = float.cos
cosh = float.cosh
degrees = float.degrees
exp = float.exp
floor = float.floor
is_infinity = float.is_infinity
is_nan = float.is_nan
is_negative_inf = float.is_negative_infinity
is_positive_inf = float.is_positive_infinity
log = float.log
log10 = float.log10
log2 = float.log2
pow = float.pow
radians = float.radians
sin = float.sin
sinh = float.sinh
sqrt = float.sqrt
tan = float.tan
tanh = float.tanh


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
    "acos",
    "asin",
    "atan",
    "atan2",
    "ceil",
    "cos",
    "cosh",
    "degrees",
    "exp",
    "floor",
    "is_infinity",
    "is_nan",
    "log10",
    "log2",
    "pow",
    "radians",
    "sin",
    "sinh",
    "tan",
    "tanh",
]
