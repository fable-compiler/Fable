from .core import float64, floats
from .types import FSharpRef


def sign(x: float64) -> float64:
    return float64(-1) if x < 0 else float64(1) if x > 0 else float64(0)


def max(x: float64, y: float64) -> float64:
    return x if x > y else y


def min(x: float64, y: float64) -> float64:
    return x if x < y else y


def try_parse(string: str, def_value: FSharpRef[float64]) -> bool:
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
