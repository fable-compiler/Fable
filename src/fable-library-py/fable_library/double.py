from typing import Any

from .types import FSharpRef


def parse(value: Any) -> float:
    try:
        return float(value)
    except Exception:
        raise ValueError("Input string was not in a correct format.")


def try_parse(string: str, def_value: FSharpRef[float]) -> bool:
    try:
        def_value.contents = parse(string)
        return True
    except Exception:
        return False
