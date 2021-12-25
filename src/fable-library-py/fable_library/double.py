from typing import Any

from .types import FSharpRef


def parse(value: Any) -> float:
    try:
        return float(value)
    except Exception:
        raise ValueError("Input string was not in a correct format.")


def try_parse(string: str, defValue: FSharpRef[float]) -> bool:
    try:
        defValue.contents = parse(string)
        return True
    except Exception:
        return False
