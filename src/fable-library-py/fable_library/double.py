from .types import FSharpRef


def parse(value):
    return float(value)


def try_parse(string: str, defValue: FSharpRef[float]) -> bool:
    try:
        defValue.contents = parse(string)
        return True
    except Exception:
        return False
