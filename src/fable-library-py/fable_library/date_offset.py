from datetime import datetime

from .types import FSharpRef


def parse(string: str, detectUTC: bool = False) -> datetime:
    from dateutil import parser

    return parser.parse(string)


def try_parse(string: str, style: int, unsigned: bool, bitsize: int, defValue: FSharpRef[datetime]) -> bool:
    try:
        defValue.contents = parse(string)
        return True
    except Exception:
        return False
