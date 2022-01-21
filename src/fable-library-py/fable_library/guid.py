import uuid

from .types import FSharpRef


def parse(string: str) -> uuid.UUID:
    return uuid.UUID(string)


def try_parse(string: str, def_value: FSharpRef[uuid.UUID]) -> bool:
    try:
        def_value.contents = parse(string)
        return True
    except Exception:
        return False


def to_string(guid: uuid.UUID) -> str:
    return str(guid)


def new_guid() -> uuid.UUID:
    return uuid.uuid4()
