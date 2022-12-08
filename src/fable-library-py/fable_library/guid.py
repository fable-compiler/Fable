import uuid

from .types import FSharpRef


def parse(string: str) -> uuid.UUID:
    return uuid.UUID(string.strip("()"))


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


def guid_to_array(guid: uuid.UUID) -> bytearray:
    return bytearray(guid.bytes_le)


def array_to_guid(guid: bytearray) -> uuid.UUID:
    return uuid.UUID(bytes_le=bytes(guid))


__all__ = [
    "parse",
    "try_parse",
    "to_string",
    "new_guid",
    "guid_to_array",
    "array_to_guid",
]
