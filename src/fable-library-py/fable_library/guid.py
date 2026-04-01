import os
import time
import uuid

from .array_ import Array
from .core import FSharpRef, byte


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


def guid_to_array(guid: uuid.UUID) -> Array[byte]:
    return Array[byte](byte(x) for x in guid.bytes_le)


def array_to_guid(guid: Array[byte]) -> uuid.UUID:
    return uuid.UUID(bytes_le=bytes(guid))


def create_version7(timestamp: object | None = None) -> uuid.UUID:
    """Create a UUID v7 per RFC 9562."""
    if timestamp is None:
        ms = int(time.time() * 1000)
    else:
        # DateTimeOffset has getTime() returning ms since epoch
        ms = int(timestamp.getTime())  # type: ignore

    # Build 16 bytes per RFC 9562
    rand_bytes = os.urandom(10)

    # Bytes 0-5: 48-bit big-endian ms timestamp
    ts_bytes = ms.to_bytes(6, byteorder="big")

    # Bytes 6-7: version (0111) + 12 bits rand_a
    rand_a = int.from_bytes(rand_bytes[0:2], "big")
    ver_rand_a = (0x7000 | (rand_a & 0x0FFF)).to_bytes(2, "big")

    # Bytes 8-9: variant (10) + 14 bits rand_b
    rand_b_hi = int.from_bytes(rand_bytes[2:4], "big")
    var_rand_b = (0x8000 | (rand_b_hi & 0x3FFF)).to_bytes(2, "big")

    # Bytes 10-15: 48 bits rand_b continued
    rand_rest = rand_bytes[4:10]

    all_bytes = ts_bytes + ver_rand_a + var_rand_b + rand_rest
    return uuid.UUID(bytes=bytes(all_bytes))


__all__ = [
    "array_to_guid",
    "create_version7",
    "guid_to_array",
    "new_guid",
    "parse",
    "to_string",
    "try_parse",
]
