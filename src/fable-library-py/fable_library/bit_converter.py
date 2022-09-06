import struct
import sys

from typing import Optional


def get_bytes_char(value: str) -> bytes:
    return bytearray(bytes(value, "UTF-8"))


def get_bytes_int16(value: int) -> bytes:
    return bytearray(value.to_bytes(length=2, byteorder=sys.byteorder))


def get_bytes_uint16(value: int) -> bytes:
    return bytearray(value.to_bytes(length=2, byteorder=sys.byteorder))


def get_bytes_int32(value: int) -> bytes:
    return bytearray(value.to_bytes(length=4, byteorder=sys.byteorder))


def get_bytes_uint32(value: int) -> bytes:
    return bytearray(value.to_bytes(length=4, byteorder=sys.byteorder))


def get_bytes_int64(value: int) -> bytes:
    return bytearray(value.to_bytes(length=8, byteorder=sys.byteorder))


def get_bytes_uint64(value: int) -> bytes:
    return bytearray(value.to_bytes(length=8, byteorder=sys.byteorder))


def get_bytes_boolean(value: bool) -> bytes:
    return bytearray(value.to_bytes(length=1, byteorder=sys.byteorder))


def get_bytes_single(value: float) -> bytes:
    return bytearray(struct.pack("f", value))


def get_bytes_double(value: float) -> bytes:
    return bytearray(struct.pack("d", value))


def int64bits_to_double(value: int) -> float:
    bytes = value.to_bytes(length=8, byteorder=sys.byteorder)
    [number] = struct.unpack("d", bytes)
    return number


def double_to_int64bits(value: float) -> int:
    bytes = bytearray(struct.pack("d", value))
    [number] = struct.unpack("q", bytes)
    return number


def to_boolean(bytes: bytearray, offset: int) -> bool:
    return bool(bool.from_bytes(bytes[offset : offset + 1], byteorder=sys.byteorder))


def to_char(bytes: bytearray, offset: int) -> str:
    return bytes[offset : offset + 1].decode("utf8")


def to_int16(bytes: bytearray, offset: int) -> int:
    return int.from_bytes(
        bytes[offset : offset + 2], byteorder=sys.byteorder, signed=True
    )


def to_uint16(bytes: bytearray, offset: int) -> int:
    return int.from_bytes(
        bytes[offset : offset + 2], byteorder=sys.byteorder, signed=False
    )


def to_int32(bytes: bytearray, offset: int) -> int:
    return int.from_bytes(
        bytes[offset : offset + 4], byteorder=sys.byteorder, signed=True
    )


def to_uint32(bytes: bytearray, offset: int) -> int:
    return int.from_bytes(
        bytes[offset : offset + 4], byteorder=sys.byteorder, signed=False
    )


def to_int64(bytes: bytes, offset: int) -> int:
    return int.from_bytes(
        bytes[offset : offset + 8], byteorder=sys.byteorder, signed=True
    )


def to_uint64(bytes: bytearray, offset: int) -> int:
    return int.from_bytes(
        bytes[offset : offset + 8], byteorder=sys.byteorder, signed=False
    )


def to_single(bytes: bytearray, offset: int) -> float:
    [number] = struct.unpack("f", bytes[offset : offset + 4])
    return number


def to_double(bytes: bytearray, offset: int) -> float:
    [number] = struct.unpack("d", bytes[offset : offset + 8])
    return number


def to_string(bytes: bytearray, offset: int = 0, count: Optional[int] = None) -> str:
    count = len(bytes) if count is None else count
    return "-".join([f"{x:02x}" for x in bytes[offset : offset + count]])


def is_little_endian() -> bool:
    return sys.byteorder == "little"


__all__ = [
    "get_bytes_char",
    "get_bytes_int16",
    "get_bytes_uint16",
    "get_bytes_int32",
    "get_bytes_uint32",
    "get_bytes_int64",
    "get_bytes_uint64",
    "get_bytes_boolean",
    "get_bytes_single",
    "get_bytes_double",
    "int64bits_to_double",
    "double_to_int64bits",
    "to_boolean",
    "to_char",
    "to_int16",
    "to_uint16",
    "to_int32",
    "to_uint64",
    "to_single",
    "to_double",
    "to_string",
    "is_little_endian",
]
