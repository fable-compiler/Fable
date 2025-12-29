import struct
import sys
from typing import SupportsFloat, SupportsInt

from .types import float32, float64, int16, int32, int64, uint16, uint32, uint64


def get_bytes_char(value: str) -> bytes:
    return bytes(value, "UTF-8")


def get_bytes_int16(value: SupportsInt) -> bytes:
    return int(value).to_bytes(length=2, byteorder=sys.byteorder, signed=True)


def get_bytes_uint16(value: SupportsInt) -> bytes:
    return int(value).to_bytes(length=2, byteorder=sys.byteorder)


def get_bytes_int32(value: SupportsInt) -> bytes:
    return int(value).to_bytes(length=4, byteorder=sys.byteorder, signed=True)


def get_bytes_uint32(value: SupportsInt) -> bytes:
    return int(value).to_bytes(length=4, byteorder=sys.byteorder)


def get_bytes_int64(value: SupportsInt) -> bytes:
    return int(value).to_bytes(length=8, byteorder=sys.byteorder, signed=True)


def get_bytes_uint64(value: SupportsInt) -> bytes:
    return int(value).to_bytes(length=8, byteorder=sys.byteorder)


def get_bytes_boolean(value: bool) -> bytes:
    return value.to_bytes(length=1, byteorder=sys.byteorder)


def get_bytes_single(value: SupportsFloat) -> bytes:
    return struct.pack("f", float(value))


def get_bytes_double(value: SupportsFloat) -> bytes:
    return struct.pack("d", float(value))


def int64bits_to_double(value: SupportsInt) -> float64:
    byte_data = int(value).to_bytes(length=8, byteorder=sys.byteorder, signed=True)
    [number] = struct.unpack("d", byte_data)
    return float64(number)


def double_to_int64bits(value: SupportsFloat) -> int64:
    byte_data = bytearray(struct.pack("d", float(value)))
    [number] = struct.unpack("q", byte_data)
    return int64(number)


def to_boolean(byte_data: bytes | bytearray, offset: SupportsInt) -> bool:
    off = int(offset)
    return bool(bool.from_bytes(byte_data[off : off + 1], byteorder=sys.byteorder))


def to_char(byte_data: bytes | bytearray, offset: SupportsInt) -> str:
    off = int(offset)
    return byte_data[off : off + 1].decode("utf8")


def to_int16(byte_data: bytes | bytearray, offset: SupportsInt) -> int16:
    off = int(offset)
    return int16(int.from_bytes(byte_data[off : off + 2], byteorder=sys.byteorder, signed=True))


def to_uint16(byte_data: bytes | bytearray, offset: SupportsInt) -> uint16:
    off = int(offset)
    return uint16(int.from_bytes(byte_data[off : off + 2], byteorder=sys.byteorder, signed=False))


def to_int32(byte_data: bytes | bytearray, offset: SupportsInt) -> int32:
    off = int(offset)
    return int32(int.from_bytes(byte_data[off : off + 4], byteorder=sys.byteorder, signed=True))


def to_uint32(byte_data: bytes | bytearray, offset: SupportsInt) -> uint32:
    off = int(offset)
    return uint32(int.from_bytes(byte_data[off : off + 4], byteorder=sys.byteorder, signed=False))


def to_int64(byte_data: bytes | bytearray, offset: SupportsInt) -> int64:
    off = int(offset)
    return int64(int.from_bytes(byte_data[off : off + 8], byteorder=sys.byteorder, signed=True))


def to_uint64(byte_data: bytes | bytearray, offset: SupportsInt) -> uint64:
    off = int(offset)
    return uint64(int.from_bytes(byte_data[off : off + 8], byteorder=sys.byteorder, signed=False))


def to_single(byte_data: bytes | bytearray, offset: SupportsInt) -> float32:
    off = int(offset)
    [number] = struct.unpack("f", byte_data[off : off + 4])
    return float32(number)


def to_double(byte_data: bytes | bytearray, offset: SupportsInt) -> float64:
    off = int(offset)
    [number] = struct.unpack("d", byte_data[off : off + 8])
    return float64(number)


def to_string(byte_data: bytes | bytearray, offset: SupportsInt = 0, count: SupportsInt | None = None) -> str:
    off = int(offset)
    cnt = len(byte_data) if count is None else int(count)
    return "-".join([f"{x:02x}" for x in byte_data[off : off + cnt]])


def is_little_endian() -> bool:
    return sys.byteorder == "little"


__all__ = [
    "double_to_int64bits",
    "get_bytes_boolean",
    "get_bytes_char",
    "get_bytes_double",
    "get_bytes_int16",
    "get_bytes_int32",
    "get_bytes_int64",
    "get_bytes_single",
    "get_bytes_uint16",
    "get_bytes_uint32",
    "get_bytes_uint64",
    "int64bits_to_double",
    "is_little_endian",
    "to_boolean",
    "to_char",
    "to_double",
    "to_int16",
    "to_int32",
    "to_int64",
    "to_single",
    "to_string",
    "to_uint16",
    "to_uint32",
    "to_uint64",
]
