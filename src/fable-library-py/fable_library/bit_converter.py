import struct
import sys
from typing import SupportsFloat, SupportsInt

from .array_ import Array
from .core import byte, float32, float64, int16, int32, int64, uint8, uint16, uint32, uint64


def _bytes_to_uint8_array(data: bytes) -> Array[uint8]:
    """Convert Python bytes to FSharpArray[uint8]."""
    return Array[uint8](byte(x) for x in data)


def get_bytes_char(value: str) -> Array[uint8]:
    return _bytes_to_uint8_array(bytes(value, "UTF-8"))


def get_bytes_int16(value: SupportsInt) -> Array[uint8]:
    return _bytes_to_uint8_array(int(value).to_bytes(length=2, byteorder=sys.byteorder, signed=True))


def get_bytes_uint16(value: SupportsInt) -> Array[uint8]:
    return _bytes_to_uint8_array(int(value).to_bytes(length=2, byteorder=sys.byteorder))


def get_bytes_int32(value: SupportsInt) -> Array[uint8]:
    return _bytes_to_uint8_array(int(value).to_bytes(length=4, byteorder=sys.byteorder, signed=True))


def get_bytes_uint32(value: SupportsInt) -> Array[uint8]:
    return _bytes_to_uint8_array(int(value).to_bytes(length=4, byteorder=sys.byteorder))


def get_bytes_int64(value: SupportsInt) -> Array[uint8]:
    return _bytes_to_uint8_array(int(value).to_bytes(length=8, byteorder=sys.byteorder, signed=True))


def get_bytes_uint64(value: SupportsInt) -> Array[uint8]:
    return _bytes_to_uint8_array(int(value).to_bytes(length=8, byteorder=sys.byteorder))


def get_bytes_boolean(value: bool) -> Array[uint8]:
    return _bytes_to_uint8_array(value.to_bytes(length=1, byteorder=sys.byteorder))


def get_bytes_single(value: SupportsFloat) -> Array[uint8]:
    return _bytes_to_uint8_array(struct.pack("f", float(value)))


def get_bytes_double(value: SupportsFloat) -> Array[uint8]:
    return _bytes_to_uint8_array(struct.pack("d", float(value)))


def int64bits_to_double(value: SupportsInt) -> float64:
    byte_data = int(value).to_bytes(length=8, byteorder=sys.byteorder, signed=True)
    [number] = struct.unpack("d", byte_data)
    return float64(number)


def double_to_int64bits(value: SupportsFloat) -> int64:
    byte_data = bytearray(struct.pack("d", float(value)))
    [number] = struct.unpack("q", byte_data)
    return int64(number)


def _to_bytes(data: bytes | bytearray | Array[uint8]) -> bytes:
    """Convert input to bytes, handling both Python bytes and FSharpArray."""
    if isinstance(data, bytes | bytearray):
        return bytes(data)
    # FSharpArray - convert to bytes
    return bytes(int(x) for x in data)


def to_boolean(byte_data: bytes | bytearray | Array[uint8], offset: SupportsInt) -> bool:
    data = _to_bytes(byte_data)
    off = int(offset)
    return bool(bool.from_bytes(data[off : off + 1], byteorder=sys.byteorder))


def to_char(byte_data: bytes | bytearray | Array[uint8], offset: SupportsInt) -> str:
    data = _to_bytes(byte_data)
    off = int(offset)
    return data[off : off + 1].decode("utf8")


def to_int16(byte_data: bytes | bytearray | Array[uint8], offset: SupportsInt) -> int16:
    data = _to_bytes(byte_data)
    off = int(offset)
    return int16(int.from_bytes(data[off : off + 2], byteorder=sys.byteorder, signed=True))


def to_uint16(byte_data: bytes | bytearray | Array[uint8], offset: SupportsInt) -> uint16:
    data = _to_bytes(byte_data)
    off = int(offset)
    return uint16(int.from_bytes(data[off : off + 2], byteorder=sys.byteorder, signed=False))


def to_int32(byte_data: bytes | bytearray | Array[uint8], offset: SupportsInt) -> int32:
    data = _to_bytes(byte_data)
    off = int(offset)
    return int32(int.from_bytes(data[off : off + 4], byteorder=sys.byteorder, signed=True))


def to_uint32(byte_data: bytes | bytearray | Array[uint8], offset: SupportsInt) -> uint32:
    data = _to_bytes(byte_data)
    off = int(offset)
    return uint32(int.from_bytes(data[off : off + 4], byteorder=sys.byteorder, signed=False))


def to_int64(byte_data: bytes | bytearray | Array[uint8], offset: SupportsInt) -> int64:
    data = _to_bytes(byte_data)
    off = int(offset)
    return int64(int.from_bytes(data[off : off + 8], byteorder=sys.byteorder, signed=True))


def to_uint64(byte_data: bytes | bytearray | Array[uint8], offset: SupportsInt) -> uint64:
    data = _to_bytes(byte_data)
    off = int(offset)
    return uint64(int.from_bytes(data[off : off + 8], byteorder=sys.byteorder, signed=False))


def to_single(byte_data: bytes | bytearray | Array[uint8], offset: SupportsInt) -> float32:
    data = _to_bytes(byte_data)
    off = int(offset)
    [number] = struct.unpack("f", data[off : off + 4])
    return float32(number)


def to_double(byte_data: bytes | bytearray | Array[uint8], offset: SupportsInt) -> float64:
    data = _to_bytes(byte_data)
    off = int(offset)
    [number] = struct.unpack("d", data[off : off + 8])
    return float64(number)


def to_string(
    byte_data: bytes | bytearray | Array[uint8], offset: SupportsInt = 0, count: SupportsInt | None = None
) -> str:
    data = _to_bytes(byte_data)
    off = int(offset)
    cnt = len(data) if count is None else int(count)
    return "-".join([f"{x:02x}" for x in data[off : off + cnt]])


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
