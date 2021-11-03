import sys


def to_int64(bytes: bytes, offset: int) -> int:
    return int.from_bytes(bytes[offset : offset + 7], byteorder=sys.byteorder, signed=True)


def get_bytes_char(value: str) -> bytes:
    return bytearray(bytes(value, "UTF-8"))


def get_bytes_int16(value: int) -> bytes:
    return bytearray(value.to_bytes(length=2, byteorder=sys.byteorder))


def get_bytes_uint16(value: int) -> bytes:
    return bytearray(value.to_bytes(length=2, byteorder=sys.byteorder))


def get_bytes_int32(value: int) -> bytes:
    return bytearray(value.to_bytes(length=4, byteorder=sys.byteorder))


def get_bytes_int64(value: int) -> bytes:
    return bytearray(value.to_bytes(length=8, byteorder=sys.byteorder))


def get_bytes_boolean(value: bool) -> bytes:
    return bytearray(value.to_bytes(length=1, byteorder=sys.byteorder))


def is_little_endian() -> bool:
    return sys.byteorder == "little"
