import sys


def to_int64(bytes: bytes, offset: int) -> int:
    return int.from_bytes(bytes[offset : offset + 7], byteorder=sys.byteorder, signed=True)


def get_bytes_int32(value: int):
    return value.to_bytes(length=4, byteorder=sys.byteorder)
