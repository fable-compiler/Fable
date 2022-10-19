from typing import Optional


class UTF16LE:
    def get_bytes(
        self, string: str, index: Optional[int] = None, count: Optional[int] = None
    ) -> bytes:
        return string.encode("utf-16le")

    def get_string(
        self, bytes: bytes, index: Optional[int] = None, count: Optional[int] = None
    ) -> str:
        if index is None:
            return bytes.decode("utf-8")
        else:
            return bytes.decode("utf-8")[index:count]


class UTF8:
    def get_bytes(
        self, string: str, index: Optional[int] = None, count: Optional[int] = None
    ) -> bytes:
        if index is None:
            return string.encode("utf-8")
        else:
            return string.encode("utf-8")[index:count]

    def get_string(
        self, bytes: bytes, index: Optional[int] = None, count: Optional[int] = None
    ) -> str:
        if index is None:
            return bytes.decode("utf-8")
        else:
            return bytes.decode("utf-8")[index:count]


_UTF8 = UTF8()
_UTF16 = UTF16LE()


def get_unicode():
    return _UTF16


def get_utf8() -> UTF8:
    return _UTF8


__all__ = ["UTF8", "UTF16LE", "get_utf8", "get_unicode"]
