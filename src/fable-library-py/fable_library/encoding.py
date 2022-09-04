from typing import Optional


class UTF8:
    def get_bytes(
        self, string: str, index: Optional[int] = None, count: Optional[int] = None
    ) -> bytes:
        if index is None:
            return string.encode("utf-8")
        else:
            return string.encode("utf-8")[index:count]


_UTF8 = UTF8()


def get_unicode():
    return _UTF16


def get_utf8() -> UTF8:
    return _UTF8
