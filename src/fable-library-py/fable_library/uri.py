from __future__ import annotations

from enum import IntEnum
from typing import Union
from urllib.parse import urlparse, urljoin


class UriKind(IntEnum):
    RelativeOrAbsolute = 0
    Absolute = 1
    Relative = 2


class Uri:
    __slots__ = "res", "kind"

    def __init__(self, uri: Union[Uri, str], uri_kind: Union[int, str, Uri, None] = None) -> None:
        uri = str(uri)

        if isinstance(uri_kind, int):
            self.kind = UriKind(uri_kind)
        else:
            self.kind = UriKind.Absolute

        if isinstance(uri_kind, (str, Uri)):
             uri = urljoin(uri, str(uri_kind))

        if self.kind == UriKind.Absolute and ":" not in uri:
            raise ValueError("Invalid URI: The format of the URI could not be determined.")

        self.res = urlparse(uri)

    @property
    def is_absolute_uri(self) -> bool:
        return True if self.res.netloc else False

    @property
    def scheme(self) -> str:
        return self.res.scheme

    @property
    def host(self) -> str:
        return self.res.hostname or ""

    @property
    def absolute_uri(self) -> str:
        if self.kind == UriKind.Relative:
            raise ValueError("This operation is not supported for a relative URI.")

        return self.res.geturl()

    @property
    def absolute_path(self) -> str:
        return self.res.path

    @property
    def query(self) -> str:
        return f"?{self.res.query}"

    @property
    def path_and_query(self) -> str:
        return f"{self.res.path}?{self.res.query}"

    @property
    def fragment(self) -> str:
        return f"#{self.res.fragment}"

    def __str__(self) -> str:
        return self.res.geturl()
