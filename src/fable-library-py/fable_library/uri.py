from __future__ import annotations

from enum import IntEnum
from typing import Union
from urllib.parse import ParseResult, urlparse, urljoin, unquote


from .types import FSharpRef


class UriKind(IntEnum):
    RelativeOrAbsolute = 0
    Absolute = 1
    Relative = 2


class Uri:
    __slots__ = "res", "kind", "orginal"

    def __init__(
        self,
        base_uri: Union[Uri, str],
        kind_or_uri: Union[int, str, Uri, None] = UriKind.Absolute,
    ) -> None:
        self.res: ParseResult

        kind = kind_or_uri if isinstance(kind_or_uri, int) else UriKind.Absolute
        uri = urlparse(base_uri) if isinstance(base_uri, str) else base_uri.res
        relative_uri = (
            urlparse(kind_or_uri)
            if isinstance(kind_or_uri, str)
            else kind_or_uri.res
            if isinstance(kind_or_uri, Uri)
            else None
        )

        # Absolute base URI must be absolute
        if kind == UriKind.Absolute and not uri.netloc:
            raise ValueError(
                "Invalid URI: The format of the URI could not be determined."
            )

        # Relative base URI must be relative
        if kind == UriKind.Relative and uri.netloc:
            raise ValueError(
                "Invalid URI: The format of the URI could not be determined."
            )

        if relative_uri:
            self.res = urlparse(urljoin(uri.geturl(), relative_uri.geturl()))
        else:
            self.res = uri
        self.kind = kind
        self.orginal = base_uri

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

    @property
    def original_string(self) -> str:
        return str(self.orginal)

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, Uri):
            return False

        return self.res == other.res

    def __str__(self) -> str:
        res = self.res._replace(netloc=self.res.netloc.replace(":80", ""))
        return unquote(res.geturl())

    @staticmethod
    def create(
        uri: Union[str, Uri], kind_or_uri: Union[UriKind, str, Uri] = UriKind.Absolute
    ) -> Uri:
        return Uri(uri, kind_or_uri)

    @staticmethod
    def try_create(
        uri: Union[str, Uri], kind_or_uri: Union[UriKind, str, Uri], out: FSharpRef[Uri]
    ) -> bool:
        try:
            out.contents = Uri.create(uri, kind_or_uri)
            return True
        except Exception:
            return False
