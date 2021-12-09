from enum import Enum
from urllib.parse import urlparse


class UriKind(Enum):
    RelativeOrAbsolute = 0
    Absolute = 1
    Relative = 2


class Uri:
    def __init__(self, uri: str, uri_kind=None) -> None:
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
