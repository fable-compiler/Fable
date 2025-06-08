from collections.abc import Callable
from typing import Generic, TypeVar

_T = TypeVar("_T")

class FSharpRef(Generic[_T]):
    def __init__(
        self, contents_or_getter: _T | Callable[[], _T], setter: Callable[[_T], None] | None = None
    ) -> None: ...
    @property
    def contents(self) -> _T: ...
    @contents.setter
    def contents(self, value: _T) -> None: ...
