"""Stub file for the array module.

This is only needed so that the static type checker can find the types for the extension
methods we have written in Rust. The file will never be used by Python at runtime.
"""

from __future__ import annotations

from collections.abc import Callable, Iterator
from typing import Any, TypeVar

class FSharpArray:
    def __init__(self, elements: list[Any] | None = None, array_type: str | None = None) -> None: ...
    def __len__(self) -> int: ...
    def __getitem__(self, idx: int) -> Any: ...  # Use Any as return type since FSharpArray is not generic
    def __setitem__(self, idx: int, value: Any) -> None: ...
    def __iter__(self) -> Iterator[Any]: ...  # Use Iterator[Any]
    def map(
        self, f: Callable[[Any], Any], cons: Any | None = None
    ) -> FSharpArray: ...  # Use Any for map function types
    def filter(self, predicate: Callable[[Any], bool]) -> FSharpArray: ...  # Use Any for filter predicate type

class FSharpCons:
    array_type: str
    def __init__(self, array_type: str) -> None: ...
    # Allocate should probably return FSharpArray[Any] or a specific type based on cons?
    # For now, let's keep it simple, but this might need refinement.
    def allocate(self, length: int) -> FSharpArray: ...
    def __call__(self, length: int) -> FSharpArray: ...

# Similar consideration for allocate_array_from_cons
def allocate_array_from_cons(cons: Any | None, length: int) -> FSharpArray: ...
