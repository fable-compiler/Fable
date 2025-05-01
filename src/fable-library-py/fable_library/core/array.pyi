"""Stub file for the array module.

This is only needed so that the static type checker can find the types for the extension
methods we have written in Rust. The file will never be used by Python at runtime.
"""

from __future__ import annotations

from collections.abc import Callable, Iterator
from typing import Any, Literal, overload

from .floats import Float32, Float64
from .ints import Int8, Int16, Int32, Int64, UInt8, UInt16, UInt32, UInt64

ArrayType = Literal[
    "Int8",
    "UInt8",
    "Int16",
    "UInt16",
    "Int32",
    "UInt32",
    "Int64",
    "UInt64",
    "Float32",
    "Float64",
    "String",
    "Generic",
]

class FSharpArray:
    @overload
    def __init__(self, elements: list[UInt8] | None = None, array_type: Literal["UInt8"] | None = None) -> None: ...
    @overload
    def __init__(self, elements: list[Int8] | None = None, array_type: Literal["Int8"] | None = None) -> None: ...
    @overload
    def __init__(self, elements: list[UInt16] | None = None, array_type: Literal["UInt16"] | None = None) -> None: ...
    @overload
    def __init__(self, elements: list[Int16] | None = None, array_type: Literal["Int16"] | None = None) -> None: ...
    @overload
    def __init__(self, elements: list[UInt32] | None = None, array_type: Literal["UInt32"] | None = None) -> None: ...
    @overload
    def __init__(self, elements: list[Int32] | None = None, array_type: Literal["Int32"] | None = None) -> None: ...
    @overload
    def __init__(self, elements: list[UInt64] | None = None, array_type: Literal["UInt64"] | None = None) -> None: ...
    @overload
    def __init__(self, elements: list[Int64] | None = None, array_type: Literal["Int64"] | None = None) -> None: ...
    @overload
    def __init__(self, elements: list[Float32] | None = None, array_type: Literal["Float32"] | None = None) -> None: ...
    @overload
    def __init__(self, elements: list[Float64] | None = None, array_type: Literal["Float64"] | None = None) -> None: ...
    @overload
    def __init__(self, elements: list[str] | None = None, array_type: Literal["String"] | None = None) -> None: ...
    @overload
    def __init__(self, elements: list[object] | None = None, array_type: Literal["Generic"] | None = None) -> None: ...
    def __init__(self, elements: list[Any] | None = None, array_type: ArrayType | None = None) -> None: ...
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
    def __init__(self, array_type: ArrayType) -> None: ...
    # Allocate should probably return FSharpArray[Any] or a specific type based on cons?
    # For now, let's keep it simple, but this might need refinement.
    def allocate(self, length: int) -> FSharpArray: ...
    def __call__(self, length: int) -> FSharpArray: ...

# Similar consideration for allocate_array_from_cons
def allocate_array_from_cons(cons: Any | None, length: int) -> FSharpArray: ...
