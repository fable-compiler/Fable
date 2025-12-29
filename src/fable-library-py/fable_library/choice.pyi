from __future__ import annotations

from collections.abc import Callable
from typing import Any, Generic, TypeVar, overload

from .reflection import TypeInfo
from .types import Union

_T1 = TypeVar("_T1")
_T2 = TypeVar("_T2")
_T3 = TypeVar("_T3")
_T4 = TypeVar("_T4")
_T5 = TypeVar("_T5")
_T6 = TypeVar("_T6")
_T7 = TypeVar("_T7")

class FSharpChoice_2(Union, Generic[_T1, _T2]):
    def __init__(self, tag: int, *fields: Any) -> None: ...
    @staticmethod
    def cases() -> list[str]: ...

FSharpChoice_2_reflection: Callable[[TypeInfo, TypeInfo], TypeInfo]

class FSharpChoice_3(Union, Generic[_T1, _T2, _T3]):
    def __init__(self, tag: int, *fields: Any) -> None: ...
    @staticmethod
    def cases() -> list[str]: ...

FSharpChoice_3_reflection: Callable[[TypeInfo, TypeInfo, TypeInfo], TypeInfo]

class FSharpChoice_4(Union, Generic[_T1, _T2, _T3, _T4]):
    def __init__(self, tag: int, *fields: Any) -> None: ...
    @staticmethod
    def cases() -> list[str]: ...

FSharpChoice_4_reflection: Callable[[TypeInfo, TypeInfo, TypeInfo, TypeInfo], TypeInfo]

class FSharpChoice_5(Union, Generic[_T1, _T2, _T3, _T4, _T5]):
    def __init__(self, tag: int, *fields: Any) -> None: ...
    @staticmethod
    def cases() -> list[str]: ...

FSharpChoice_5_reflection: Callable[[TypeInfo, TypeInfo, TypeInfo, TypeInfo, TypeInfo], TypeInfo]

class FSharpChoice_6(Union, Generic[_T1, _T2, _T3, _T4, _T5, _T6]):
    def __init__(self, tag: int, *fields: Any) -> None: ...
    @staticmethod
    def cases() -> list[str]: ...

FSharpChoice_6_reflection: Callable[[TypeInfo, TypeInfo, TypeInfo, TypeInfo, TypeInfo, TypeInfo], TypeInfo]

class FSharpChoice_7(Union, Generic[_T1, _T2, _T3, _T4, _T5, _T6, _T7]):
    def __init__(self, tag: int, *fields: Any) -> None: ...
    @staticmethod
    def cases() -> list[str]: ...

FSharpChoice_7_reflection: Callable[[TypeInfo, TypeInfo, TypeInfo, TypeInfo, TypeInfo, TypeInfo, TypeInfo], TypeInfo]

@overload
def Choice_makeChoice1Of2() -> FSharpChoice_2[None, Any]: ...
@overload
def Choice_makeChoice1Of2(x: _T1) -> FSharpChoice_2[_T1, Any]: ...
@overload
def Choice_makeChoice2Of2() -> FSharpChoice_2[Any, None]: ...
@overload
def Choice_makeChoice2Of2(x: _T2) -> FSharpChoice_2[Any, _T2]: ...
def Choice_tryValueIfChoice1Of2(x: FSharpChoice_2[_T1, Any]) -> _T1 | None: ...
def Choice_tryValueIfChoice2Of2(x: FSharpChoice_2[Any, _T2]) -> _T2 | None: ...

__all__ = [
    "Choice_makeChoice1Of2",
    "Choice_makeChoice2Of2",
    "Choice_tryValueIfChoice1Of2",
    "Choice_tryValueIfChoice2Of2",
    "FSharpChoice_2_reflection",
    "FSharpChoice_3_reflection",
    "FSharpChoice_4_reflection",
    "FSharpChoice_5_reflection",
    "FSharpChoice_6_reflection",
    "FSharpChoice_7_reflection",
]
