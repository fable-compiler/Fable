from collections.abc import Callable
from typing import Any, overload

from .reflection import TypeInfo
from .union import Union

class FSharpChoice_2[T1, T2](Union):
    def __init__(self, tag: int, *fields: Any) -> None: ...
    @staticmethod
    def cases() -> list[str]: ...

FSharpChoice_2_reflection: Callable[[TypeInfo, TypeInfo], TypeInfo]

class FSharpChoice_3[T1, T2, T3](Union):
    def __init__(self, tag: int, *fields: Any) -> None: ...
    @staticmethod
    def cases() -> list[str]: ...

FSharpChoice_3_reflection: Callable[[TypeInfo, TypeInfo, TypeInfo], TypeInfo]

class FSharpChoice_4[T1, T2, T3, T4](Union):
    def __init__(self, tag: int, *fields: Any) -> None: ...
    @staticmethod
    def cases() -> list[str]: ...

FSharpChoice_4_reflection: Callable[[TypeInfo, TypeInfo, TypeInfo, TypeInfo], TypeInfo]

class FSharpChoice_5[T1, T2, T3, T4, T5](Union):
    def __init__(self, tag: int, *fields: Any) -> None: ...
    @staticmethod
    def cases() -> list[str]: ...

FSharpChoice_5_reflection: Callable[[TypeInfo, TypeInfo, TypeInfo, TypeInfo, TypeInfo], TypeInfo]

class FSharpChoice_6[T1, T2, T3, T4, T5, T6](Union):
    def __init__(self, tag: int, *fields: Any) -> None: ...
    @staticmethod
    def cases() -> list[str]: ...

FSharpChoice_6_reflection: Callable[[TypeInfo, TypeInfo, TypeInfo, TypeInfo, TypeInfo, TypeInfo], TypeInfo]

class FSharpChoice_7[T1, T2, T3, T4, T5, T6, T7](Union):
    def __init__(self, tag: int, *fields: Any) -> None: ...
    @staticmethod
    def cases() -> list[str]: ...

FSharpChoice_7_reflection: Callable[[TypeInfo, TypeInfo, TypeInfo, TypeInfo, TypeInfo, TypeInfo, TypeInfo], TypeInfo]

@overload
def Choice_makeChoice1Of2() -> FSharpChoice_2[None, Any]: ...
@overload
def Choice_makeChoice1Of2[T1](x: T1) -> FSharpChoice_2[T1, Any]: ...
@overload
def Choice_makeChoice2Of2() -> FSharpChoice_2[Any, None]: ...
@overload
def Choice_makeChoice2Of2[T2](x: T2) -> FSharpChoice_2[Any, T2]: ...
def Choice_tryValueIfChoice1Of2[T1](x: FSharpChoice_2[T1, Any]) -> T1 | None: ...
def Choice_tryValueIfChoice2Of2[T2](x: FSharpChoice_2[Any, T2]) -> T2 | None: ...

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
