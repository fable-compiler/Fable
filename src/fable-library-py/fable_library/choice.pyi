from collections.abc import Callable
from typing import Any

from .option import Option
from .reflection import TypeInfo
from .union import Union

# FSharpChoice_2
class FSharpChoice_2[T1, T2](Union):
    @staticmethod
    def cases() -> list[str]: ...

class Choice1Of2[T1, T2](FSharpChoice_2[T1, T2]):
    item: T1
    def __init__(self, item: T1) -> None: ...

class Choice2Of2[T1, T2](FSharpChoice_2[T1, T2]):
    item: T2
    def __init__(self, item: T2) -> None: ...

type FSharpChoice_2_[T1, T2] = Choice1Of2[T1, T2] | Choice2Of2[T1, T2]

FSharpChoice_2_reflection: Callable[[TypeInfo, TypeInfo], TypeInfo]

# FSharpChoice_3
class FSharpChoice_3[T1, T2, T3](Union):
    @staticmethod
    def cases() -> list[str]: ...

class Choice1Of3[T1, T2, T3](FSharpChoice_3[T1, T2, T3]):
    item: T1
    def __init__(self, item: T1) -> None: ...

class Choice2Of3[T1, T2, T3](FSharpChoice_3[T1, T2, T3]):
    item: T2
    def __init__(self, item: T2) -> None: ...

class Choice3Of3[T1, T2, T3](FSharpChoice_3[T1, T2, T3]):
    item: T3
    def __init__(self, item: T3) -> None: ...

type FSharpChoice_3_[T1, T2, T3] = Choice1Of3[T1, T2, T3] | Choice2Of3[T1, T2, T3] | Choice3Of3[T1, T2, T3]

FSharpChoice_3_reflection: Callable[[TypeInfo, TypeInfo, TypeInfo], TypeInfo]

# FSharpChoice_4
class FSharpChoice_4[T1, T2, T3, T4](Union):
    @staticmethod
    def cases() -> list[str]: ...

class Choice1Of4[T1, T2, T3, T4](FSharpChoice_4[T1, T2, T3, T4]):
    item: T1
    def __init__(self, item: T1) -> None: ...

class Choice2Of4[T1, T2, T3, T4](FSharpChoice_4[T1, T2, T3, T4]):
    item: T2
    def __init__(self, item: T2) -> None: ...

class Choice3Of4[T1, T2, T3, T4](FSharpChoice_4[T1, T2, T3, T4]):
    item: T3
    def __init__(self, item: T3) -> None: ...

class Choice4Of4[T1, T2, T3, T4](FSharpChoice_4[T1, T2, T3, T4]):
    item: T4
    def __init__(self, item: T4) -> None: ...

type FSharpChoice_4_[T1, T2, T3, T4] = (
    Choice1Of4[T1, T2, T3, T4] | Choice2Of4[T1, T2, T3, T4] | Choice3Of4[T1, T2, T3, T4] | Choice4Of4[T1, T2, T3, T4]
)

FSharpChoice_4_reflection: Callable[[TypeInfo, TypeInfo, TypeInfo, TypeInfo], TypeInfo]

# FSharpChoice_5
class FSharpChoice_5[T1, T2, T3, T4, T5](Union):
    @staticmethod
    def cases() -> list[str]: ...

class Choice1Of5[T1, T2, T3, T4, T5](FSharpChoice_5[T1, T2, T3, T4, T5]):
    item: T1
    def __init__(self, item: T1) -> None: ...

class Choice2Of5[T1, T2, T3, T4, T5](FSharpChoice_5[T1, T2, T3, T4, T5]):
    item: T2
    def __init__(self, item: T2) -> None: ...

class Choice3Of5[T1, T2, T3, T4, T5](FSharpChoice_5[T1, T2, T3, T4, T5]):
    item: T3
    def __init__(self, item: T3) -> None: ...

class Choice4Of5[T1, T2, T3, T4, T5](FSharpChoice_5[T1, T2, T3, T4, T5]):
    item: T4
    def __init__(self, item: T4) -> None: ...

class Choice5Of5[T1, T2, T3, T4, T5](FSharpChoice_5[T1, T2, T3, T4, T5]):
    item: T5
    def __init__(self, item: T5) -> None: ...

type FSharpChoice_5_[T1, T2, T3, T4, T5] = (
    Choice1Of5[T1, T2, T3, T4, T5]
    | Choice2Of5[T1, T2, T3, T4, T5]
    | Choice3Of5[T1, T2, T3, T4, T5]
    | Choice4Of5[T1, T2, T3, T4, T5]
    | Choice5Of5[T1, T2, T3, T4, T5]
)

FSharpChoice_5_reflection: Callable[[TypeInfo, TypeInfo, TypeInfo, TypeInfo, TypeInfo], TypeInfo]

# FSharpChoice_6
class _FSharpChoice_6[T1, T2, T3, T4, T5, T6](Union):
    @staticmethod
    def cases() -> list[str]: ...

class Choice1Of6[T1, T2, T3, T4, T5, T6](_FSharpChoice_6[T1, T2, T3, T4, T5, T6]):
    item: T1
    def __init__(self, item: T1) -> None: ...

class Choice2Of6[T1, T2, T3, T4, T5, T6](_FSharpChoice_6[T1, T2, T3, T4, T5, T6]):
    item: T2
    def __init__(self, item: T2) -> None: ...

class Choice3Of6[T1, T2, T3, T4, T5, T6](_FSharpChoice_6[T1, T2, T3, T4, T5, T6]):
    item: T3
    def __init__(self, item: T3) -> None: ...

class Choice4Of6[T1, T2, T3, T4, T5, T6](_FSharpChoice_6[T1, T2, T3, T4, T5, T6]):
    item: T4
    def __init__(self, item: T4) -> None: ...

class Choice5Of6[T1, T2, T3, T4, T5, T6](_FSharpChoice_6[T1, T2, T3, T4, T5, T6]):
    item: T5
    def __init__(self, item: T5) -> None: ...

class Choice6Of6[T1, T2, T3, T4, T5, T6](_FSharpChoice_6[T1, T2, T3, T4, T5, T6]):
    item: T6
    def __init__(self, item: T6) -> None: ...

type FSharpChoice_6[T1, T2, T3, T4, T5, T6] = (
    Choice1Of6[T1, T2, T3, T4, T5, T6]
    | Choice2Of6[T1, T2, T3, T4, T5, T6]
    | Choice3Of6[T1, T2, T3, T4, T5, T6]
    | Choice4Of6[T1, T2, T3, T4, T5, T6]
    | Choice5Of6[T1, T2, T3, T4, T5, T6]
    | Choice6Of6[T1, T2, T3, T4, T5, T6]
)

FSharpChoice_6_reflection: Callable[[TypeInfo, TypeInfo, TypeInfo, TypeInfo, TypeInfo, TypeInfo], TypeInfo]

# FSharpChoice_7
class _FSharpChoice_7[T1, T2, T3, T4, T5, T6, T7](Union):
    @staticmethod
    def cases() -> list[str]: ...

class Choice1Of7[T1, T2, T3, T4, T5, T6, T7](_FSharpChoice_7[T1, T2, T3, T4, T5, T6, T7]):
    item: T1
    def __init__(self, item: T1) -> None: ...

class Choice2Of7[T1, T2, T3, T4, T5, T6, T7](_FSharpChoice_7[T1, T2, T3, T4, T5, T6, T7]):
    item: T2
    def __init__(self, item: T2) -> None: ...

class Choice3Of7[T1, T2, T3, T4, T5, T6, T7](_FSharpChoice_7[T1, T2, T3, T4, T5, T6, T7]):
    item: T3
    def __init__(self, item: T3) -> None: ...

class Choice4Of7[T1, T2, T3, T4, T5, T6, T7](_FSharpChoice_7[T1, T2, T3, T4, T5, T6, T7]):
    item: T4
    def __init__(self, item: T4) -> None: ...

class Choice5Of7[T1, T2, T3, T4, T5, T6, T7](_FSharpChoice_7[T1, T2, T3, T4, T5, T6, T7]):
    item: T5
    def __init__(self, item: T5) -> None: ...

class Choice6Of7[T1, T2, T3, T4, T5, T6, T7](_FSharpChoice_7[T1, T2, T3, T4, T5, T6, T7]):
    item: T6
    def __init__(self, item: T6) -> None: ...

class Choice7Of7[T1, T2, T3, T4, T5, T6, T7](_FSharpChoice_7[T1, T2, T3, T4, T5, T6, T7]):
    item: T7
    def __init__(self, item: T7) -> None: ...

type FSharpChoice_7[T1, T2, T3, T4, T5, T6, T7] = (
    Choice1Of7[T1, T2, T3, T4, T5, T6, T7]
    | Choice2Of7[T1, T2, T3, T4, T5, T6, T7]
    | Choice3Of7[T1, T2, T3, T4, T5, T6, T7]
    | Choice4Of7[T1, T2, T3, T4, T5, T6, T7]
    | Choice5Of7[T1, T2, T3, T4, T5, T6, T7]
    | Choice6Of7[T1, T2, T3, T4, T5, T6, T7]
    | Choice7Of7[T1, T2, T3, T4, T5, T6, T7]
)

FSharpChoice_7_reflection: Callable[[TypeInfo, TypeInfo, TypeInfo, TypeInfo, TypeInfo, TypeInfo, TypeInfo], TypeInfo]

# Helper functions
def Choice_makeChoice1Of2[T1](x: T1 = ...) -> FSharpChoice_2[T1, Any]: ...
def Choice_makeChoice2Of2[T2](x: T2 = ...) -> FSharpChoice_2[Any, T2]: ...
def Choice_tryValueIfChoice1Of2[T1](x: FSharpChoice_2[T1, Any]) -> Option[T1]: ...
def Choice_tryValueIfChoice2Of2[T2](x: FSharpChoice_2[Any, T2]) -> Option[T2]: ...

__all__ = [
    "Choice1Of2",
    "Choice1Of3",
    "Choice1Of4",
    "Choice1Of5",
    "Choice1Of6",
    "Choice1Of7",
    "Choice2Of2",
    "Choice2Of3",
    "Choice2Of4",
    "Choice2Of5",
    "Choice2Of6",
    "Choice2Of7",
    "Choice3Of3",
    "Choice3Of4",
    "Choice3Of5",
    "Choice3Of6",
    "Choice3Of7",
    "Choice4Of4",
    "Choice4Of5",
    "Choice4Of6",
    "Choice4Of7",
    "Choice5Of5",
    "Choice5Of6",
    "Choice5Of7",
    "Choice6Of6",
    "Choice6Of7",
    "Choice7Of7",
    "Choice_makeChoice1Of2",
    "Choice_makeChoice2Of2",
    "Choice_tryValueIfChoice1Of2",
    "Choice_tryValueIfChoice2Of2",
    "FSharpChoice_2",
    "FSharpChoice_2_",
    "FSharpChoice_2_reflection",
    "FSharpChoice_3",
    "FSharpChoice_3_",
    "FSharpChoice_3_reflection",
    "FSharpChoice_4",
    "FSharpChoice_4_",
    "FSharpChoice_4_reflection",
    "FSharpChoice_5",
    "FSharpChoice_5_",
    "FSharpChoice_5_reflection",
    "FSharpChoice_6",
    "FSharpChoice_6_",
    "FSharpChoice_6_reflection",
    "FSharpChoice_7",
    "FSharpChoice_7_",
    "FSharpChoice_7_reflection",
]
