from .array_ import Array
from .protocols import IEnumerable_1

class FSharpList[T](IEnumerable_1[T]):
    head_: T | None
    tail_: FSharpList[T] | None

def of_array[T](xs: Array[T]) -> FSharpList[T]: ...
def length[T](xs: FSharpList[T]) -> int: ...
