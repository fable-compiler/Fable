from collections.abc import MutableSequence
from typing import TypeAlias

from .core import array, float32, float64, int8, int16, int32, int64, uint8, uint16, uint32, uint64


def Int8Array(lst: list[int8]) -> MutableSequence[int8]:
    return array.FSharpArray(array_type="Int8", elements=lst)


def UInt8Array(lst: list[uint8]) -> MutableSequence[uint8]:
    return array.FSharpArray(array_type="UInt8", elements=lst)


def Int16Array(lst: list[int16]) -> MutableSequence[int16]:
    return array.FSharpArray(array_type="Int16", elements=lst)


def UInt16Array(lst: list[uint16]) -> MutableSequence[uint16]:
    return array.FSharpArray(array_type="UInt16", elements=lst)


def Int32Array(lst: list[int32]) -> MutableSequence[int32]:
    return array.FSharpArray(array_type="Int32", elements=lst)


def UInt32Array(lst: list[uint32]) -> MutableSequence[uint32]:
    return array.FSharpArray(array_type="UInt32", elements=lst)


def Int64Array(lst: list[int64]) -> MutableSequence[int64]:
    return array.FSharpArray(array_type="Int64", elements=lst)


def UInt64Array(lst: list[uint64]) -> MutableSequence[uint64]:
    return array.FSharpArray(array_type="UInt64", elements=lst)


def Float32Array(lst: list[float32]) -> MutableSequence[float32]:
    return array.FSharpArray(array_type="Float32", elements=lst)


def Float64Array(lst: list[float64]) -> MutableSequence[float64]:
    return array.FSharpArray(array_type="Float64", elements=lst)


# Import loose functions directly from array module
map = array.map
map_indexed = array.map_indexed
filter = array.filter
chunk_by_size = array.chunk_by_size
fill = array.fill
sort_in_place = array.sort_in_place
sort_in_place_with = array.sort_in_place_with
fold = array.fold
fold_indexed = array.fold_indexed
fold_back = array.fold_back
fold_back_indexed = array.fold_back_indexed
fold_back2 = array.fold_back2
fold_back_indexed2 = array.fold_back_indexed2
reduce = array.reduce
iterate = array.iterate
iterate_indexed = array.iterate_indexed
sum = array.sum
pairwise = array.pairwise
permute = array.permute
scan = array.scan
scan_back = array.scan_back
split_into = array.split_into
transpose = array.transpose
try_find_back = array.try_find_back
try_find_index_back = array.try_find_index_back
windowed = array.windowed
map_fold = array.map_fold
map_fold_back = array.map_fold_back
singleton = array.singleton
head = array.head
try_head = array.try_head
tail = array.tail
item = array.item
try_item = array.try_item
reverse = array.reverse
reduce_back = array.reduce_back
initialize = array.initialize
compare_with = array.compare_with
exists_offset = array.exists_offset
exists = array.exists
update_at = array.update_at
empty = array.empty
equals_with = array.equals_with
set_slice = array.set_slice
insert_at = array.insert_at

Array: TypeAlias = array.FSharpArray

__all__ = [
    "Array",
    "Float32Array",
    "Float64Array",
    "Int8Array",
    "Int16Array",
    "Int32Array",
    "Int64Array",
    "UInt8Array",
    "UInt16Array",
    "UInt32Array",
    "UInt64Array",
    "chunk_by_size",
    "compare_with",
    "empty",
    "equals_with",
    "exists",
    "exists_offset",
    "fill",
    "filter",
    "fold",
    "fold_back",
    "fold_back2",
    "fold_back_indexed",
    "fold_back_indexed2",
    "fold_indexed",
    "head",
    "initialize",
    "item",
    "iterate",
    "iterate_indexed",
    "map",
    "map_fold",
    "map_fold_back",
    "map_indexed",
    "pairwise",
    "permute",
    "reduce",
    "reduce_back",
    "reverse",
    "scan",
    "scan_back",
    "set_slice",
    "singleton",
    "sort_in_place",
    "sort_in_place_with",
    "split_into",
    "sum",
    "tail",
    "transpose",
    "try_find_back",
    "try_find_index_back",
    "try_head",
    "try_item",
    "update_at",
    "windowed",
]
