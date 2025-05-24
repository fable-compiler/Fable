from typing import TypeAlias, TypeVar

from .core import array, float32, float64, int8, int16, int32, int64, uint8, uint16, uint32, uint64


_T = TypeVar("_T")


def Int8Array(lst: list[int8]) -> array.FSharpArray[int8]:
    return array.FSharpArray(array_type="Int8", elements=lst)


def UInt8Array(lst: list[uint8]) -> array.FSharpArray[uint8]:
    return array.FSharpArray(array_type="UInt8", elements=lst)


def Int16Array(lst: list[int16]) -> array.FSharpArray[int16]:
    return array.FSharpArray(array_type="Int16", elements=lst)


def UInt16Array(lst: list[uint16]) -> array.FSharpArray[uint16]:
    return array.FSharpArray(array_type="UInt16", elements=lst)


def Int32Array(lst: list[int32]) -> array.FSharpArray[int32]:
    return array.FSharpArray(array_type="Int32", elements=lst)


def UInt32Array(lst: list[uint32]) -> array.FSharpArray[uint32]:
    return array.FSharpArray(array_type="UInt32", elements=lst)


def Int64Array(lst: list[int64]) -> array.FSharpArray[int64]:
    return array.FSharpArray(array_type="Int64", elements=lst)


def UInt64Array(lst: list[uint64]) -> array.FSharpArray[uint64]:
    return array.FSharpArray(array_type="UInt64", elements=lst)


def Float32Array(lst: list[float32]) -> array.FSharpArray[float32]:
    return array.FSharpArray(array_type="Float32", elements=lst)


def Float64Array(lst: list[float64]) -> array.FSharpArray[float64]:
    return array.FSharpArray(array_type="Float64", elements=lst)


# Import loose functions directly from array module
append = array.append
chunk_by_size = array.chunk_by_size
compare_with = array.compare_with
create = array.create
empty = array.empty
equals_with = array.equals_with
exists = array.exists
exists_offset = array.exists_offset
fill = array.fill
filter = array.filter
fold = array.fold
fold_back = array.fold_back
fold_back2 = array.fold_back2
fold_back_indexed = array.fold_back_indexed
fold_back_indexed2 = array.fold_back_indexed2
fold_indexed = array.fold_indexed
head = array.head
initialize = array.initialize
insert_at = array.insert_at
insert_many_at = array.insert_many_at
item = array.item
iterate = array.iterate
iterate_indexed = array.iterate_indexed
map = array.map
map2 = array.map2
map3 = array.map3
map_fold = array.map_fold
map_fold_back = array.map_fold_back
map_indexed = array.map_indexed
map_indexed2 = array.map_indexed2
map_indexed3 = array.map_indexed3
pairwise = array.pairwise
permute = array.permute
reduce = array.reduce
reduce_back = array.reduce_back
remove_at = array.remove_at
remove_many_at = array.remove_many_at
reverse = array.reverse
scan = array.scan
scan_back = array.scan_back
set_slice = array.set_slice
singleton = array.singleton
sort_in_place = array.sort_in_place
sort_in_place_with = array.sort_in_place_with
split_into = array.split_into
sum = array.sum
tail = array.tail
transpose = array.transpose
try_find = array.try_find
try_find_back = array.try_find_back
try_find_index_back = array.try_find_index_back
try_head = array.try_head
try_item = array.try_item
update_at = array.update_at
windowed = array.windowed
index_of = array.index_of
remove_in_place = array.remove_in_place
copy_to = array.copy_to
zip = array.zip
for_all = array.for_all
find = array.find
find_last_index = array.find_last_index
add_in_place = array.add_in_place
add_range_in_place = array.add_range_in_place
insert_range_in_place = array.insert_range_in_place
get_sub_array = array.get_sub_array
contains = array.contains
max = array.max
min = array.min

Array: TypeAlias = array.FSharpArray[_T]
FSharpCons: TypeAlias = array.FSharpCons[_T]

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
    "append",
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
