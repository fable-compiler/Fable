from typing import TypeVar

from .core import array, float32, float64, int8, int16, int32, int64, uint8, uint16, uint32, uint64


_T = TypeVar("_T")

# Define the typed array constructors.
Int8Array = array.FSharpCons[int8]("Int8")
UInt8Array = array.FSharpCons[uint8]("UInt8")
Int16Array = array.FSharpCons[int16]("Int16")
UInt16Array = array.FSharpCons[uint16]("UInt16")
Int32Array = array.FSharpCons[int32]("Int32")
UInt32Array = array.FSharpCons[uint32]("UInt32")
Int64Array = array.FSharpCons[int64]("Int64")
UInt64Array = array.FSharpCons[uint64]("UInt64")
Float32Array = array.FSharpCons[float32]("Float32")
Float64Array = array.FSharpCons[float64]("Float64")

# Import loose functions directly from array module
append = array.append
chunk_by_size = array.chunk_by_size
compare_with = array.compare_with
create = array.create
collect = array.collect
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
last = array.last
map = array.map
map2 = array.map2
map3 = array.map3
map_fold = array.map_fold
map_fold_back = array.map_fold_back
map_indexed = array.map_indexed
map_indexed2 = array.map_indexed2
map_indexed3 = array.map_indexed3
of_seq = array.of_seq
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
sort = array.sort
sort_in_place = array.sort_in_place
sort_in_place_with = array.sort_in_place_with
sort_in_place_by = array.sort_in_place_by
split_into = array.split_into
sum = array.sum
tail = array.tail
transpose = array.transpose
try_find = array.try_find
try_find_back = array.try_find_back
try_find_index_back = array.try_find_index_back
try_find_index = array.try_find_index
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
find_back = array.find_back
find_last_index = array.find_last_index
find_index_back = array.find_index_back
add_in_place = array.add_in_place
add_range_in_place = array.add_range_in_place
insert_range_in_place = array.insert_range_in_place
get_sub_array = array.get_sub_array
contains = array.contains
max = array.max
min = array.min
max_by = array.max_by
min_by = array.min_by
average = array.average
average_by = array.average_by
pick = array.pick
try_pick = array.try_pick
remove_all_in_place = array.remove_all_in_place
indexed = array.indexed
last = array.last
try_last = array.try_last
truncate = array.truncate
partition = array.partition
concat = array.concat
find_index = array.find_index
sort_by = array.sort_by
sum_by = array.sum_by
resize = array.resize
unzip = array.unzip
copy = array.copy
take = array.take
take_while = array.take_while
skip = array.skip
skip_while = array.skip_while
compare_to = array.compare_to
sort_with = array.sort_with
choose = array.choose

Array = array.FSharpArray
FSharpCons = array.FSharpCons

__all__ = [
    "Array",
    "Float64Array",
    "Int8Array",
    "Int16Array",
    "Int32Array",
    "Int64Array",
    "UInt8Array",
    "UInt16Array",
    "UInt32Array",
    "UInt64Array",
    "add_in_place",
    "add_range_in_place",
    "append",
    "average",
    "average_by",
    "choose",
    "chunk_by_size",
    "compare_to",
    "compare_with",
    "concat",
    "contains",
    "copy",
    "copy_to",
    "create",
    "empty",
    "equals_with",
    "exists",
    "exists_offset",
    "fill",
    "filter",
    "find",
    "find_back",
    "find_index",
    "find_index_back",
    "find_last_index",
    "fold",
    "fold_back",
    "fold_back2",
    "fold_back_indexed",
    "fold_back_indexed2",
    "fold_indexed",
    "for_all",
    "get_sub_array",
    "head",
    "index_of",
    "indexed",
    "initialize",
    "insert_at",
    "insert_many_at",
    "insert_range_in_place",
    "item",
    "iterate",
    "iterate_indexed",
    "last",
    "map",
    "map2",
    "map3",
    "map_fold",
    "map_fold_back",
    "map_indexed",
    "map_indexed2",
    "map_indexed3",
    "max",
    "max_by",
    "min",
    "min_by",
    "of_seq",
    "pairwise",
    "partition",
    "permute",
    "pick",
    "reduce",
    "reduce_back",
    "remove_at",
    "remove_in_place",
    "remove_many_at",
    "resize",
    "reverse",
    "scan",
    "scan_back",
    "set_slice",
    "singleton",
    "skip",
    "skip_while",
    "sort",
    "sort_by",
    "sort_by",
    "sort_in_place",
    "sort_in_place",
    "sort_in_place_by",
    "sort_in_place_with",
    "sort_with",
    "split_into",
    "sum",
    "sum_by",
    "tail",
    "take",
    "take_while",
    "transpose",
    "truncate",
    "try_find",
    "try_find_back",
    "try_find_index",
    "try_find_index_back",
    "try_head",
    "try_item",
    "try_pick",
    "unzip",
    "update_at",
    "windowed",
    "zip",
]
