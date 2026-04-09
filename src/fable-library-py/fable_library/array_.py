from typing import TypeVar

from .core import array, float32, float64, int8, int16, int32, int64, uint8, uint16, uint32, uint64


_T = TypeVar("_T")

# Typed array constructors (use these to create new typed arrays).
Int8ArrayCons = array.FSharpCons[int8]("Int8")
UInt8ArrayCons = array.FSharpCons[uint8]("UInt8")
Int16ArrayCons = array.FSharpCons[int16]("Int16")
UInt16ArrayCons = array.FSharpCons[uint16]("UInt16")
Int32ArrayCons = array.FSharpCons[int32]("Int32")
UInt32ArrayCons = array.FSharpCons[uint32]("UInt32")
Int64ArrayCons = array.FSharpCons[int64]("Int64")
UInt64ArrayCons = array.FSharpCons[uint64]("UInt64")
Float32ArrayCons = array.FSharpCons[float32]("Float32")
Float64ArrayCons = array.FSharpCons[float64]("Float64")
BoolArrayCons = array.FSharpCons[bool]("Bool")

# Typed array classes (use these for isinstance checks and type annotations).
Int8Array = array.Int8Array
UInt8Array = array.UInt8Array
Int16Array = array.Int16Array
UInt16Array = array.UInt16Array
Int32Array = array.Int32Array
UInt32Array = array.UInt32Array
Int64Array = array.Int64Array
UInt64Array = array.UInt64Array
Float32Array = array.Float32Array
Float64Array = array.Float64Array
BoolArray = array.BoolArray
GenericArray = array.GenericArray

# Import loose functions directly from array module
add_in_place = array.add_in_place
add_range_in_place = array.add_range_in_place
all_pairs = array.all_pairs
append = array.append
average = array.average
average_by = array.average_by
choose = array.choose
chunk_by_size = array.chunk_by_size
collect = array.collect
compare_to = array.compare_to
compare_with = array.compare_with
concat = array.concat
contains = array.contains
copy = array.copy
copy_to = array.copy_to
create = array.create
empty = array.empty
equals_with = array.equals_with
exactly_one = array.exactly_one
exists = array.exists
exists2 = array.exists2
exists_offset = array.exists_offset
fill = array.fill
filter = array.filter
find = array.find
find_back = array.find_back
find_index = array.find_index
find_index_back = array.find_index_back
find_last_index = array.find_last_index
fold = array.fold
fold2 = array.fold2
fold_back = array.fold_back
fold_back2 = array.fold_back2
fold_back_indexed = array.fold_back_indexed
fold_back_indexed2 = array.fold_back_indexed2
fold_indexed = array.fold_indexed
for_all = array.for_all
for_all2 = array.for_all2
get_sub_array = array.get_sub_array
head = array.head
index_of = array.index_of
indexed = array.indexed
initialize = array.initialize
insert_at = array.insert_at
insert_many_at = array.insert_many_at
insert_range_in_place = array.insert_range_in_place
item = array.item
iterate = array.iterate
iterate2 = array.iterate2
iterate_indexed = array.iterate_indexed
iterate_indexed2 = array.iterate_indexed2
last = array.last
map = array.map
map2 = array.map2
map3 = array.map3
map_fold = array.map_fold
map_fold_back = array.map_fold_back
map_indexed = array.map_indexed
map_indexed2 = array.map_indexed2
map_indexed3 = array.map_indexed3
max = array.max
max_by = array.max_by
min = array.min
min_by = array.min_by
of_seq = array.of_seq
pairwise = array.pairwise
partition = array.partition
permute = array.permute
pick = array.pick
random_choice = array.random_choice
random_choice_by = array.random_choice_by
random_choice_with = array.random_choice_with
random_choices = array.random_choices
random_choices_by = array.random_choices_by
random_choices_with = array.random_choices_with
random_sample = array.random_sample
random_sample_by = array.random_sample_by
random_sample_with = array.random_sample_with
random_shuffle = array.random_shuffle
random_shuffle_by = array.random_shuffle_by
random_shuffle_in_place = array.random_shuffle_in_place
random_shuffle_in_place_by = array.random_shuffle_in_place_by
random_shuffle_in_place_with = array.random_shuffle_in_place_with
random_shuffle_with = array.random_shuffle_with
reduce = array.reduce
reduce_back = array.reduce_back
remove_all_in_place = array.remove_all_in_place
remove_at = array.remove_at
remove_in_place = array.remove_in_place
remove_many_at = array.remove_many_at
resize = array.resize
reverse = array.reverse
scan = array.scan
scan_back = array.scan_back
set_slice = array.set_slice
singleton = array.singleton
skip = array.skip
skip_while = array.skip_while
sort = array.sort
sort_by = array.sort_by
sort_by_descending = array.sort_by_descending
sort_descending = array.sort_descending
sort_in_place = array.sort_in_place
sort_in_place_by = array.sort_in_place_by
sort_in_place_with = array.sort_in_place_with
sort_with = array.sort_with
split_into = array.split_into
sum = array.sum
sum_by = array.sum_by
tail = array.tail
take = array.take
take_while = array.take_while
transpose = array.transpose
truncate = array.truncate
try_exactly_one = array.try_exactly_one
try_find = array.try_find
try_find_back = array.try_find_back
try_find_index = array.try_find_index
try_find_index_back = array.try_find_index_back
try_head = array.try_head
try_item = array.try_item
try_last = array.try_last
try_pick = array.try_pick
unzip = array.unzip
unzip3 = array.unzip3
update_at = array.update_at
where = array.where
windowed = array.windowed
zero_create = array.zero_create
zip = array.zip
zip3 = array.zip3

Array = array.FSharpArray
FSharpCons = array.FSharpCons

__all__ = [
    "Array",
    "BoolArray",
    "BoolArrayCons",
    "Float32Array",
    "Float32ArrayCons",
    "Float64Array",
    "Float64ArrayCons",
    "GenericArray",
    "Int8Array",
    "Int8ArrayCons",
    "Int16Array",
    "Int16ArrayCons",
    "Int32Array",
    "Int32ArrayCons",
    "Int64Array",
    "Int64ArrayCons",
    "UInt8Array",
    "UInt8ArrayCons",
    "UInt16Array",
    "UInt16ArrayCons",
    "UInt32Array",
    "UInt32ArrayCons",
    "UInt64Array",
    "UInt64ArrayCons",
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
    "exists2",
    "exists_offset",
    "fill",
    "filter",
    "find",
    "find_back",
    "find_index",
    "find_index_back",
    "find_last_index",
    "fold",
    "fold2",
    "fold_back",
    "fold_back2",
    "fold_back_indexed",
    "fold_back_indexed2",
    "fold_indexed",
    "for_all",
    "for_all2",
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
    "iterate2",
    "iterate_indexed",
    "iterate_indexed2",
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
    "random_choice",
    "random_choice_by",
    "random_choice_with",
    "random_choices",
    "random_choices_by",
    "random_choices_with",
    "random_sample",
    "random_sample_by",
    "random_sample_with",
    "random_shuffle",
    "random_shuffle_by",
    "random_shuffle_in_place",
    "random_shuffle_in_place_by",
    "random_shuffle_in_place_with",
    "random_shuffle_with",
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
    "sort_by_descending",
    "sort_descending",
    "sort_in_place",
    "sort_in_place_by",
    "sort_in_place_with",
    "sort_with",
    "split_into",
    "sum",
    "sum_by",
    "all_pairs",
    "exactly_one",
    "tail",
    "take",
    "take_while",
    "transpose",
    "truncate",
    "try_exactly_one",
    "try_find",
    "try_find_back",
    "try_find_index",
    "try_find_index_back",
    "try_head",
    "try_item",
    "try_last",
    "try_pick",
    "unzip",
    "unzip3",
    "update_at",
    "windowed",
    "where",
    "zero_create",
    "zip",
    "zip3",
]
