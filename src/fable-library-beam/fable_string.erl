-module(fable_string).
-export([insert/3, remove/2, remove/3, starts_with/2, ends_with/2,
         pad_left/2, pad_left/3, pad_right/2, pad_right/3,
         replace/3, join/2, concat/1, replicate/2,
         is_null_or_empty/1, is_null_or_white_space/1]).

insert(Str, Idx, Value) ->
    iolist_to_binary([binary:part(Str, 0, Idx), Value, binary:part(Str, Idx, byte_size(Str) - Idx)]).

remove(Str, StartIdx) ->
    binary:part(Str, 0, StartIdx).

remove(Str, StartIdx, Count) ->
    iolist_to_binary([binary:part(Str, 0, StartIdx),
                      binary:part(Str, StartIdx + Count, byte_size(Str) - StartIdx - Count)]).

starts_with(Str, Prefix) ->
    binary:match(Str, Prefix) =:= {0, byte_size(Prefix)}.

ends_with(Str, Suffix) ->
    SuffixLen = byte_size(Suffix),
    StrLen = byte_size(Str),
    case StrLen >= SuffixLen of
        true -> binary:match(Str, Suffix, [{scope, {StrLen - SuffixLen, SuffixLen}}]) =/= nomatch;
        false -> false
    end.

pad_left(Str, Width) ->
    iolist_to_binary(string:pad(Str, Width, leading)).

pad_left(Str, Width, PadChar) ->
    iolist_to_binary(string:pad(Str, Width, leading, [PadChar])).

pad_right(Str, Width) ->
    iolist_to_binary(string:pad(Str, Width, trailing)).

pad_right(Str, Width, PadChar) ->
    iolist_to_binary(string:pad(Str, Width, trailing, [PadChar])).

replace(Str, Old, New) ->
    iolist_to_binary(string:replace(Str, Old, New, all)).

join(Sep, Items) ->
    iolist_to_binary(lists:join(Sep, Items)).

concat(Items) ->
    iolist_to_binary(Items).

replicate(Count, Str) ->
    iolist_to_binary(lists:duplicate(Count, Str)).

is_null_or_empty(Str) ->
    (Str =:= undefined) orelse (Str =:= <<>>).

is_null_or_white_space(Str) ->
    (Str =:= undefined) orelse (string:trim(Str) =:= <<>>).
