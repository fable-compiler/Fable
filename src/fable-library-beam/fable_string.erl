-module(fable_string).
-export([insert/3, remove/2, remove/3, starts_with/2, ends_with/2,
         pad_left/2, pad_left/3, pad_right/2, pad_right/3,
         replace/3, join/2, concat/1, replicate/2,
         is_null_or_empty/1, is_null_or_white_space/1,
         forall/2, exists/2, init/2, collect/2,
         iter/2, iteri/2, map/2, mapi/2, filter/2,
         index_of/2, index_of/3, last_index_of/2, last_index_of/3,
         contains/2, trim_chars/2, trim_start_chars/2, trim_end_chars/2,
         to_char_array/1, to_char_array/3,
         compare/2, compare_ignore_case/2,
         string_ctor_chars/1, string_ctor_char_count/2, string_ctor_chars_range/3,
         split/2, split/3, split_remove_empty/2, split_with_count/3]).

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

%% String module functions (F# String.forall, String.exists, etc.)

forall(Fn, Str) ->
    Chars = binary_to_list(Str),
    lists:all(Fn, Chars).

exists(Fn, Str) ->
    Chars = binary_to_list(Str),
    lists:any(Fn, Chars).

init(Count, Fn) ->
    Chars = lists:map(fun(I) -> (Fn)(I) end, lists:seq(0, Count - 1)),
    iolist_to_binary(Chars).

collect(Fn, Str) ->
    Chars = binary_to_list(Str),
    Parts = lists:map(fun(C) -> (Fn)(C) end, Chars),
    iolist_to_binary(Parts).

iter(Fn, Str) ->
    Chars = binary_to_list(Str),
    lists:foreach(Fn, Chars),
    ok.

iteri(Fn, Str) ->
    Chars = binary_to_list(Str),
    iteri_loop(Fn, Chars, 0),
    ok.

iteri_loop(_Fn, [], _Idx) -> ok;
iteri_loop(Fn, [C | Rest], Idx) ->
    ((Fn)(Idx))(C),
    iteri_loop(Fn, Rest, Idx + 1).

map(Fn, Str) ->
    Chars = binary_to_list(Str),
    Mapped = lists:map(Fn, Chars),
    << <<C/utf8>> || C <- Mapped >>.

mapi(Fn, Str) ->
    Chars = binary_to_list(Str),
    {Mapped, _} = lists:mapfoldl(fun(C, I) -> {((Fn)(I))(C), I + 1} end, 0, Chars),
    << <<C/utf8>> || C <- Mapped >>.

filter(Fn, Str) ->
    Chars = binary_to_list(Str),
    Filtered = lists:filter(Fn, Chars),
    << <<C/utf8>> || C <- Filtered >>.

%% IndexOf/LastIndexOf with offset support

index_of(Str, Sub) ->
    index_of(Str, Sub, 0).

index_of(Str, Sub, StartIdx) ->
    SearchStr = binary:part(Str, StartIdx, byte_size(Str) - StartIdx),
    case binary:match(SearchStr, Sub) of
        {Pos, _Len} -> Pos + StartIdx;
        nomatch -> -1
    end.

last_index_of(Str, Sub) ->
    last_index_of(Str, Sub, byte_size(Str) - 1).

last_index_of(Str, Sub, MaxIdx) ->
    SubLen = byte_size(Sub),
    SearchLen = erlang:min(MaxIdx + SubLen, byte_size(Str)),
    SearchStr = binary:part(Str, 0, SearchLen),
    case binary:matches(SearchStr, Sub) of
        [] -> -1;
        Matches -> element(1, lists:last(Matches))
    end.

contains(Str, Sub) ->
    binary:match(Str, Sub) =/= nomatch.

%% Trim with specific characters

trim_chars(Str, Chars) when is_integer(Chars) ->
    trim_chars(Str, [Chars]);
trim_chars(Str, Chars) ->
    iolist_to_binary(string:trim(binary_to_list(Str), both, Chars)).

trim_start_chars(Str, Chars) when is_integer(Chars) ->
    trim_start_chars(Str, [Chars]);
trim_start_chars(Str, Chars) ->
    iolist_to_binary(string:trim(binary_to_list(Str), leading, Chars)).

trim_end_chars(Str, Chars) when is_integer(Chars) ->
    trim_end_chars(Str, [Chars]);
trim_end_chars(Str, Chars) ->
    iolist_to_binary(string:trim(binary_to_list(Str), trailing, Chars)).

%% ToCharArray

to_char_array(Str) ->
    binary_to_list(Str).

to_char_array(Str, Start, Len) ->
    Part = binary:part(Str, Start, Len),
    binary_to_list(Part).

%% String comparison

compare(A, B) when A < B -> -1;
compare(A, B) when A > B -> 1;
compare(_, _) -> 0.

compare_ignore_case(A, B) ->
    LA = string:lowercase(A),
    LB = string:lowercase(B),
    compare(LA, LB).

%% String constructors

string_ctor_chars(Chars) ->
    << <<C/utf8>> || C <- Chars >>.

string_ctor_char_count(Char, Count) ->
    << <<Char/utf8>> || _ <- lists:seq(1, Count) >>.

string_ctor_chars_range(Chars, Start, Len) ->
    SubChars = lists:sublist(Chars, Start + 1, Len),
    << <<C/utf8>> || C <- SubChars >>.

%% Split functions

normalize_sep(Sep) when is_integer(Sep) -> <<Sep/utf8>>;
normalize_sep(Sep) -> Sep.

split(Str, Sep) ->
    binary:split(Str, normalize_sep(Sep), [global]).

split(Str, Sep, 0) ->
    split(Str, Sep);
split(Str, Sep, 1) ->
    split_remove_empty(Str, Sep);
split(Str, Sep, _Options) ->
    split(Str, Sep).

split_remove_empty(Str, Seps) ->
    Parts = binary:split(Str, normalize_sep(Seps), [global]),
    [P || P <- Parts, P =/= <<>>].

split_with_count(Str, Seps, Count) ->
    split_count_loop(Str, normalize_sep(Seps), Count, []).

split_count_loop(Str, _Seps, 1, Acc) ->
    lists:reverse([Str | Acc]);
split_count_loop(Str, Seps, Count, Acc) ->
    case binary:split(Str, Seps) of
        [Part, Rest] -> split_count_loop(Rest, Seps, Count - 1, [Part | Acc]);
        [_] -> lists:reverse([Str | Acc])
    end.
