-module(fable_string).
-export([
    insert/3,
    remove/2, remove/3,
    starts_with/2,
    ends_with/2,
    pad_left/2, pad_left/3,
    pad_right/2, pad_right/3,
    replace/3,
    join/2, join/4,
    join_strings/2,
    concat/1,
    replicate/2,
    is_null_or_empty/1,
    is_null_or_white_space/1,
    forall/2,
    exists/2,
    init/2,
    collect/2,
    iter/2,
    iteri/2,
    map/2,
    mapi/2,
    filter/2,
    index_of/2, index_of/3,
    index_of_any/2, index_of_any/3,
    last_index_of/2, last_index_of/3,
    contains/2,
    trim_chars/2,
    trim_start_chars/2,
    trim_end_chars/2,
    to_char_array/1, to_char_array/3,
    compare/2, compare/3,
    compare_ignore_case/2,
    string_ctor_chars/1,
    string_ctor_char_count/2,
    string_ctor_chars_range/3,
    split/2, split/3,
    split_remove_empty/2,
    split_with_count/3,
    to_string/1,
    printf/1,
    to_text/1, to_text/2, to_text/3, to_text/4, to_text/5,
    to_console/1, to_console/2, to_console/3, to_console/4, to_console/5,
    to_console_error/1, to_console_error/2, to_console_error/3,
    to_fail/1, to_fail/2, to_fail/3,
    interpolate/2,
    format/2,
    console_writeline/1,
    console_write/1,
    format_any/1,
    substring/2, substring/3,
    get_slice/3
]).

-spec insert(binary(), non_neg_integer(), binary()) -> binary().
-spec remove(binary(), non_neg_integer()) -> binary().
-spec remove(binary(), non_neg_integer(), non_neg_integer()) -> binary().
-spec starts_with(binary(), binary()) -> boolean().
-spec ends_with(binary(), binary()) -> boolean().
-spec pad_left(binary(), non_neg_integer()) -> binary().
-spec pad_left(binary(), non_neg_integer(), integer()) -> binary().
-spec pad_right(binary(), non_neg_integer()) -> binary().
-spec pad_right(binary(), non_neg_integer(), integer()) -> binary().
-spec replace(binary(), binary() | integer(), binary() | integer()) -> binary().
-spec join(binary(), list() | reference() | map()) -> binary().
-spec join(binary(), list() | reference() | map(), non_neg_integer(), non_neg_integer()) ->
    binary().
-spec join_strings(binary(), list() | reference() | map()) -> binary().
-spec concat(list() | reference() | map()) -> binary().
-spec replicate(non_neg_integer(), binary()) -> binary().
-spec is_null_or_empty(binary() | undefined) -> boolean().
-spec is_null_or_white_space(binary() | undefined) -> boolean().
-spec forall(fun((integer()) -> boolean()), binary()) -> boolean().
-spec exists(fun((integer()) -> boolean()), binary()) -> boolean().
-spec init(non_neg_integer(), fun((non_neg_integer()) -> term())) -> binary().
-spec collect(fun((integer()) -> term()), binary()) -> binary().
-spec iter(fun((integer()) -> term()), binary()) -> ok.
-spec iteri(fun((non_neg_integer()) -> fun((integer()) -> term())), binary()) -> ok.
-spec map(fun((integer()) -> integer()), binary()) -> binary().
-spec mapi(fun((non_neg_integer()) -> fun((integer()) -> integer())), binary()) -> binary().
-spec filter(fun((integer()) -> boolean()), binary()) -> binary().
-spec index_of(binary(), binary()) -> integer().
-spec index_of(binary(), binary(), non_neg_integer()) -> integer().
-spec index_of_any(binary(), list() | reference()) -> integer().
-spec index_of_any(binary(), list() | reference(), non_neg_integer()) -> integer().
-spec last_index_of(binary(), binary()) -> integer().
-spec last_index_of(binary(), binary(), non_neg_integer()) -> integer().
-spec contains(binary(), binary()) -> boolean().
-spec trim_chars(binary(), list() | integer()) -> binary().
-spec trim_start_chars(binary(), list() | integer()) -> binary().
-spec trim_end_chars(binary(), list() | integer()) -> binary().
-spec to_char_array(binary()) -> list().
-spec to_char_array(binary(), non_neg_integer(), non_neg_integer()) -> list().
-spec compare(binary(), binary()) -> -1 | 0 | 1.
-spec compare_ignore_case(binary(), binary()) -> -1 | 0 | 1.
-spec string_ctor_chars(list()) -> binary().
-spec string_ctor_char_count(integer(), non_neg_integer()) -> binary().
-spec string_ctor_chars_range(list(), non_neg_integer(), non_neg_integer()) -> binary().
-spec split(binary(), binary() | integer()) -> list().
-spec split(binary(), binary() | integer(), non_neg_integer()) -> list().
-spec split_remove_empty(binary(), binary() | integer()) -> list().
-spec split_with_count(binary(), binary() | integer() | list(), non_neg_integer()) -> list().
-spec to_string(term()) -> binary().
-spec printf(binary()) -> map().
-spec to_text(map() | binary()) -> binary() | fun().
-spec to_text(map() | binary(), term()) -> binary() | fun().
-spec to_text(map() | binary(), term(), term()) -> binary() | fun().
-spec to_text(map() | binary(), term(), term(), term()) -> binary() | fun().
-spec to_text(map() | binary(), term(), term(), term(), term()) -> binary() | fun().
-spec to_console(map() | binary()) -> ok | fun().
-spec to_console(map() | binary(), term()) -> ok | fun().
-spec to_console(map() | binary(), term(), term()) -> ok | fun().
-spec to_console(map() | binary(), term(), term(), term()) -> ok | fun().
-spec to_console(map() | binary(), term(), term(), term(), term()) -> ok | fun().
-spec to_console_error(map() | binary()) -> ok | fun().
-spec to_console_error(map() | binary(), term()) -> ok | fun().
-spec to_console_error(map() | binary(), term(), term()) -> ok | fun().
-spec to_fail(map() | binary()) -> no_return() | fun().
-spec to_fail(map() | binary(), term()) -> no_return() | fun().
-spec to_fail(map() | binary(), term(), term()) -> no_return() | fun().
-spec interpolate(binary(), list() | term()) -> binary().
-spec format(binary(), list() | reference() | term()) -> binary().
-spec console_writeline(term()) -> ok.
-spec console_write(term()) -> ok.
-spec format_any(term()) -> binary().

insert(Str, Idx, Value) ->
    iolist_to_binary([binary:part(Str, 0, Idx), Value, binary:part(Str, Idx, byte_size(Str) - Idx)]).

remove(Str, StartIdx) ->
    binary:part(Str, 0, StartIdx).

remove(Str, StartIdx, Count) ->
    iolist_to_binary([
        binary:part(Str, 0, StartIdx),
        binary:part(Str, StartIdx + Count, byte_size(Str) - StartIdx - Count)
    ]).

starts_with(_Str, <<>>) ->
    true;
starts_with(Str, Prefix) ->
    binary:match(Str, Prefix) =:= {0, byte_size(Prefix)}.

ends_with(_Str, <<>>) ->
    true;
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

replace(Str, Old, New) when is_integer(Old), is_integer(New) ->
    iolist_to_binary(string:replace(Str, <<Old/utf8>>, <<New/utf8>>, all));
replace(Str, Old, New) when is_integer(Old) ->
    iolist_to_binary(string:replace(Str, <<Old/utf8>>, New, all));
replace(Str, Old, New) when is_integer(New) ->
    iolist_to_binary(string:replace(Str, Old, <<New/utf8>>, all));
replace(Str, Old, New) ->
    iolist_to_binary(string:replace(Str, Old, New, all)).

join(Sep, Items) when is_reference(Items) ->
    join(Sep, fable_utils:to_list(Items));
join(Sep, Items) when is_map(Items) ->
    join(Sep, fable_utils:to_list(Items));
join(Sep, Items) ->
    iolist_to_binary(lists:join(Sep, Items)).

join(Sep, Items, StartIndex, Count) when is_reference(Items) ->
    join(Sep, fable_utils:to_list(Items), StartIndex, Count);
join(Sep, Items, StartIndex, Count) when is_map(Items) ->
    join(Sep, fable_utils:to_list(Items), StartIndex, Count);
join(Sep, Items, StartIndex, Count) ->
    Sub = lists:sublist(Items, StartIndex + 1, Count),
    iolist_to_binary(lists:join(Sep, Sub)).

%% join_strings: same as join but ensures all items are converted to string (integer_to_binary etc.)
join_strings(Sep, Items) when is_reference(Items) ->
    join_strings(Sep, fable_utils:to_list(Items));
join_strings(Sep, Items) when is_map(Items) ->
    join_strings(Sep, fable_utils:to_list(Items));
join_strings(Sep, Items) ->
    StrItems = [to_string(I) || I <- Items],
    iolist_to_binary(lists:join(Sep, StrItems)).

concat(Items) when is_reference(Items) ->
    concat(fable_utils:to_list(Items));
concat(Items) when is_map(Items) ->
    concat(fable_utils:to_list(Items));
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
    Chars = unicode:characters_to_list(Str),
    lists:all(Fn, Chars).

exists(Fn, Str) ->
    Chars = unicode:characters_to_list(Str),
    lists:any(Fn, Chars).

init(Count, Fn) ->
    Chars = lists:map(fun(I) -> (Fn)(I) end, lists:seq(0, Count - 1)),
    iolist_to_binary(Chars).

collect(Fn, Str) ->
    Chars = unicode:characters_to_list(Str),
    Parts = lists:map(fun(C) -> (Fn)(C) end, Chars),
    iolist_to_binary(Parts).

iter(Fn, Str) ->
    Chars = unicode:characters_to_list(Str),
    lists:foreach(Fn, Chars),
    ok.

iteri(Fn, Str) ->
    Chars = unicode:characters_to_list(Str),
    iteri_loop(Fn, Chars, 0),
    ok.

iteri_loop(_Fn, [], _Idx) ->
    ok;
iteri_loop(Fn, [C | Rest], Idx) ->
    ((Fn)(Idx))(C),
    iteri_loop(Fn, Rest, Idx + 1).

map(Fn, Str) ->
    Chars = unicode:characters_to_list(Str),
    Mapped = lists:map(Fn, Chars),
    <<<<C/utf8>> || C <- Mapped>>.

mapi(Fn, Str) ->
    Chars = unicode:characters_to_list(Str),
    {Mapped, _} = lists:mapfoldl(fun(C, I) -> {((Fn)(I))(C), I + 1} end, 0, Chars),
    <<<<C/utf8>> || C <- Mapped>>.

filter(Fn, Str) ->
    Chars = unicode:characters_to_list(Str),
    Filtered = lists:filter(Fn, Chars),
    <<<<C/utf8>> || C <- Filtered>>.

%% IndexOf/LastIndexOf with offset support

index_of(Str, Sub) ->
    index_of(Str, Sub, 0).

index_of(_Str, <<>>, StartIdx) ->
    StartIdx;
index_of(Str, Sub, StartIdx) ->
    SearchStr = binary:part(Str, StartIdx, byte_size(Str) - StartIdx),
    case binary:match(SearchStr, Sub) of
        {Pos, _Len} -> Pos + StartIdx;
        nomatch -> -1
    end.

%% IndexOfAny: find first position of any char in the array
index_of_any(Str, Chars) ->
    index_of_any(Str, Chars, 0).

index_of_any(Str, Chars, StartIdx) when is_reference(Chars) ->
    index_of_any(Str, get(Chars), StartIdx);
index_of_any(Str, Chars, StartIdx) ->
    CharSet = Chars,
    Codepoints = unicode:characters_to_list(Str),
    index_of_any_loop(Codepoints, CharSet, 0, StartIdx).

index_of_any_loop([], _CharSet, _Idx, _StartIdx) ->
    -1;
index_of_any_loop([_C | Rest], CharSet, Idx, StartIdx) when Idx < StartIdx ->
    index_of_any_loop(Rest, CharSet, Idx + 1, StartIdx);
index_of_any_loop([C | Rest], CharSet, Idx, StartIdx) ->
    case lists:member(C, CharSet) of
        true -> Idx;
        false -> index_of_any_loop(Rest, CharSet, Idx + 1, StartIdx)
    end.

last_index_of(Str, Sub) ->
    last_index_of(Str, Sub, byte_size(Str) - 1).

last_index_of(Str, <<>>, MaxIdx) ->
    erlang:min(MaxIdx + 1, byte_size(Str));
last_index_of(Str, Sub, MaxIdx) ->
    SubLen = byte_size(Sub),
    SearchLen = erlang:min(MaxIdx + SubLen, byte_size(Str)),
    SearchStr = binary:part(Str, 0, SearchLen),
    case binary:matches(SearchStr, Sub) of
        [] -> -1;
        Matches -> element(1, lists:last(Matches))
    end.

contains(_Str, <<>>) ->
    true;
contains(Str, Sub) ->
    binary:match(Str, Sub) =/= nomatch.

%% Trim with specific characters

trim_chars(Str, Chars) when is_integer(Chars) ->
    trim_chars(Str, [Chars]);
trim_chars(Str, Chars) ->
    iolist_to_binary(string:trim(Str, both, Chars)).

trim_start_chars(Str, Chars) when is_integer(Chars) ->
    trim_start_chars(Str, [Chars]);
trim_start_chars(Str, Chars) ->
    iolist_to_binary(string:trim(Str, leading, Chars)).

trim_end_chars(Str, Chars) when is_integer(Chars) ->
    trim_end_chars(Str, [Chars]);
trim_end_chars(Str, Chars) ->
    iolist_to_binary(string:trim(Str, trailing, Chars)).

%% ToCharArray
%%
%% An F# string is a UTF-8 binary, so splitting it into chars means walking Unicode
%% codepoints, not raw bytes — `binary_to_list/1` would instead hand back one bogus
%% "char" per byte of a multi-byte codepoint's UTF-8 encoding. Start/Len are char
%% (codepoint) offsets, matching .NET's `ToCharArray(startIndex, length)`, so they're
%% applied to the codepoint list rather than to the binary's byte offsets.

to_char_array(Str) ->
    unicode:characters_to_list(Str).

to_char_array(Str, Start, Len) ->
    Chars = unicode:characters_to_list(Str),
    lists:sublist(Chars, Start + 1, Len).

%% String comparison

compare(A, B) when A < B -> -1;
compare(A, B) when A > B -> 1;
compare(_, _) -> 0.

compare_ignore_case(A, B) ->
    LA = string:lowercase(A),
    LB = string:lowercase(B),
    compare(LA, LB).

%% compare/3 — dispatch on ignoreCase (bool) or StringComparison enum (int)
compare(A, B, true) -> compare_ignore_case(A, B);
compare(A, B, 1) -> compare_ignore_case(A, B);   % CurrentCultureIgnoreCase
compare(A, B, 3) -> compare_ignore_case(A, B);   % InvariantCultureIgnoreCase
compare(A, B, 5) -> compare_ignore_case(A, B);   % OrdinalIgnoreCase
compare(A, B, _) -> compare(A, B).

%% String constructors

string_ctor_chars(Chars) ->
    <<<<C/utf8>> || C <- Chars>>.

string_ctor_char_count(Char, Count) ->
    <<<<Char/utf8>> || _ <- lists:seq(1, Count)>>.

string_ctor_chars_range(Chars, Start, Len) ->
    SubChars = lists:sublist(Chars, Start + 1, Len),
    <<<<C/utf8>> || C <- SubChars>>.

%% Split functions

normalize_sep(Sep) when is_integer(Sep) -> <<Sep/utf8>>;
normalize_sep(Sep) -> Sep.

%% Normalize separator that may be a list of chars to a list of binaries
normalize_sep_list(Sep) when is_integer(Sep) -> <<Sep/utf8>>;
normalize_sep_list(Sep) when is_binary(Sep) -> Sep;
normalize_sep_list(Sep) when is_list(Sep) ->
    [<<C/utf8>> || C <- Sep].

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
    split_count_loop(Str, normalize_sep_list(Seps), Count, []).

split_count_loop(Str, _Seps, 1, Acc) ->
    lists:reverse([Str | Acc]);
split_count_loop(Str, Seps, Count, Acc) ->
    case binary:split(Str, Seps) of
        [Part, Rest] -> split_count_loop(Rest, Seps, Count - 1, [Part | Acc]);
        [_] -> lists:reverse([Str | Acc])
    end.

%% Generic value-to-string conversion for string interpolation.
%% Handles all Erlang types, outputting human-readable strings (not Erlang term format).
to_string(V) when is_binary(V) -> V;
to_string(V) when is_integer(V) -> integer_to_binary(V);
to_string(V) when is_float(V) ->
    %% Match .NET ToString() "G" format: whole-number floats produce "2" not "2.0"
    case V == trunc(V) of
        true -> integer_to_binary(trunc(V));
        false -> float_to_binary(V, [{decimals, 10}, compact])
    end;
to_string(V) when is_atom(V) -> atom_to_binary(V, utf8);
to_string(V) when is_boolean(V) ->
    case V of
        true -> <<"true">>;
        false -> <<"false">>
    end;
to_string(V) ->
    iolist_to_binary(io_lib:format(binary_to_list(<<"~p">>), [V])).

%% =====================================================================
%% Printf / sprintf / String.Format support
%% =====================================================================

%% printf/1 — Parse an F# format string and return a format object (map).
%% The format object has: input (original string), cont (function accepting a continuation).
printf(Str) ->
    Printer = fs_format(Str),
    #{input => Str, cont => Printer}.

%% apply_curried/2 — Apply a list of args one at a time to a curried function.
apply_curried(Fun, []) -> Fun;
apply_curried(Fun, [Arg | Rest]) -> apply_curried(Fun(Arg), Rest).

%% to_text — sprintf: returns a curried function (or final string for no-arg formats).
%% Extra args (from Fable inlining) are applied to the curried result.
to_text(FmtObj) when is_map(FmtObj) ->
    Cont = maps:get(cont, FmtObj),
    Cont(fun(X) -> X end);
to_text(Str) when is_binary(Str) ->
    Str.
to_text(FmtObj, A1) -> apply_curried(to_text(FmtObj), [A1]).
to_text(FmtObj, A1, A2) -> apply_curried(to_text(FmtObj), [A1, A2]).
to_text(FmtObj, A1, A2, A3) -> apply_curried(to_text(FmtObj), [A1, A2, A3]).
to_text(FmtObj, A1, A2, A3, A4) -> apply_curried(to_text(FmtObj), [A1, A2, A3, A4]).

%% to_console — printfn: prints to stdout with newline.
to_console(FmtObj) when is_map(FmtObj) ->
    Cont = maps:get(cont, FmtObj),
    Cont(fun(X) ->
        io:format("~ts~n", [X]),
        ok
    end);
to_console(Str) when is_binary(Str) ->
    io:format("~ts~n", [Str]),
    ok.
to_console(FmtObj, A1) -> apply_curried(to_console(FmtObj), [A1]).
to_console(FmtObj, A1, A2) -> apply_curried(to_console(FmtObj), [A1, A2]).
to_console(FmtObj, A1, A2, A3) -> apply_curried(to_console(FmtObj), [A1, A2, A3]).
to_console(FmtObj, A1, A2, A3, A4) -> apply_curried(to_console(FmtObj), [A1, A2, A3, A4]).

%% to_console_error — eprintfn: prints to stderr with newline.
to_console_error(FmtObj) when is_map(FmtObj) ->
    Cont = maps:get(cont, FmtObj),
    Cont(fun(X) ->
        io:format(standard_error, "~ts~n", [X]),
        ok
    end);
to_console_error(Str) when is_binary(Str) ->
    io:format(standard_error, "~ts~n", [Str]),
    ok.
to_console_error(FmtObj, A1) -> apply_curried(to_console_error(FmtObj), [A1]).
to_console_error(FmtObj, A1, A2) -> apply_curried(to_console_error(FmtObj), [A1, A2]).

%% to_fail — failwithf: formats and throws an error.
to_fail(FmtObj) when is_map(FmtObj) ->
    Cont = maps:get(cont, FmtObj),
    Cont(fun(X) -> erlang:error(X) end);
to_fail(Str) when is_binary(Str) ->
    erlang:error(Str).
to_fail(FmtObj, A1) -> apply_curried(to_fail(FmtObj), [A1]).
to_fail(FmtObj, A1, A2) -> apply_curried(to_fail(FmtObj), [A1, A2]).

%% interpolate/2 — String interpolation: format string with %P() placeholders replaced by values.
interpolate(Str, Values) when is_reference(Values) ->
    interpolate(Str, get(Values));
interpolate(Str, Values) ->
    ValList =
        case is_list(Values) of
            true -> Values;
            false -> [Values]
        end,
    interpolate_loop(Str, ValList, []).

interpolate_loop(<<>>, _Values, Acc) ->
    iolist_to_binary(lists:reverse(Acc));
interpolate_loop(<<"%P()", Rest/binary>>, [V | Vs], Acc) ->
    interpolate_loop(Rest, Vs, [to_string(V) | Acc]);
interpolate_loop(<<"%%", Rest/binary>>, Values, Acc) ->
    %% Escaped percent sign → literal %
    interpolate_loop(Rest, Values, [<<"%">> | Acc]);
interpolate_loop(<<$%, Rest/binary>>, Values, Acc) ->
    %% Try to match format specifier followed by %P()
    case extract_spec_before_placeholder(Rest) of
        {Spec, AfterPlaceholder, true} ->
            [V | Vs] = Values,
            Formatted = format_value(Spec, V),
            interpolate_loop(AfterPlaceholder, Vs, [Formatted | Acc]);
        _ ->
            interpolate_loop(Rest, Values, [<<$%>> | Acc])
    end;
interpolate_loop(<<C/utf8, Rest/binary>>, Values, Acc) ->
    interpolate_loop(Rest, Values, [<<C/utf8>> | Acc]).

%% Extract a printf format specifier that ends with %P().
%% Returns {SpecString, RestAfterPlaceholder, true} or false.
extract_spec_before_placeholder(Bin) ->
    extract_spec_before_placeholder(Bin, []).

extract_spec_before_placeholder(<<"%P()", Rest/binary>>, SpecAcc) ->
    {lists:reverse(SpecAcc), Rest, true};
extract_spec_before_placeholder(<<>>, _SpecAcc) ->
    false;
extract_spec_before_placeholder(<<C, Rest/binary>>, SpecAcc) ->
    extract_spec_before_placeholder(Rest, [C | SpecAcc]).

%% format/2 — .NET String.Format("{0} {1}", [Arg0, Arg1]).
format(FmtStr, Args) when is_reference(Args) ->
    format(FmtStr, get(Args));
format(FmtStr, Args) ->
    ArgList =
        case is_list(Args) of
            true -> Args;
            false -> [Args]
        end,
    format_dotnet(FmtStr, ArgList, []).

format_dotnet(<<>>, _Args, Acc) ->
    iolist_to_binary(lists:reverse(Acc));
format_dotnet(<<"{{", Rest/binary>>, Args, Acc) ->
    format_dotnet(Rest, Args, [<<"{">> | Acc]);
format_dotnet(<<"}}", Rest/binary>>, Args, Acc) ->
    format_dotnet(Rest, Args, [<<"}">> | Acc]);
format_dotnet(<<"{", Rest/binary>>, Args, Acc) ->
    {IdxStr, Align, Spec, Rest2} = parse_format_index(Rest, []),
    Idx = list_to_integer(IdxStr),
    Val = lists:nth(Idx + 1, Args),
    Formatted0 =
        case Spec of
            [] -> to_string(Val);
            _ -> apply_dotnet_spec(Spec, Val)
        end,
    Formatted = apply_alignment(Align, Formatted0),
    format_dotnet(Rest2, Args, [Formatted | Acc]);
format_dotnet(<<C/utf8, Rest/binary>>, Args, Acc) ->
    format_dotnet(Rest, Args, [<<C/utf8>> | Acc]).

%% parse_format_index/2 — Parse "{index[,alignment][:spec]}" into its components.
parse_format_index(<<"}", Rest/binary>>, Acc) ->
    {lists:reverse(Acc), [], [], Rest};
parse_format_index(<<",", Rest/binary>>, Acc) ->
    %% Alignment, e.g. {0,6} or {0,-6:X4}
    {Align, Spec, Rest2} = parse_format_align(Rest, []),
    {lists:reverse(Acc), Align, Spec, Rest2};
parse_format_index(<<":", Rest/binary>>, Acc) ->
    %% Format specifier, e.g. {0:X4}
    {Spec, Rest2} = parse_format_spec(Rest, []),
    {lists:reverse(Acc), [], Spec, Rest2};
parse_format_index(<<C, Rest/binary>>, Acc) ->
    parse_format_index(Rest, [C | Acc]).

parse_format_align(<<"}", Rest/binary>>, Acc) ->
    {lists:reverse(Acc), [], Rest};
parse_format_align(<<":", Rest/binary>>, Acc) ->
    {Spec, Rest2} = parse_format_spec(Rest, []),
    {lists:reverse(Acc), Spec, Rest2};
parse_format_align(<<C, Rest/binary>>, Acc) ->
    parse_format_align(Rest, [C | Acc]).

parse_format_spec(<<"}", Rest/binary>>, Acc) ->
    {lists:reverse(Acc), Rest};
parse_format_spec(<<C, Rest/binary>>, Acc) ->
    parse_format_spec(Rest, [C | Acc]).

%% apply_alignment/2 — Pad to the given alignment width. A positive width
%% right-aligns (pad left); a negative width left-aligns (pad right).
apply_alignment([], Str) ->
    Str;
apply_alignment(Align, Str) ->
    case catch list_to_integer(Align) of
        N when is_integer(N), N >= 0 -> pad_left(Str, N);
        N when is_integer(N) -> pad_right(Str, -N);
        _ -> Str
    end.

%% apply_dotnet_spec/2 — Apply a .NET numeric format specifier (the text after ':'
%% in "{0:Spec}") to a value. Handles the standard numeric specifiers; falls back
%% to the plain string representation for non-numeric values or unsupported specs.
apply_dotnet_spec(Spec, Value) when is_number(Value) ->
    case parse_standard_spec(Spec) of
        {Type, Prec} -> apply_dotnet_numeric(Type, Prec, Value);
        custom -> to_string(Value)
    end;
apply_dotnet_spec(_Spec, Value) ->
    to_string(Value).

%% parse_standard_spec/1 — A standard numeric spec is a single letter optionally
%% followed by a precision number (e.g. "X4", "D", "F2"). Anything else (custom
%% numeric format strings like "#,##0.00") returns `custom`.
parse_standard_spec([Type | Rest]) when
    (Type >= $A andalso Type =< $Z) orelse (Type >= $a andalso Type =< $z)
->
    case Rest of
        [] -> {Type, -1};
        _ ->
            case all_digits(Rest) of
                true -> {Type, list_to_integer(Rest)};
                false -> custom
            end
    end;
parse_standard_spec(_) ->
    custom.

all_digits([]) -> true;
all_digits([C | T]) when C >= $0, C =< $9 -> all_digits(T);
all_digits(_) -> false.

%% apply_dotnet_numeric/3 — Format a numeric value per a standard specifier.
%% See https://learn.microsoft.com/dotnet/standard/base-types/standard-numeric-format-strings
apply_dotnet_numeric(Type, Prec, Value) when Type =:= $X; Type =:= $x ->
    Hex = format_raw(Type, -1, Value),
    pad_zeros(Hex, Prec);
apply_dotnet_numeric(Type, Prec, Value) when Type =:= $D; Type =:= $d ->
    pad_zeros(integer_to_binary(trunc(Value)), Prec);
apply_dotnet_numeric(Type, Prec, Value) when Type =:= $F; Type =:= $f ->
    P = default_prec(Prec, 2),
    format_raw($f, P, Value);
apply_dotnet_numeric(Type, Prec, Value) when Type =:= $E; Type =:= $e ->
    P = default_prec(Prec, 6),
    format_exponential(Type, P, Value);
apply_dotnet_numeric(Type, Prec, Value) when Type =:= $G; Type =:= $g ->
    format_general(Type, Prec, Value);
apply_dotnet_numeric(Type, Prec, Value) when Type =:= $N; Type =:= $n ->
    P = default_prec(Prec, 2),
    {IntB, DecB} = split_int_dec(format_raw($f, P, Value)),
    join_int_dec(thousand_separate(IntB), DecB, P);
apply_dotnet_numeric(Type, Prec, Value) when Type =:= $C; Type =:= $c ->
    P = default_prec(Prec, 2),
    {IntB, DecB} = split_int_dec(format_raw($f, P, abs_num(Value))),
    Body = join_int_dec(<<"¤"/utf8, (thousand_separate(IntB))/binary>>, DecB, P),
    case Value < 0 of
        true -> <<"(", Body/binary, ")">>;
        false -> Body
    end;
apply_dotnet_numeric(Type, Prec, Value) when Type =:= $P; Type =:= $p ->
    P = default_prec(Prec, 2),
    {IntB, DecB} = split_int_dec(format_raw($f, P, Value * 100)),
    Body = join_int_dec(thousand_separate(IntB), DecB, P),
    <<Body/binary, " %">>;
apply_dotnet_numeric(Type, Prec, Value) when Type =:= $B; Type =:= $b ->
    Unsigned = trunc(Value) band 16#FFFFFFFF,
    MinWidth =
        if
            Prec =< 0 -> 1;
            true -> Prec
        end,
    pad_left(integer_to_binary(Unsigned, 2), MinWidth, $0);
apply_dotnet_numeric(Type, _Prec, Value) when Type =:= $R; Type =:= $r ->
    to_string(Value);
apply_dotnet_numeric(_Type, _Prec, Value) ->
    to_string(Value).

%% default_prec/2 — Use Default when the spec carried no explicit precision (-1).
default_prec(Prec, Default) ->
    if
        Prec < 0 -> Default;
        true -> Prec
    end.

%% format_exponential/3 — .NET "E"/"e" format: one leading digit, Prec decimals,
%% an explicit exponent sign and a minimum of three exponent digits (e.g. 1234.5
%% with "E2" -> "1.23E+003"). Erlang's scientific notation gives "1.23e+03", which
%% we reshape: uppercase the exponent marker on demand and left-pad the exponent.
format_exponential(TypeChar, P, Value) ->
    Sci = float_to_binary(float(Value), [{scientific, P}]),
    [Mantissa, ExpPart] = binary:split(Sci, <<"e">>),
    <<ExpSign, ExpDigits/binary>> = ExpPart,
    ExpPadded = pad_left(ExpDigits, 3, $0),
    EChar =
        case TypeChar of
            $E -> <<"E">>;
            _ -> <<"e">>
        end,
    <<Mantissa/binary, EChar/binary, ExpSign, ExpPadded/binary>>.

%% format_general/3 — .NET "G"/"g" (general) format. The value is shown with
%% `Prec` significant digits, choosing fixed-point or scientific notation
%% (scientific when the exponent is < -4 or >= Prec), with trailing zeros
%% removed. Unlike "E"/"e", the exponent uses a 2-digit minimum. When no
%% precision is given, the value uses its default representation (integers in
%% full), matching bare interpolation (`$"{x}"`).
format_general(_Type, Prec, Value) when Prec =< 0 ->
    to_string(Value);
format_general(Type, Prec, Value) ->
    F = float(Value),
    case F == 0.0 of
        true ->
            <<"0">>;
        false ->
            Exp = trunc(math:floor(math:log10(abs(F)))),
            EChar =
                case Type of
                    $G -> $E;
                    _ -> $e
                end,
            case Exp < -4 orelse Exp >= Prec of
                true -> general_scientific(EChar, Prec, F);
                false -> general_fixed(Prec, F, Exp)
            end
    end.

%% general_fixed/3 — Fixed-point notation with (Prec - 1 - Exp) decimals so the
%% result carries Prec significant digits, trailing zeros trimmed.
general_fixed(Prec, F, Exp) ->
    Decimals = max(0, Prec - 1 - Exp),
    trim_trailing_zeros(format_raw($f, Decimals, F)).

%% general_scientific/3 — Scientific notation with Prec significant digits
%% (one leading digit + Prec-1 decimals), trailing zeros trimmed, and a
%% 2-digit minimum exponent (e.g. "1.23E+06").
general_scientific(EChar, Prec, F) ->
    Sci = float_to_binary(F, [{scientific, max(0, Prec - 1)}]),
    [Mantissa0, ExpPart] = binary:split(Sci, <<"e">>),
    Mantissa = trim_trailing_zeros(Mantissa0),
    <<ExpSign, ExpDigits/binary>> = ExpPart,
    ExpPadded = pad_left(ExpDigits, 2, $0),
    <<Mantissa/binary, EChar, ExpSign, ExpPadded/binary>>.

abs_num(V) when V < 0 -> -V;
abs_num(V) -> V.

%% split_int_dec/1 — Split "1234.50" into {<<"1234">>, <<"50">>}.
split_int_dec(Bin) ->
    case binary:split(Bin, <<".">>) of
        [I, D] -> {I, D};
        [I] -> {I, <<>>}
    end.

%% join_int_dec/3 — Reassemble integral and decimal parts, dropping the decimal
%% point when the precision is zero.
join_int_dec(IntB, DecB, Prec) ->
    case Prec > 0 of
        true -> <<IntB/binary, ".", DecB/binary>>;
        false -> IntB
    end.

%% thousand_separate/1 — Insert ',' every three digits, keeping any leading sign.
thousand_separate(<<$-, Rest/binary>>) ->
    <<$-, (group_thousands(Rest))/binary>>;
thousand_separate(Bin) ->
    group_thousands(Bin).

group_thousands(Bin) ->
    Digits = lists:reverse(binary_to_list(Bin)),
    list_to_binary(do_group(Digits, 1, [])).

do_group([], _N, Acc) ->
    Acc;
do_group([D], _N, Acc) ->
    [D | Acc];
do_group([D | T], N, Acc) when N rem 3 =:= 0 ->
    do_group(T, N + 1, [$,, D | Acc]);
do_group([D | T], N, Acc) ->
    do_group(T, N + 1, [D | Acc]).

%% pad_zeros/2 — Left-pad to a minimum width with '0', keeping any leading sign.
pad_zeros(Bin, Width) when Width =< 0 ->
    Bin;
pad_zeros(<<$-, Rest/binary>>, Width) ->
    <<$-, (pad_left(Rest, Width, $0))/binary>>;
pad_zeros(Bin, Width) ->
    pad_left(Bin, Width, $0).

%% =====================================================================
%% Internal: F# format string parser
%% =====================================================================

%% fs_format/1 — Parse F# format string like "%d %s %.2f" and return
%% fun(Cont) -> fun(Arg1) -> fun(Arg2) -> ... -> Cont(Result) end end end
fs_format(Str) ->
    {Parts, Specs} = parse_format(Str, <<>>, [], []),
    create_printer(Parts, Specs).

%% parse_format/4 — Split format string into literal parts and format specifiers.
%% Returns {[binary()], [string()]} where Parts has one more element than Specs.
parse_format(<<>>, Current, Parts, Specs) ->
    {lists:reverse([Current | Parts]), lists:reverse(Specs)};
parse_format(<<"%%", Rest/binary>>, Current, Parts, Specs) ->
    %% Escaped percent sign
    parse_format(Rest, <<Current/binary, "%">>, Parts, Specs);
parse_format(<<"%", Rest/binary>>, Current, Parts, Specs) ->
    %% Start of format specifier
    {SpecStr, Rest2} = parse_spec(Rest, [$%]),
    parse_format(Rest2, <<>>, [Current | Parts], [lists:reverse(SpecStr) | Specs]);
parse_format(<<C/utf8, Rest/binary>>, Current, Parts, Specs) ->
    parse_format(Rest, <<Current/binary, C/utf8>>, Parts, Specs).

%% parse_spec/2 — Parse a single format specifier after the %.
%% Handles flags (0, -, +, space), width, precision (.N), and type char.
parse_spec(<<C, Rest/binary>>, Acc) when C =:= $0; C =:= $-; C =:= $+; C =:= $\s ->
    parse_spec(Rest, [C | Acc]);
parse_spec(<<C, Rest/binary>>, Acc) when C >= $0, C =< $9 ->
    parse_spec_width(Rest, [C | Acc]);
parse_spec(<<".", Rest/binary>>, Acc) ->
    parse_spec_precision(Rest, [$. | Acc]);
parse_spec(<<C, Rest/binary>>, Acc) ->
    %% Type character (d, s, f, g, x, etc.)
    {[C | Acc], Rest}.

parse_spec_width(<<C, Rest/binary>>, Acc) when C >= $0, C =< $9 ->
    parse_spec_width(Rest, [C | Acc]);
parse_spec_width(<<".", Rest/binary>>, Acc) ->
    parse_spec_precision(Rest, [$. | Acc]);
parse_spec_width(<<C, Rest/binary>>, Acc) ->
    {[C | Acc], Rest}.

parse_spec_precision(<<C, Rest/binary>>, Acc) when C >= $0, C =< $9 ->
    parse_spec_precision(Rest, [C | Acc]);
parse_spec_precision(<<C, Rest/binary>>, Acc) ->
    {[C | Acc], Rest}.

%% create_printer/2 — Build nested curried functions from parts and specifiers.
%% Parts = [P0, P1, P2, ...], Specs = [S1, S2, ...]
%% Result: fun(Cont) -> if no specs, Cont(P0); else fun(Arg1) -> ... end end
create_printer([SinglePart], []) ->
    %% No format specifiers — just a plain string
    fun(Cont) -> Cont(SinglePart) end;
create_printer(Parts, Specs) ->
    fun(Cont) ->
        build_curried(Parts, Specs, Cont, <<>>)
    end.

build_curried([Part], [], Cont, Acc) ->
    Cont(<<Acc/binary, Part/binary>>);
build_curried([Part | RestParts], [Spec | RestSpecs], Cont, Acc) ->
    NewAcc = <<Acc/binary, Part/binary>>,
    fun(Arg) ->
        Formatted = format_value(Spec, Arg),
        build_curried(RestParts, RestSpecs, Cont, <<NewAcc/binary, Formatted/binary>>)
    end.

%% format_value/2 — Format a single value according to a format specifier string.
format_value(Spec, Value) ->
    {Flags, Width, Prec, Type} = parse_spec_parts(Spec),
    Raw = format_raw(Type, Prec, Value),
    WithSign = apply_sign_flag(Flags, Raw, Value),
    apply_width(Flags, Width, WithSign).

%% apply_sign_flag/3 — Apply '+' or ' ' flag for numeric values.
apply_sign_flag(Flags, Raw, Value) ->
    IsNeg =
        case Raw of
            <<$-, _/binary>> -> true;
            _ -> false
        end,
    case {IsNeg, lists:member($+, Flags)} of
        {false, true} when is_number(Value) -> <<$+, Raw/binary>>;
        {false, false} ->
            case lists:member($\s, Flags) of
                true when is_number(Value) -> <<$\s, Raw/binary>>;
                _ -> Raw
            end;
        _ ->
            Raw
    end.

%% parse_spec_parts/1 — Parse "%[flags][width][.prec]type" into components.
parse_spec_parts([$% | Rest]) ->
    parse_spec_flags(Rest, []);
parse_spec_parts(Rest) ->
    parse_spec_flags(Rest, []).

parse_spec_flags([C | Rest], Flags) when C =:= $0; C =:= $-; C =:= $+; C =:= $\s ->
    parse_spec_flags(Rest, [C | Flags]);
parse_spec_flags(Rest, Flags) ->
    parse_spec_width_part(Rest, lists:reverse(Flags), 0).

parse_spec_width_part([C | Rest], Flags, W) when C >= $0, C =< $9 ->
    parse_spec_width_part(Rest, Flags, W * 10 + (C - $0));
parse_spec_width_part([$. | Rest], Flags, W) ->
    parse_spec_prec_part(Rest, Flags, W, 0);
parse_spec_width_part([Type], Flags, W) ->
    {Flags, W, -1, Type};
parse_spec_width_part([], Flags, W) ->
    {Flags, W, -1, $s}.

parse_spec_prec_part([C | Rest], Flags, W, P) when C >= $0, C =< $9 ->
    parse_spec_prec_part(Rest, Flags, W, P * 10 + (C - $0));
parse_spec_prec_part([Type], Flags, W, P) ->
    {Flags, W, P, Type};
parse_spec_prec_part([], Flags, W, P) ->
    {Flags, W, P, $s}.

%% format_raw/3 — Format a value to a binary string based on type and precision.
format_raw(Type, _Prec, Value) when Type =:= $d; Type =:= $i ->
    integer_to_binary(trunc(Value));
format_raw($s, _Prec, Value) when is_binary(Value) ->
    Value;
format_raw($s, _Prec, Value) ->
    to_string(Value);
format_raw($f, Prec, Value) ->
    P =
        if
            Prec < 0 -> 6;
            true -> Prec
        end,
    F = float(Value),
    case P of
        0 ->
            %% Erlang's io_lib:format doesn't support precision 0 for ~f
            %% So we round manually and format as integer
            integer_to_binary(erlang:round(F));
        _ ->
            iolist_to_binary(io_lib:format("~.*f", [P, F]))
    end;
format_raw($F, Prec, Value) ->
    format_raw($f, Prec, Value);
format_raw($e, Prec, Value) ->
    P =
        if
            Prec < 0 -> 6;
            true -> Prec
        end,
    iolist_to_binary(io_lib:format("~.*e", [P, float(Value)]));
format_raw($E, Prec, Value) ->
    S = format_raw($e, Prec, Value),
    string:uppercase(S);
format_raw(Type, Prec, Value) when Type =:= $g; Type =:= $G ->
    %% %g: use shortest of %e and %f (default precision 6 significant digits)
    P =
        if
            Prec < 0 -> 6;
            true -> Prec
        end,
    F = float(Value),
    Abs = abs(F),
    Threshold = math:pow(10, P),
    if
        Abs =:= +0.0 ->
            <<"0">>;
        Abs < 0.0001 ->
            S = format_raw($e, P - 1, F),
            case Type of
                $G -> string:uppercase(S);
                _ -> S
            end;
        Abs >= Threshold ->
            S = format_raw($e, P - 1, F),
            case Type of
                $G -> string:uppercase(S);
                _ -> S
            end;
        true ->
            %% Use fixed notation, trim trailing zeros
            S = format_raw($f, P, F),
            trim_trailing_zeros(S)
    end;
format_raw($x, _Prec, Value) ->
    V = trunc(Value),
    Masked = if V < 0, abs(V) > 16#FFFFFFFF -> V band 16#FFFFFFFFFFFFFFFF;
                V < 0 -> V band 16#FFFFFFFF;
                true -> V
             end,
    iolist_to_binary(io_lib:format("~.16b", [Masked]));
format_raw($X, _Prec, Value) ->
    S = format_raw($x, _Prec, Value),
    string:uppercase(S);
format_raw($o, _Prec, Value) ->
    iolist_to_binary(io_lib:format("~.8b", [trunc(Value)]));
format_raw($c, _Prec, Value) when is_integer(Value) ->
    <<Value/utf8>>;
format_raw(Type, _Prec, Value) when Type =:= $b; Type =:= $B ->
    case Value of
        true -> <<"true">>;
        false -> <<"false">>
    end;
format_raw($A, _Prec, Value) ->
    format_any(Value);
format_raw($O, _Prec, Value) ->
    to_string(Value);
format_raw(_, _Prec, Value) ->
    to_string(Value).

%% trim_trailing_zeros/1 — Remove trailing zeros after decimal point.
trim_trailing_zeros(Bin) ->
    S = binary_to_list(Bin),
    case lists:member($., S) of
        false ->
            Bin;
        true ->
            Trimmed = lists:reverse(drop_zeros(lists:reverse(S))),
            iolist_to_binary(Trimmed)
    end.

drop_zeros([$0 | Rest]) -> drop_zeros(Rest);
%% Remove trailing dot too
drop_zeros([$. | Rest]) -> Rest;
drop_zeros(Other) -> Other.

%% apply_width/3 — Apply width padding and flags.
apply_width(_Flags, 0, Str) ->
    Str;
apply_width(Flags, Width, Str) when Width > 0 ->
    Len = byte_size(Str),
    if
        Len >= Width ->
            Str;
        true ->
            Pad = Width - Len,
            ZeroPad = lists:member($0, Flags),
            case {ZeroPad, lists:member($-, Flags)} of
                {true, false} ->
                    %% Zero-padding: sign/prefix goes before zeros
                    case Str of
                        <<$+, Rest/binary>> ->
                            PadStr = list_to_binary(lists:duplicate(Pad, $0)),
                            <<$+, PadStr/binary, Rest/binary>>;
                        <<$-, Rest/binary>> ->
                            PadStr = list_to_binary(lists:duplicate(Pad, $0)),
                            <<$-, PadStr/binary, Rest/binary>>;
                        <<$\s, Rest/binary>> ->
                            PadStr = list_to_binary(lists:duplicate(Pad, $0)),
                            <<$\s, PadStr/binary, Rest/binary>>;
                        _ ->
                            PadStr = list_to_binary(lists:duplicate(Pad, $0)),
                            <<PadStr/binary, Str/binary>>
                    end;
                {_, true} ->
                    %% Left-aligned (- flag)
                    PadStr = list_to_binary(lists:duplicate(Pad, $\s)),
                    <<Str/binary, PadStr/binary>>;
                _ ->
                    %% Right-aligned with spaces
                    PadStr = list_to_binary(lists:duplicate(Pad, $\s)),
                    <<PadStr/binary, Str/binary>>
            end
    end;
apply_width(_, _, Str) ->
    Str.

%% Substring / GetStringSlice

-spec substring(binary(), non_neg_integer()) -> binary().
-spec substring(binary(), non_neg_integer(), non_neg_integer()) -> binary().
-spec get_slice(non_neg_integer() | undefined, non_neg_integer() | undefined, binary()) -> binary().

substring(Str, Start) ->
    binary:part(Str, Start, byte_size(Str) - Start).

substring(Str, Start, Length) ->
    binary:part(Str, Start, Length).

get_slice(Lower, Upper, Str) ->
    Start =
        case Lower of
            undefined -> 0;
            _ -> Lower
        end,
    End =
        case Upper of
            undefined -> byte_size(Str) - 1;
            _ -> Upper
        end,
    binary:part(Str, Start, End - Start + 1).

%% Console.WriteLine / Console.Write
%%
%% An F# string is a UTF-8 binary, so it must go out through the `t` (unicode) modifier: plain `~s`
%% writes the binary's raw bytes, which only looks right on a latin1 device that happens to
%% reassemble the UTF-8 itself, and turns into mojibake the moment the device is set to unicode.
%% `~ts` is the other half of the contract — it needs the device set to unicode, which the generated
%% `main.erl` entry shim does at startup. `printfn` already takes a `~ts` path, so before this the
%% two disagreed and no device setting satisfied both.
console_writeline(Value) when is_binary(Value) ->
    io:format(<<"~ts~n">>, [Value]);
console_writeline(Value) when is_integer(Value) ->
    io:format(<<"~B~n">>, [Value]);
console_writeline(Value) when is_float(Value) ->
    io:format(<<"~p~n">>, [Value]);
console_writeline(Value) when is_boolean(Value) ->
    io:format(<<"~ts~n">>, [atom_to_binary(Value)]);
console_writeline(Value) ->
    io:format(<<"~tp~n">>, [Value]).

console_write(Value) when is_binary(Value) ->
    io:format(<<"~ts">>, [Value]);
console_write(Value) when is_integer(Value) ->
    io:format(<<"~B">>, [Value]);
console_write(Value) when is_float(Value) ->
    io:format(<<"~p">>, [Value]);
console_write(Value) when is_boolean(Value) ->
    io:format(<<"~ts">>, [atom_to_binary(Value)]);
console_write(Value) ->
    io:format(<<"~tp">>, [Value]).

%%% ---------------------------------------------------------------------------
%%% F# structured formatting (`%A`)
%%% ---------------------------------------------------------------------------
%%
%% `%A` renders a value in F# syntax. On the other Fable targets the generated types carry a real
%% `ToString` for this (a JS record is a class instance, a Python record defines `__str__`), so the
%% formatter just calls it. Beam has nothing to call: a record is a bare map, a union a bare tagged
%% tuple, neither carrying any back-pointer to its type. Reflection cannot help either — every
%% `fable_reflection` accessor takes a type-info map, and there is none to be had from the value.
%%
%% So this reads the *shape* of the term. Several F# types share a shape on Beam, and where they do
%% the clause comments below say which way the ambiguity resolves and what that costs. The rules
%% otherwise mirror `fable-library-ts/Types.ts` (`seqToString`/`unionToString`/`recordToString`) so
%% the targets stay aligned.
%%
%% Known limitations, all of them shape ambiguities rather than bugs:
%%   * `char` is an `integer()` and prints as its codepoint. Documented in FABLE-BEAM.md.
%%   * Record field and union case names come back from lowercased Erlang atoms, so their original
%%     casing is reconstructed by convention (`my_case` -> `MyCase`) and is a guess.
%%   * Record fields print in Erlang term order (atoms sort alphabetically), not in declaration
%%     order, because a map does not remember the order its keys went in.
%%   * An F# `Set` is an ordset, i.e. a plain list, and renders as a list rather than `set [...]`.
%%   * `option` is erased, so `Some x` renders as `x` — the same collapse JS and Python have.
%%   * An F# `ref` cell is the same process-dictionary reference an array is, so `ref 5` renders as
%%     `5` and `ref [1, 2]` as `[|1; 2|]`, never as `{ contents = ... }`.
%%   * A `decimal` is a fixed-scale integer and prints as its scaled value; a `DateTime` is a
%%     `{Ticks, Kind}` tuple and prints as one. Both are in the FABLE-BEAM.md table.
%%
%% Recovering the first three needs the argument's static type threaded from the `%A` call site,
%% which is where it still exists; nothing about the runtime value can supply it.

%% Depth cap. Erlang terms are acyclic, but an array is a ref cell into the process dictionary and
%% those *can* form a cycle (`R = new_ref([]), put(R, [R])`). Falling back to `~tp` at the cap keeps
%% a pathological term terminating instead of looping.
-define(FORMAT_ANY_MAX_DEPTH, 24).

%% Element cap, matching `seqToString`'s in fable-library-ts.
-define(FORMAT_ANY_MAX_ITEMS, 100).

format_any(Value) ->
    flatten(fmt_any(Value, 0)).

%% Collapse a rendering to a binary. `unicode:characters_to_binary/1` rather than
%% `iolist_to_binary/1`: the `~tp` fallbacks below can yield codepoints above 255, which
%% `iolist_to_binary/1` rejects outright with `badarg`.
%%
%% It signals bad input by *returning* `{error, _, _}` rather than raising, and a tuple escaping
%% from here would fail somewhere far away — `format_any/1` is specced to return a `binary()`. The
%% only way to get one is a binary that is not valid UTF-8, which an F# `string` never is; falling
%% back to Erlang's own term printing keeps that hypothetical honest rather than crashing.
flatten(Rendering) ->
    case unicode:characters_to_binary(Rendering) of
        Bin when is_binary(Bin) -> Bin;
        _ -> iolist_to_binary(io_lib:format("~tp", [Rendering]))
    end.

fmt_any(Value, Depth) when Depth > ?FORMAT_ANY_MAX_DEPTH ->
    io_lib:format("~tp", [Value]);
%% Strings are quoted but *not* escaped: .NET prints `%A` of `he said "hi"` as `"he said "hi""`.
fmt_any(Value, _Depth) when is_binary(Value) ->
    [$", Value, $"];
fmt_any(Value, _Depth) when is_boolean(Value) ->
    atom_to_binary(Value);
%% `undefined` is the erased `None`.
fmt_any(undefined, _Depth) ->
    <<"None">>;
fmt_any(Value, _Depth) when is_integer(Value) ->
    integer_to_binary(Value);
fmt_any(Value, _Depth) when is_float(Value) ->
    %% `1.0` must stay `1.0` — `fable_convert:to_string/1` renders whole floats as integers, which
    %% is right for `string x` but would make `%A` lose the type.
    float_to_binary(Value, [short]);
%% A bare atom is a fieldless union case (`Empty`), the only way one reaches here.
fmt_any(Value, _Depth) when is_atom(Value) ->
    pascal_case(atom_to_binary(Value));
fmt_any(Value, Depth) when is_list(Value) ->
    [$[, fmt_items(Value, Depth, <<"; ">>), $]];
%% A `byte[]` is an atomics object behind a `{byte_array, Size, Ref}` tag. Matched ahead of the
%% general tuple clause, which would otherwise read that tag as a union case named `ByteArray`.
fmt_any({byte_array, _, _} = Value, Depth) ->
    fmt_array(fable_utils:byte_array_to_list(Value), Depth);
fmt_any(Value, Depth) when is_tuple(Value) ->
    fmt_tuple(Value, Depth);
fmt_any(Value, Depth) when is_reference(Value) ->
    fmt_ref(Value, Depth);
fmt_any(Value, Depth) when is_map(Value) ->
    fmt_map(Value, Depth);
fmt_any(Value, _Depth) when is_function(Value) ->
    <<"<fun>">>;
%% Pids, ports, anything unrecognised falls back to Erlang's own term printing — which is what `%A`
%% did for *every* value before, so this can never be worse than the old behaviour. `~tp` rather
%% than `~p` so the fallback stays unicode-aware, matching `console_write`/`console_writeline`.
fmt_any(Value, _Depth) ->
    io_lib:format("~tp", [Value]).

%% A tagged tuple is a union case; anything else is an F# tuple.
%%
%% The two are genuinely indistinguishable when a tuple's first element is itself a fieldless union
%% case: `(Empty, 1)` is `{empty, 1}`, exactly the shape of a one-field case `Empty 1`, and renders
%% as the latter. Booleans and `None` are excluded because those atoms are common as tuple heads
%% (`(true, 1)`) and are never union tags.
fmt_tuple(Value, Depth) ->
    case tuple_to_list(Value) of
        [Tag | Fields] when
            is_atom(Tag), Fields =/= [], Tag =/= true, Tag =/= false, Tag =/= undefined
        ->
            fmt_union(pascal_case(atom_to_binary(Tag)), Fields, Depth);
        Elements ->
            [$(, fmt_items(Elements, Depth, <<", ">>), $)]
    end.

%% `Named "x"` for one field, `Case (a, b)` for several. A single field is parenthesised only when
%% its own rendering contains a space, so `Circle 1.0` stays bare but `Wrapped (Named "q")` does
%% not run together — the rule `unionToString` uses in fable-library-ts.
fmt_union(Name, [Field], Depth) ->
    Rendered = flatten(fmt_any(Field, Depth + 1)),

    case binary:match(Rendered, <<" ">>) of
        nomatch -> [Name, $\s, Rendered];
        _ -> [Name, <<" (">>, Rendered, $)]
    end;
fmt_union(Name, Fields, Depth) ->
    [Name, <<" (">>, fmt_items(Fields, Depth, <<", ">>), $)].

%% An array is a ref cell into the process dictionary holding its elements. A class instance, a
%% ref-wrapped byte array and an F# `ref` cell reach here the same way, and are handed back to
%% `fmt_any` on their contents.
%%
%% Nothing distinguishes those, so a `ref` holding a list is rendered as an array and one holding a
%% scalar as the scalar — never as `{ contents = ... }`. A `ref` holding `undefined` (an erased
%% `None`) is indistinguishable from a key that was never stored, and falls through to `#Ref<...>`.
fmt_ref(Value, Depth) ->
    case get(Value) of
        Stored when is_list(Stored) ->
            fmt_array(Stored, Depth);
        undefined ->
            %% Not one of ours — a raw `make_ref()`, which prints as `#Ref<...>`.
            io_lib:format("~tp", [Value]);
        Stored ->
            fmt_any(Stored, Depth + 1)
    end.

fmt_array(Elements, Depth) ->
    [<<"[|">>, fmt_items(Elements, Depth, <<"; ">>), <<"|]">>].

%% A map is a record when every key is an atom, since record field names are compiled to atoms and
%% an F# `Map`'s keys are runtime values (binaries, integers, tuples). A `Map<SomeEnum, _>` would be
%% misread as a record, and an empty map is reported as `map []` because there is nothing to look at.
fmt_map(Value, Depth) ->
    Keys = maps:keys(Value),

    case Keys =/= [] andalso lists:all(fun erlang:is_atom/1, Keys) of
        true ->
            %% `{ Name = "bob"` / newline+2 spaces / `  Age = 7 }`, as .NET and fable-library-ts do.
            Fields = [
                [pascal_case(atom_to_binary(K)), <<" = ">>, fmt_any(maps:get(K, Value), Depth + 1)]
             || K <- Keys
            ],
            [<<"{ ">>, lists:join(<<"\n  ">>, Fields), <<" }">>];
        false ->
            Pairs = [
                [$(, fmt_any(K, Depth + 1), <<", ">>, fmt_any(maps:get(K, Value), Depth + 1), $)]
             || K <- Keys
            ],
            [<<"map [">>, lists:join(<<"; ">>, Pairs), $]]
    end.

%% Render up to ?FORMAT_ANY_MAX_ITEMS elements, then `; ...` — an unbounded `%A` of a large
%% collection is never what a failure message wants.
fmt_items(Elements, Depth, Separator) ->
    {Shown, Rest} = split_at(Elements, ?FORMAT_ANY_MAX_ITEMS, []),
    Rendered = lists:join(Separator, [fmt_any(E, Depth + 1) || E <- Shown]),

    case Rest of
        [] -> Rendered;
        _ -> [Rendered, Separator, <<"...">>]
    end.

split_at([], _N, Acc) ->
    {lists:reverse(Acc), []};
split_at(Rest, 0, Acc) ->
    {lists:reverse(Acc), Rest};
split_at([H | T], N, Acc) ->
    split_at(T, N - 1, [H | Acc]).

%% Rebuild an F# name from the Erlang atom it was compiled to: `my_case` -> `MyCase`. The compiler
%% lowercases and snake-cases on the way down and that is not injective, so this is a convention,
%% not a recovery — `ABc` comes back as `Abc`.
pascal_case(Name) ->
    Parts = binary:split(Name, <<"_">>, [global]),
    iolist_to_binary([capitalize(P) || P <- Parts, P =/= <<>>]).

capitalize(<<First/utf8, Rest/binary>>) ->
    <<(string:uppercase(<<First/utf8>>))/binary, Rest/binary>>;
capitalize(Empty) ->
    Empty.
