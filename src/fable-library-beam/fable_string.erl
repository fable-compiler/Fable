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
         split/2, split/3, split_remove_empty/2, split_with_count/3,
         to_string/1,
         printf/1,
         to_text/1, to_text/2, to_text/3, to_text/4, to_text/5,
         to_console/1, to_console/2, to_console/3, to_console/4, to_console/5,
         to_console_error/1, to_console_error/2, to_console_error/3,
         to_fail/1, to_fail/2, to_fail/3,
         interpolate/2, format/2,
         console_writeline/1, console_write/1]).

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

%% Generic value-to-string conversion for string interpolation.
%% Handles all Erlang types, outputting human-readable strings (not Erlang term format).
to_string(V) when is_binary(V) -> V;
to_string(V) when is_integer(V) -> integer_to_binary(V);
to_string(V) when is_float(V) ->
    %% Match Erlang float_to_binary default but trim trailing zeros
    S = float_to_binary(V, [{decimals, 10}, compact]),
    %% Ensure at least one decimal: "42" -> "42.0"
    case binary:match(S, <<".">>) of
        nomatch -> <<S/binary, ".0">>;
        _ -> S
    end;
to_string(V) when is_atom(V) -> atom_to_binary(V, utf8);
to_string(V) when is_boolean(V) ->
    case V of true -> <<"true">>; false -> <<"false">> end;
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
    Cont(fun(X) -> io:format("~ts~n", [X]), ok end);
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
    Cont(fun(X) -> io:format(standard_error, "~ts~n", [X]), ok end);
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
interpolate(Str, Values) ->
    ValList = case is_list(Values) of true -> Values; false -> [Values] end,
    interpolate_loop(Str, ValList, []).

interpolate_loop(<<>>, _Values, Acc) ->
    iolist_to_binary(lists:reverse(Acc));
interpolate_loop(<<"%P()", Rest/binary>>, [V | Vs], Acc) ->
    interpolate_loop(Rest, Vs, [to_string(V) | Acc]);
interpolate_loop(<<"%", Spec:1/binary, "%P()", Rest/binary>>, [V | Vs], Acc) ->
    %% Handle format specifier before %P() like %s%P() or %i%P()
    Formatted = format_value(binary_to_list(Spec), V),
    interpolate_loop(Rest, Vs, [Formatted | Acc]);
interpolate_loop(<<C/utf8, Rest/binary>>, Values, Acc) ->
    interpolate_loop(Rest, Values, [<<C/utf8>> | Acc]).

%% format/2 — .NET String.Format("{0} {1}", [Arg0, Arg1]).
format(FmtStr, Args) ->
    ArgList = case is_list(Args) of true -> Args; false -> [Args] end,
    format_dotnet(FmtStr, ArgList, []).

format_dotnet(<<>>, _Args, Acc) ->
    iolist_to_binary(lists:reverse(Acc));
format_dotnet(<<"{{", Rest/binary>>, Args, Acc) ->
    format_dotnet(Rest, Args, [<<"{" >> | Acc]);
format_dotnet(<<"}}", Rest/binary>>, Args, Acc) ->
    format_dotnet(Rest, Args, [<<"}">> | Acc]);
format_dotnet(<<"{", Rest/binary>>, Args, Acc) ->
    {IdxStr, Rest2} = parse_format_index(Rest, []),
    Idx = list_to_integer(IdxStr),
    Val = lists:nth(Idx + 1, Args),
    format_dotnet(Rest2, Args, [to_string(Val) | Acc]);
format_dotnet(<<C/utf8, Rest/binary>>, Args, Acc) ->
    format_dotnet(Rest, Args, [<<C/utf8>> | Acc]).

parse_format_index(<<"}", Rest/binary>>, Acc) ->
    {lists:reverse(Acc), Rest};
parse_format_index(<<":", Rest/binary>>, Acc) ->
    %% Skip format specifier after colon (e.g., {0:N5}) until closing }
    skip_until_close(Rest, lists:reverse(Acc));
parse_format_index(<<C, Rest/binary>>, Acc) ->
    parse_format_index(Rest, [C | Acc]).

skip_until_close(<<"}", Rest/binary>>, IdxStr) ->
    {IdxStr, Rest};
skip_until_close(<<_, Rest/binary>>, IdxStr) ->
    skip_until_close(Rest, IdxStr).

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
parse_spec(<<C, Rest/binary>>, Acc) when C =:= $0; C =:= $-; C =:= $+; C =:= $  ->
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
    IsNeg = case Raw of <<$-, _/binary>> -> true; _ -> false end,
    case {IsNeg, lists:member($+, Flags)} of
        {false, true} when is_number(Value) -> <<$+, Raw/binary>>;
        {false, false} ->
            case lists:member($\s, Flags) of
                true when is_number(Value) -> <<$\s, Raw/binary>>;
                _ -> Raw
            end;
        _ -> Raw
    end.

%% parse_spec_parts/1 — Parse "%[flags][width][.prec]type" into components.
parse_spec_parts([$% | Rest]) ->
    parse_spec_flags(Rest, []);
parse_spec_parts(Rest) ->
    parse_spec_flags(Rest, []).

parse_spec_flags([C | Rest], Flags) when C =:= $0; C =:= $-; C =:= $+; C =:= $  ->
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
    P = if Prec < 0 -> 6; true -> Prec end,
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
    P = if Prec < 0 -> 6; true -> Prec end,
    iolist_to_binary(io_lib:format("~.*e", [P, float(Value)]));
format_raw($E, Prec, Value) ->
    S = format_raw($e, Prec, Value),
    string:uppercase(S);
format_raw(Type, Prec, Value) when Type =:= $g; Type =:= $G ->
    %% %g: use shortest of %e and %f (default precision 6 significant digits)
    P = if Prec < 0 -> 6; true -> Prec end,
    F = float(Value),
    Abs = abs(F),
    Threshold = math:pow(10, P),
    if
        Abs =:= +0.0 ->
            <<"0">>;
        Abs < 0.0001 ->
            S = format_raw($e, P - 1, F),
            case Type of $G -> string:uppercase(S); _ -> S end;
        Abs >= Threshold ->
            S = format_raw($e, P - 1, F),
            case Type of $G -> string:uppercase(S); _ -> S end;
        true ->
            %% Use fixed notation, trim trailing zeros
            S = format_raw($f, P, F),
            trim_trailing_zeros(S)
    end;
format_raw($x, _Prec, Value) ->
    iolist_to_binary(io_lib:format("~.16b", [trunc(Value)]));
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
    iolist_to_binary(io_lib:format("~p", [Value]));
format_raw($O, _Prec, Value) ->
    to_string(Value);
format_raw(_, _Prec, Value) ->
    to_string(Value).

%% trim_trailing_zeros/1 — Remove trailing zeros after decimal point.
trim_trailing_zeros(Bin) ->
    S = binary_to_list(Bin),
    case lists:member($., S) of
        false -> Bin;
        true ->
            Trimmed = lists:reverse(drop_zeros(lists:reverse(S))),
            iolist_to_binary(Trimmed)
    end.

drop_zeros([$0 | Rest]) -> drop_zeros(Rest);
drop_zeros([$. | Rest]) -> Rest; %% Remove trailing dot too
drop_zeros(Other) -> Other.

%% apply_width/3 — Apply width padding and flags.
apply_width(_Flags, 0, Str) -> Str;
apply_width(Flags, Width, Str) when Width > 0 ->
    Len = byte_size(Str),
    if
        Len >= Width -> Str;
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
apply_width(_, _, Str) -> Str.

%% Console.WriteLine / Console.Write
console_writeline(Value) when is_binary(Value) ->
    io:format(<<"~s~n">>, [Value]);
console_writeline(Value) when is_integer(Value) ->
    io:format(<<"~B~n">>, [Value]);
console_writeline(Value) when is_float(Value) ->
    io:format(<<"~p~n">>, [Value]);
console_writeline(Value) when is_boolean(Value) ->
    io:format(<<"~s~n">>, [atom_to_binary(Value)]);
console_writeline(Value) ->
    io:format(<<"~p~n">>, [Value]).

console_write(Value) when is_binary(Value) ->
    io:format(<<"~s">>, [Value]);
console_write(Value) when is_integer(Value) ->
    io:format(<<"~B">>, [Value]);
console_write(Value) when is_float(Value) ->
    io:format(<<"~p">>, [Value]);
console_write(Value) when is_boolean(Value) ->
    io:format(<<"~s">>, [atom_to_binary(Value)]);
console_write(Value) ->
    io:format(<<"~p">>, [Value]).
