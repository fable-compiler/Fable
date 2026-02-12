-module(fable_convert).
-export([to_float/1, to_int/1, to_int_with_base/2, to_string/1, to_string_with_base/3,
         to_base64/1, from_base64/1, boolean_parse/1, boolean_try_parse/2,
         int_to_string_with_format/2,
         try_parse_int/2, try_parse_float/2]).

%% Robust string-to-float conversion that handles edge cases
%% Erlang's binary_to_float/1 is strict and rejects formats like "1." or "1"
%% .NET accepts these, so we normalize before converting.
to_float(Bin) when is_binary(Bin) ->
    case binary_to_list(Bin) of
        [] -> erlang:error(badarg);
        Str ->
            case string:to_float(Str) of
                {Float, []} -> Float;
                {_Float, _Rest} ->
                    %% Partial parse - try as integer
                    try_as_integer(Str);
                {error, no_float} ->
                    %% No float found - try as integer
                    try_as_integer(Str)
            end
    end;
to_float(N) when is_integer(N) -> float(N);
to_float(F) when is_float(F) -> F.

try_as_integer(Str) ->
    case string:to_integer(Str) of
        {Int, []} -> float(Int);
        {Int, "."} -> float(Int);  %% Handle trailing dot like "1."
        _ -> erlang:error(badarg)
    end.

%% Parse string to integer, handling 0x/0o/0b prefixes
to_int(Bin) when is_binary(Bin) ->
    case Bin of
        <<"0x", Rest/binary>> -> binary_to_integer(Rest, 16);
        <<"0X", Rest/binary>> -> binary_to_integer(Rest, 16);
        <<"0o", Rest/binary>> -> binary_to_integer(Rest, 8);
        <<"0O", Rest/binary>> -> binary_to_integer(Rest, 8);
        <<"0b", Rest/binary>> -> binary_to_integer(Rest, 2);
        <<"0B", Rest/binary>> -> binary_to_integer(Rest, 2);
        _ -> binary_to_integer(Bin)
    end;
to_int(N) when is_integer(N) -> N;
to_int(F) when is_float(F) -> trunc(F).

%% Parse string to integer with given base (2, 8, 10, 16)
to_int_with_base(Bin, Base) when is_binary(Bin), is_integer(Base) ->
    binary_to_integer(Bin, Base).

%% Convert integer to string with given base and bit width
%% BitWidth: 8 (SByte), 16 (Int16), 32 (Int32), 64 (Int64)
%% For negative numbers with non-decimal bases, .NET uses two's complement
%% .NET always produces lowercase hex digits
to_string_with_base(N, Base, _BitWidth) when is_integer(N), N >= 0 ->
    string:lowercase(integer_to_binary(N, Base));
to_string_with_base(N, 10, _BitWidth) when is_integer(N) ->
    integer_to_binary(N);
to_string_with_base(N, Base, BitWidth) when is_integer(N), N < 0 ->
    Mask = (1 bsl BitWidth) - 1,
    string:lowercase(integer_to_binary(N band Mask, Base)).

%% Generic ToString - handles runtime type dispatch for obj.ToString()
%% Unlike ~p which wraps binaries in <<"...">> notation, this returns
%% the string as-is when the value is already a binary.
to_string(Value) when is_binary(Value) -> Value;
to_string(Value) when is_integer(Value) -> integer_to_binary(Value);
to_string(Value) when is_float(Value) -> float_to_binary(Value);
to_string(Value) when is_atom(Value) -> atom_to_binary(Value);
to_string(Value) when is_list(Value) ->
    %% Lists could be charlists or regular lists
    try list_to_binary(Value)
    catch _:_ -> iolist_to_binary(io_lib:format("~p", [Value]))
    end;
to_string(Value) ->
    iolist_to_binary(io_lib:format("~p", [Value])).

%% Base64 encoding/decoding
%% .NET's Convert.ToBase64String takes byte[] and returns string
%% .NET's Convert.FromBase64String takes string and returns byte[]
to_base64(Bytes) when is_list(Bytes) ->
    base64:encode(list_to_binary(Bytes));
to_base64(Bin) when is_binary(Bin) ->
    base64:encode(Bin).

from_base64(Str) when is_binary(Str) ->
    base64:decode(Str).

%% TryParse: returns bool and sets out-param via put(OutRef, Value).
%% F# uses out-parameter pattern: result = TryParse(str, &outRef).
try_parse_int(Bin, OutRef) when is_binary(Bin) ->
    Trimmed = string:trim(binary_to_list(Bin)),
    case string:to_integer(Trimmed) of
        {Int, []} -> put(OutRef, Int), true;
        _ -> false
    end;
try_parse_int(_, _) -> false.

try_parse_float(Bin, OutRef) when is_binary(Bin) ->
    Trimmed = string:trim(binary_to_list(Bin)),
    case string:to_float(Trimmed) of
        {Float, []} -> put(OutRef, Float), true;
        _ ->
            case string:to_integer(Trimmed) of
                {Int, []} -> put(OutRef, float(Int)), true;
                _ -> false
            end
    end;
try_parse_float(_, _) -> false.

%% Boolean.Parse - case insensitive, trims whitespace
boolean_parse(Bin) when is_binary(Bin) ->
    Trimmed = string:trim(binary_to_list(Bin)),
    Lower = string:lowercase(Trimmed),
    case Lower of
        "true" -> true;
        "false" -> false;
        _ -> erlang:error({badarg, Bin})
    end.

%% Boolean.TryParse - sets out-param via process dictionary, returns success bool
boolean_try_parse(Bin, OutRef) when is_binary(Bin) ->
    Trimmed = string:trim(binary_to_list(Bin)),
    Lower = string:lowercase(Trimmed),
    case Lower of
        "true" -> put(OutRef, true), true;
        "false" -> put(OutRef, false), true;
        _ -> put(OutRef, false), false
    end.

%% Int32/Int64 ToString with format specifier
%% Supports: "d", "d<N>" (decimal with padding), "x", "x<N>" (hex with padding)
int_to_string_with_format(Value, Fmt) when is_binary(Fmt) ->
    FmtStr = string:lowercase(binary_to_list(Fmt)),
    case FmtStr of
        [$d] ->
            integer_to_binary(Value);
        [$d | WidthStr] ->
            Width = list_to_integer(WidthStr),
            S = integer_to_list(Value),
            Len = length(S),
            if Len >= Width -> list_to_binary(S);
               true -> list_to_binary(string:pad(S, Width, leading, $0))
            end;
        [$x] ->
            list_to_binary(string:lowercase(integer_to_list(Value, 16)));
        [$x | WidthStr] ->
            Width = list_to_integer(WidthStr),
            S = string:lowercase(integer_to_list(Value, 16)),
            Len = length(S),
            if Len >= Width -> list_to_binary(S);
               true -> list_to_binary(string:pad(S, Width, leading, $0))
            end;
        _ ->
            integer_to_binary(Value)
    end.
