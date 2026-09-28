-module(fable_convert).
-export([
    to_float/1,
    to_int/1,
    to_int/3,
    to_int_with_base/2,
    to_int_with_base/4,
    to_string/1,
    to_string_with_base/3,
    to_base64/1,
    from_base64/1,
    boolean_parse/1,
    boolean_try_parse/2,
    int_to_string_with_format/2,
    try_parse_int/2,
    try_parse_int/4,
    try_parse_float/2
]).

-spec to_float(binary() | integer() | float()) -> float().
-spec to_int(binary() | integer() | float()) -> integer().
-spec to_int(binary() | integer() | float(), pos_integer(), boolean()) -> integer().
-spec to_int_with_base(binary(), integer()) -> integer().
-spec to_int_with_base(binary(), integer(), pos_integer(), boolean()) -> integer().
-spec to_string(term()) -> binary().
-spec to_string_with_base(integer(), integer(), integer()) -> binary().
-spec to_base64(tuple() | list() | binary()) -> binary().
-spec from_base64(binary()) -> tuple().
-spec boolean_parse(binary()) -> boolean().
-spec boolean_try_parse(binary(), reference()) -> boolean().
-spec int_to_string_with_format(integer(), binary()) -> binary().
-spec try_parse_int(binary(), reference()) -> boolean().
-spec try_parse_int(binary(), reference(), pos_integer(), boolean()) -> boolean().
-spec try_parse_float(binary(), reference()) -> boolean().

%% Robust string-to-float conversion that handles edge cases
%% Erlang's binary_to_float/1 is strict and rejects formats like "1." or "1"
%% .NET accepts these, so we normalize before converting.
to_float(Bin) when is_binary(Bin) ->
    case binary_to_list(Bin) of
        [] ->
            erlang:error(badarg);
        Str ->
            case string:to_float(Str) of
                {Float, []} ->
                    Float;
                _ ->
                    %% Partial parse or no float found - try as integer
                    try_as_integer(Str)
            end
    end;
to_float(N) when is_integer(N) -> float(N);
to_float(F) when is_float(F) -> F.

try_as_integer(Str) ->
    case string:to_integer(Str) of
        {Int, []} -> float(Int);
        %% Handle trailing dot like "1."
        {Int, "."} -> float(Int);
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

to_int(Bin, Bits, Signed) when is_binary(Bin) ->
    case Bin of
        <<"0x", Rest/binary>> -> to_int_with_base(Rest, 16, Bits, Signed);
        <<"0X", Rest/binary>> -> to_int_with_base(Rest, 16, Bits, Signed);
        <<"0o", Rest/binary>> -> to_int_with_base(Rest, 8, Bits, Signed);
        <<"0O", Rest/binary>> -> to_int_with_base(Rest, 8, Bits, Signed);
        <<"0b", Rest/binary>> -> to_int_with_base(Rest, 2, Bits, Signed);
        <<"0B", Rest/binary>> -> to_int_with_base(Rest, 2, Bits, Signed);
        _ -> ensure_integer_range(binary_to_integer(Bin), Bits, Signed)
    end;
to_int(N, Bits, Signed) when is_integer(N) -> ensure_integer_range(N, Bits, Signed);
to_int(F, Bits, Signed) when is_float(F) -> ensure_integer_range(trunc(F), Bits, Signed).

%% Parse string to integer with given base (2, 8, 10, 16)
to_int_with_base(Bin, Base) when is_binary(Bin), is_integer(Base) ->
    binary_to_integer(Bin, Base).

to_int_with_base(Bin, Base, Bits, Signed) when is_binary(Bin), is_integer(Base) ->
    checked_base_integer(binary_to_integer(Bin, Base), Base, Bits, Signed).

%% Non-decimal signed parsing accepts the target width's two's-complement form.
%% invariant: checked integer parsing never returns a value outside the selected target width.
checked_base_integer(N, Base, Bits, true) when Base =/= 10 ->
    SignBit = 1 bsl (Bits - 1),
    Modulus = 1 bsl Bits,
    case N >= SignBit andalso N < Modulus of
        true -> N - Modulus;
        false -> ensure_integer_range(N, Bits, true)
    end;
checked_base_integer(N, _Base, Bits, Signed) ->
    ensure_integer_range(N, Bits, Signed).

ensure_integer_range(N, Bits, Signed) ->
    case integer_in_range(N, Bits, Signed) of
        true -> N;
        false ->
            erlang:error(#{
                exn_type => overflow_exception,
                message => <<"Value was either too large or too small for an integer type.">>
            })
    end.

integer_in_range(N, Bits, true) ->
    Limit = 1 bsl (Bits - 1),
    N >= -Limit andalso N < Limit;
integer_in_range(N, Bits, false) ->
    N >= 0 andalso N < (1 bsl Bits).

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
to_string(Value) when is_float(Value) ->
    case Value == trunc(Value) of
        true -> integer_to_binary(trunc(Value));
        false -> float_to_binary(Value, [{decimals, 10}, compact])
    end;
to_string(Value) when is_atom(Value) -> atom_to_binary(Value);
to_string(Value) when is_list(Value) ->
    %% Lists could be charlists or regular lists
    try
        list_to_binary(Value)
    catch
        _:_ -> iolist_to_binary(io_lib:format("~p", [Value]))
    end;
to_string(Value) ->
    iolist_to_binary(io_lib:format("~p", [Value])).

%% Base64 encoding/decoding
%% .NET's Convert.ToBase64String takes byte[] and returns string
%% .NET's Convert.FromBase64String takes string and returns byte[]
to_base64({byte_array, _, _} = BA) ->
    base64:encode(list_to_binary(fable_utils:byte_array_to_list(BA)));
to_base64(Bytes) when is_list(Bytes) ->
    base64:encode(list_to_binary(Bytes));
to_base64(Bin) when is_binary(Bin) ->
    base64:encode(Bin).

from_base64(Str) when is_binary(Str) ->
    fable_utils:new_byte_array(binary_to_list(base64:decode(Str))).

%% TryParse: returns bool and sets out-param via put(OutRef, Value).
%% F# uses out-parameter pattern: result = TryParse(str, &outRef).
try_parse_int(Bin, OutRef) when is_binary(Bin) ->
    Trimmed = string:trim(binary_to_list(Bin)),
    case string:to_integer(Trimmed) of
        {Int, []} ->
            put(OutRef, Int),
            true;
        _ ->
            false
    end;
try_parse_int(_, _) ->
    false.

try_parse_int(Bin, OutRef, Bits, Signed) when is_binary(Bin) ->
    Trimmed = string:trim(binary_to_list(Bin)),
    case string:to_integer(Trimmed) of
        {Int, []} ->
            case integer_in_range(Int, Bits, Signed) of
                true ->
                    put(OutRef, Int),
                    true;
                false ->
                    false
            end;
        _ ->
            false
    end;
try_parse_int(_, _, _, _) ->
    false.

try_parse_float(Bin, OutRef) when is_binary(Bin) ->
    Trimmed = string:trim(binary_to_list(Bin)),
    case string:to_float(Trimmed) of
        {Float, []} ->
            put(OutRef, Float),
            true;
        _ ->
            case string:to_integer(Trimmed) of
                {Int, []} ->
                    put(OutRef, float(Int)),
                    true;
                _ ->
                    false
            end
    end;
try_parse_float(_, _) ->
    false.

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
        "true" ->
            put(OutRef, true),
            true;
        "false" ->
            put(OutRef, false),
            true;
        _ ->
            put(OutRef, false),
            false
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
            if
                Len >= Width -> list_to_binary(S);
                true -> list_to_binary(string:pad(S, Width, leading, $0))
            end;
        [$x] ->
            list_to_binary(string:lowercase(integer_to_list(Value, 16)));
        [$x | WidthStr] ->
            Width = list_to_integer(WidthStr),
            S = string:lowercase(integer_to_list(Value, 16)),
            Len = length(S),
            if
                Len >= Width -> list_to_binary(S);
                true -> list_to_binary(string:pad(S, Width, leading, $0))
            end;
        _ ->
            integer_to_binary(Value)
    end.
