-module(fable_decimal).

-export([
    multiply/2,
    divide/2,
    divide_by_int/2,
    to_string/1,
    to_number/1,
    to_int/1,
    from_parts/5,
    parse/1,
    try_parse/2,
    get_one/0,
    get_minus_one/0
]).

%% Fixed-scale decimal representation: value × 10^28
%% All decimal values are plain Erlang integers scaled by 10^28.
%% Addition, subtraction, remainder, abs, sign, comparisons all work natively.

-define(SCALE28, 10000000000000000000000000000).

%% Multiply two fixed-scale decimals: (A * B) div Scale28
multiply(A, B) ->
    (A * B) div ?SCALE28.

%% Divide two fixed-scale decimals: (A * Scale28) div B
divide(_A, 0) ->
    erlang:error(badarith);
divide(A, B) ->
    (A * ?SCALE28) div B.

%% DivideByInt: decimal / int (int is NOT scaled)
divide_by_int(_A, 0) ->
    erlang:error(badarith);
divide_by_int(A, B) ->
    A div B.

%% Convert fixed-scale integer to string
to_string(0) ->
    <<"0">>;
to_string(X) when X < 0 ->
    Inner = to_string_positive(-X),
    <<"-", Inner/binary>>;
to_string(X) ->
    to_string_positive(X).

to_string_positive(X) ->
    %% Split into integer and fractional parts
    IntPart = X div ?SCALE28,
    FracPart = X rem ?SCALE28,
    IntBin = integer_to_binary(IntPart),
    case FracPart of
        0 ->
            IntBin;
        _ ->
            %% Pad fractional part to 28 digits, then strip trailing zeros
            FracStr = integer_to_binary(FracPart),
            Padded = pad_left(FracStr, 28),
            Stripped = strip_trailing_zeros(Padded),
            <<IntBin/binary, ".", Stripped/binary>>
    end.

pad_left(Bin, Len) when byte_size(Bin) >= Len ->
    Bin;
pad_left(Bin, Len) ->
    Padding = Len - byte_size(Bin),
    Zeros = binary:copy(<<"0">>, Padding),
    <<Zeros/binary, Bin/binary>>.

strip_trailing_zeros(Bin) ->
    strip_trailing_zeros(Bin, byte_size(Bin)).

strip_trailing_zeros(Bin, 0) ->
    Bin;
strip_trailing_zeros(Bin, Len) ->
    case binary:at(Bin, Len - 1) of
        $0 -> strip_trailing_zeros(Bin, Len - 1);
        _ -> binary:part(Bin, 0, Len)
    end.

%% Convert fixed-scale decimal to float
to_number(X) ->
    X / ?SCALE28.

%% Convert fixed-scale decimal to integer (truncate)
to_int(X) ->
    X div ?SCALE28.

%% MakeDecimal(low, mid, high, isNegative, scale)
%% low/mid/high are signed int32 from .NET — mask to unsigned
from_parts(Low, Mid, High, IsNegative, Scale) ->
    L = Low band 16#FFFFFFFF,
    M = Mid band 16#FFFFFFFF,
    H = High band 16#FFFFFFFF,
    Coefficient = L bor (M bsl 32) bor (H bsl 64),
    %% Scale to 28 decimal places
    Pow = pow10(28 - Scale),
    Adjusted = Coefficient * Pow,
    case IsNegative of
        true -> -Adjusted;
        false -> Adjusted
    end.

pow10(0) -> 1;
pow10(N) when N > 0 -> 10 * pow10(N - 1);
pow10(N) when N < 0 ->
    %% Should not happen with valid .NET decimals (scale 0..28)
    erlang:error({invalid_scale, N}).

%% Parse a decimal string to fixed-scale integer
parse(Bin) when is_binary(Bin) ->
    S = binary_to_list(string:trim(Bin)),
    parse_string(S).

parse_string([$- | Rest]) ->
    -parse_unsigned(Rest);
parse_string([$+ | Rest]) ->
    parse_unsigned(Rest);
parse_string(S) ->
    parse_unsigned(S).

parse_unsigned(S) ->
    case lists:splitwith(fun(C) -> C >= $0 andalso C =< $9 end, S) of
        {IntDigits, [$. | FracDigits]} ->
            IntVal = list_to_integer(IntDigits),
            FracLen = length(FracDigits),
            FracVal = list_to_integer(FracDigits),
            IntVal * ?SCALE28 + FracVal * pow10(28 - FracLen);
        {IntDigits, []} ->
            list_to_integer(IntDigits) * ?SCALE28;
        _ ->
            erlang:error(badarg)
    end.

%% TryParse: returns true/false, sets out-param via process dictionary
try_parse(Bin, OutRef) ->
    try
        Val = parse(Bin),
        erlang:put(OutRef, Val),
        true
    catch
        _:_ -> false
    end.

get_one() ->
    ?SCALE28.

get_minus_one() ->
    -?SCALE28.
