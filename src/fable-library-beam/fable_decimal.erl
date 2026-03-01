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
    get_minus_one/0,
    round_decimal/1,
    round_decimal/2,
    ceil_decimal/1,
    floor_decimal/1,
    truncate_decimal/1,
    pown/2
]).

-spec multiply(integer(), integer()) -> integer().
-spec divide(integer(), integer()) -> integer().
-spec divide_by_int(integer(), integer()) -> integer().
-spec to_string(integer()) -> binary().
-spec to_number(integer()) -> float().
-spec to_int(integer()) -> integer().
-spec from_parts(integer(), integer(), integer(), boolean(), non_neg_integer()) -> integer().
-spec parse(binary()) -> integer().
-spec try_parse(binary(), reference()) -> boolean().
-spec get_one() -> integer().
-spec get_minus_one() -> integer().
-spec round_decimal(integer()) -> integer().
-spec round_decimal(integer(), non_neg_integer()) -> integer().
-spec ceil_decimal(integer()) -> integer().
-spec floor_decimal(integer()) -> integer().
-spec truncate_decimal(integer()) -> integer().
-spec pown(integer(), integer()) -> integer().

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

pow10(0) ->
    1;
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

%% Round decimal to nearest integer (banker's rounding / round half to even).
%% Returns a fixed-scale decimal (integer part × SCALE28).
round_decimal(X) ->
    round_decimal(X, 0).

%% Round decimal to N decimal digits (0..28).
round_decimal(X, Digits) when Digits >= 0, Digits =< 28 ->
    %% Factor = 10^(28 - Digits), the scale unit for this precision
    Factor = pow10(28 - Digits),
    %% Split into units and remainder
    Quotient = X div Factor,
    Remainder = abs(X rem Factor),
    Half = Factor div 2,
    if
        Remainder > Half ->
            %% Round away from zero
            RoundedQ = if X >= 0 -> Quotient + 1; true -> Quotient - 1 end,
            RoundedQ * Factor;
        Remainder < Half ->
            %% Round toward zero
            Quotient * Factor;
        true ->
            %% Exactly half: round to even (banker's rounding)
            case Quotient rem 2 of
                0 -> Quotient * Factor;
                _ ->
                    RoundedQ2 = if X >= 0 -> Quotient + 1; true -> Quotient - 1 end,
                    RoundedQ2 * Factor
            end
    end.

%% Ceiling: smallest integer >= X, returned as fixed-scale decimal.
ceil_decimal(X) ->
    IntPart = (X div ?SCALE28) * ?SCALE28,
    Frac = X - IntPart,
    if
        Frac > 0 -> IntPart + ?SCALE28;
        true -> IntPart
    end.

%% Floor: largest integer <= X, returned as fixed-scale decimal.
floor_decimal(X) ->
    IntPart = (X div ?SCALE28) * ?SCALE28,
    Frac = X - IntPart,
    if
        Frac < 0 -> IntPart - ?SCALE28;
        true -> IntPart
    end.

%% Truncate: remove fractional part (round toward zero), returned as fixed-scale decimal.
truncate_decimal(X) ->
    (X div ?SCALE28) * ?SCALE28.

%% Power: raise a fixed-scale decimal to an integer exponent.
pown(_, 0) ->
    ?SCALE28; % 1.0M
pown(Base, 1) ->
    Base;
pown(Base, N) when N > 0 ->
    %% Multiply and rescale at each step
    Half = pown(Base, N div 2),
    Sq = multiply(Half, Half),
    case N rem 2 of
        0 -> Sq;
        _ -> multiply(Sq, Base)
    end;
pown(Base, N) when N < 0 ->
    %% x^(-n) = 1/x^n
    divide(?SCALE28, pown(Base, -N)).
