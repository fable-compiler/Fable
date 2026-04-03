-module(fable_time_only).
-export([
    create/3, create/4,
    from_ticks/1,
    from_time_span/1,
    from_date_time/1,
    min_value/0,
    max_value/0,
    hour/1,
    minute/1,
    second/1,
    millisecond/1,
    ticks/1,
    add/2,
    add_hours/2,
    add_minutes/2,
    is_between/3,
    to_time_span/1,
    to_string/1, to_string/2, to_string/3,
    parse/1,
    try_parse/2,
    op_subtraction/2
]).

-type time_only() :: integer().

-spec create(integer(), integer(), integer()) -> time_only().
-spec create(integer(), integer(), integer(), integer()) -> time_only().
-spec from_ticks(integer()) -> time_only().
-spec from_time_span(integer()) -> time_only().
-spec from_date_time({integer(), integer()}) -> time_only().
-spec min_value() -> time_only().
-spec max_value() -> time_only().
-spec hour(time_only()) -> integer().
-spec minute(time_only()) -> integer().
-spec second(time_only()) -> integer().
-spec millisecond(time_only()) -> integer().
-spec ticks(time_only()) -> integer().
-spec add(time_only(), integer()) -> time_only().
-spec add_hours(time_only(), number()) -> time_only().
-spec add_minutes(time_only(), number()) -> time_only().
-spec is_between(time_only(), time_only(), time_only()) -> boolean().
-spec to_time_span(time_only()) -> integer().
-spec to_string(time_only()) -> binary().
-spec to_string(time_only(), binary()) -> binary().
-spec to_string(time_only(), binary(), term()) -> binary().
-spec parse(binary()) -> time_only().
-spec try_parse(binary(), reference()) -> boolean().
-spec op_subtraction(time_only(), time_only()) -> time_only().

%% TimeOnly is represented as ticks since midnight (integer),
%% same unit as TimeSpan.

-define(TICKS_PER_MILLISECOND, 10000).
-define(TICKS_PER_SECOND, 10000000).
-define(TICKS_PER_MINUTE, 600000000).
-define(TICKS_PER_HOUR, 36000000000).
-define(TICKS_PER_DAY, 864000000000).

%% ============================================================
%% Constructors
%% ============================================================

create(H, M, S) ->
    create(H, M, S, 0).

create(H, M, S, Ms) ->
    H * ?TICKS_PER_HOUR + M * ?TICKS_PER_MINUTE +
        S * ?TICKS_PER_SECOND + Ms * ?TICKS_PER_MILLISECOND.

from_ticks(T) -> T.

from_time_span(TS) ->
    case TS >= 0 andalso TS < ?TICKS_PER_DAY of
        true -> TS;
        false -> erlang:error(<<"The TimeSpan describes an unrepresentable TimeOnly.">>)
    end.

from_date_time({Ticks, _Kind}) ->
    %% Extract time-of-day ticks
    Ticks rem ?TICKS_PER_DAY.

%% ============================================================
%% Static fields
%% ============================================================

min_value() -> 0.

max_value() -> ?TICKS_PER_DAY - 1.

%% ============================================================
%% Properties
%% ============================================================

hour(T) -> T div ?TICKS_PER_HOUR.

minute(T) -> (T rem ?TICKS_PER_HOUR) div ?TICKS_PER_MINUTE.

second(T) -> (T rem ?TICKS_PER_MINUTE) div ?TICKS_PER_SECOND.

millisecond(T) -> (T rem ?TICKS_PER_SECOND) div ?TICKS_PER_MILLISECOND.

ticks(T) -> T.

%% ============================================================
%% Arithmetic
%% ============================================================

add(T, TS) ->
    T2 = (T + TS) rem ?TICKS_PER_DAY,
    case T2 >= 0 of
        true -> T2;
        false -> ?TICKS_PER_DAY + T2
    end.

add_hours(T, H) ->
    add(T, trunc(H * ?TICKS_PER_HOUR)).

add_minutes(T, M) ->
    add(T, trunc(M * ?TICKS_PER_MINUTE)).

%% ============================================================
%% Queries
%% ============================================================

is_between(T, Start, End) ->
    case Start =< End of
        true -> Start =< T andalso End > T;
        false -> Start =< T orelse End > T
    end.

%% ============================================================
%% Conversion
%% ============================================================

to_time_span(T) -> T.

%% ============================================================
%% ToString
%% ============================================================

to_string(T) -> to_string(T, <<"t">>).

to_string(T, Format) -> to_string(T, Format, undefined).

to_string(T, Format, _Provider) ->
    H = pad(hour(T), 2),
    M = pad(minute(T), 2),
    Base = iolist_to_binary([H, <<":">>, M]),
    case Format of
        <<"t">> -> Base;
        <<"o">> -> long_format(T, Base);
        <<"O">> -> long_format(T, Base);
        _ ->
            S = pad(second(T), 2),
            iolist_to_binary([Base, <<":">>, S])
    end.

long_format(T, Base) ->
    S = pad(second(T), 2),
    Ms = pad(millisecond(T), 3),
    iolist_to_binary([Base, <<":">>, S, <<".">>, Ms, <<"0000">>]).

pad(N, Width) ->
    S = integer_to_binary(N),
    Len = byte_size(S),
    case Len >= Width of
        true -> S;
        false -> iolist_to_binary([binary:copy(<<"0">>, Width - Len), S])
    end.

%% ============================================================
%% Parse / TryParse
%% ============================================================

parse(Str) ->
    %% Parse "HH:MM" or "HH:MM:SS" or "HH:MM:SS.fff"
    case parse_time(Str) of
        {ok, T} -> T;
        error -> erlang:error(iolist_to_binary([<<"String '">>, Str, <<"' was not recognized as a valid TimeOnly.">>]))
    end.

try_parse(Str, OutRef) ->
    case parse_time(Str) of
        {ok, T} ->
            put(OutRef, T),
            true;
        error ->
            false
    end.

parse_time(Str) ->
    %% Simple parser for HH:MM[:SS[.fff...]]
    try
        Parts = binary:split(Str, <<":">>, [global]),
        case Parts of
            [HB, MB] ->
                {ok, create(binary_to_integer(HB), binary_to_integer(MB), 0, 0)};
            [HB, MB, Rest] ->
                {SecB, Ms} = parse_seconds_frac(Rest),
                {ok, create(binary_to_integer(HB), binary_to_integer(MB),
                            binary_to_integer(SecB), Ms)};
            _ -> error
        end
    catch
        _:_ -> error
    end.

parse_seconds_frac(Rest) ->
    case binary:split(Rest, <<".">>) of
        [SecB] -> {SecB, 0};
        [SecB, FracB] ->
            %% Normalize fraction to milliseconds
            FracStr = binary_to_list(FracB),
            Val = list_to_integer(FracStr),
            Ms = case length(FracStr) of
                1 -> Val * 100;
                2 -> Val * 10;
                3 -> Val;
                4 -> Val div 10;
                5 -> Val div 100;
                6 -> Val div 1000;
                _ -> list_to_integer(lists:sublist(FracStr, 7)) div 10000
            end,
            {SecB, Ms}
    end.

%% ============================================================
%% Operators
%% ============================================================

op_subtraction(Left, Right) ->
    add(Left, -Right).
