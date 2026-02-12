-module(fable_timespan).
-export([
    create/1, create/3, create/4, create/5, create/6,
    from_ticks/1,
    from_days/1, from_hours/1, from_minutes/1, from_seconds/1,
    from_milliseconds/1, from_microseconds/1,
    days/1, hours/1, minutes/1, seconds/1, milliseconds/1,
    microseconds/1, ticks/1,
    total_days/1, total_hours/1, total_minutes/1, total_seconds/1,
    total_milliseconds/1, total_microseconds/1,
    add/2, subtract/2, negate/1, duration/1, multiply/2, divide/2,
    parse/1, parse/2,
    try_parse/2, try_parse/3,
    to_string/1, to_string/2, to_string/3,
    zero/0
]).

%% TimeSpan is represented as an integer of ticks (100-nanosecond units).
%% This matches .NET's System.TimeSpan internal representation.
%%
%% Key constants:
%%   1 tick        = 100 nanoseconds
%%   10 ticks      = 1 microsecond
%%   10,000 ticks  = 1 millisecond
%%   10,000,000    = 1 second
%%   600,000,000   = 1 minute
%%   36,000,000,000 = 1 hour
%%   864,000,000,000 = 1 day

-define(TICKS_PER_MICROSECOND, 10).
-define(TICKS_PER_MILLISECOND, 10000).
-define(TICKS_PER_SECOND, 10000000).
-define(TICKS_PER_MINUTE, 600000000).
-define(TICKS_PER_HOUR, 36000000000).
-define(TICKS_PER_DAY, 864000000000).

%% ============================================================
%% Constructors
%% ============================================================

%% create(Ticks) - single arg: ticks value
create(Ticks) when is_integer(Ticks) -> Ticks;
create(Ticks) when is_float(Ticks) -> trunc(Ticks).

%% create(Hours, Minutes, Seconds)
create(Hours, Minutes, Seconds) ->
    Hours * ?TICKS_PER_HOUR +
    Minutes * ?TICKS_PER_MINUTE +
    Seconds * ?TICKS_PER_SECOND.

%% create(Days, Hours, Minutes, Seconds)
create(Days, Hours, Minutes, Seconds) ->
    Days * ?TICKS_PER_DAY +
    Hours * ?TICKS_PER_HOUR +
    Minutes * ?TICKS_PER_MINUTE +
    Seconds * ?TICKS_PER_SECOND.

%% create(Days, Hours, Minutes, Seconds, Milliseconds)
create(Days, Hours, Minutes, Seconds, Milliseconds) ->
    Days * ?TICKS_PER_DAY +
    Hours * ?TICKS_PER_HOUR +
    Minutes * ?TICKS_PER_MINUTE +
    Seconds * ?TICKS_PER_SECOND +
    Milliseconds * ?TICKS_PER_MILLISECOND.

%% create(Days, Hours, Minutes, Seconds, Milliseconds, Microseconds)
create(Days, Hours, Minutes, Seconds, Milliseconds, Microseconds) ->
    Days * ?TICKS_PER_DAY +
    Hours * ?TICKS_PER_HOUR +
    Minutes * ?TICKS_PER_MINUTE +
    Seconds * ?TICKS_PER_SECOND +
    Milliseconds * ?TICKS_PER_MILLISECOND +
    Microseconds * ?TICKS_PER_MICROSECOND.

%% ============================================================
%% Static factory methods
%% ============================================================

from_ticks(Ticks) when is_integer(Ticks) -> Ticks;
from_ticks(Ticks) when is_float(Ticks) -> trunc(Ticks).

from_days(D) when is_integer(D) -> D * ?TICKS_PER_DAY;
from_days(D) when is_float(D) -> trunc(D * ?TICKS_PER_DAY).

from_hours(H) when is_integer(H) -> H * ?TICKS_PER_HOUR;
from_hours(H) when is_float(H) -> trunc(H * ?TICKS_PER_HOUR).

from_minutes(M) when is_integer(M) -> M * ?TICKS_PER_MINUTE;
from_minutes(M) when is_float(M) -> trunc(M * ?TICKS_PER_MINUTE).

from_seconds(S) when is_integer(S) -> S * ?TICKS_PER_SECOND;
from_seconds(S) when is_float(S) -> trunc(S * ?TICKS_PER_SECOND).

from_milliseconds(Ms) when is_integer(Ms) -> Ms * ?TICKS_PER_MILLISECOND;
from_milliseconds(Ms) when is_float(Ms) -> trunc(Ms * ?TICKS_PER_MILLISECOND).

from_microseconds(Mc) when is_integer(Mc) -> Mc * ?TICKS_PER_MICROSECOND;
from_microseconds(Mc) when is_float(Mc) -> trunc(Mc * ?TICKS_PER_MICROSECOND).

%% ============================================================
%% Component properties
%% ============================================================

%% Days component: truncate toward zero
days(Ticks) when Ticks < 0 ->
    erlang:ceil(Ticks / ?TICKS_PER_DAY);
days(Ticks) ->
    erlang:floor(Ticks / ?TICKS_PER_DAY).

%% Hours component: -23 to 23
hours(Ticks) ->
    trunc(math:fmod(Ticks, ?TICKS_PER_DAY) / ?TICKS_PER_HOUR).

%% Minutes component: -59 to 59
minutes(Ticks) ->
    trunc(math:fmod(Ticks, ?TICKS_PER_HOUR) / ?TICKS_PER_MINUTE).

%% Seconds component: -59 to 59
seconds(Ticks) ->
    trunc(math:fmod(Ticks, ?TICKS_PER_MINUTE) / ?TICKS_PER_SECOND).

%% Milliseconds component: -999 to 999
milliseconds(Ticks) ->
    trunc(math:fmod(Ticks, ?TICKS_PER_SECOND) / ?TICKS_PER_MILLISECOND).

%% Microseconds component: -999 to 999
microseconds(Ticks) ->
    trunc(math:fmod(Ticks, ?TICKS_PER_MILLISECOND) / ?TICKS_PER_MICROSECOND).

%% Ticks (identity)
ticks(Ticks) -> Ticks.

%% ============================================================
%% Total properties (as float)
%% ============================================================

total_days(Ticks) -> Ticks / ?TICKS_PER_DAY.
total_hours(Ticks) -> Ticks / ?TICKS_PER_HOUR.
total_minutes(Ticks) -> Ticks / ?TICKS_PER_MINUTE.
total_seconds(Ticks) -> Ticks / ?TICKS_PER_SECOND.
total_milliseconds(Ticks) -> Ticks / ?TICKS_PER_MILLISECOND.
total_microseconds(Ticks) -> Ticks / ?TICKS_PER_MICROSECOND.

%% ============================================================
%% Arithmetic
%% ============================================================

add(Ticks, Other) -> Ticks + Other.
subtract(Ticks, Other) -> Ticks - Other.
negate(Ticks) -> -Ticks.
duration(Ticks) -> erlang:abs(Ticks).
multiply(Ticks, Factor) -> trunc(Ticks * Factor).
divide(Ticks, Divisor) -> trunc(Ticks / Divisor).

%% ============================================================
%% Zero
%% ============================================================

zero() -> 0.

%% ============================================================
%% ToString
%% ============================================================

to_string(Ticks) -> to_string(Ticks, <<"c">>).

to_string(Ticks, Format) -> to_string(Ticks, Format, undefined).

to_string(Ticks, Format, _Provider) ->
    Sign = case Ticks < 0 of true -> <<"-">>; false -> <<"">> end,
    ATicks = erlang:abs(Ticks),
    D = ATicks div ?TICKS_PER_DAY,
    H = (ATicks rem ?TICKS_PER_DAY) div ?TICKS_PER_HOUR,
    M = (ATicks rem ?TICKS_PER_HOUR) div ?TICKS_PER_MINUTE,
    S = (ATicks rem ?TICKS_PER_MINUTE) div ?TICKS_PER_SECOND,
    SubSecondTicks = ATicks rem ?TICKS_PER_SECOND,
    Ms = SubSecondTicks div ?TICKS_PER_MILLISECOND,
    case Format of
        <<"c">> -> format_c(Sign, D, H, M, S, SubSecondTicks);
        <<"g">> -> format_g(Sign, D, H, M, S, Ms);
        <<"G">> -> format_big_g(Sign, D, H, M, S, SubSecondTicks);
        _ -> erlang:error(<<"Custom formats are not supported">>)
    end.

%% Format "c": [-][d.]hh:mm:ss[.fffffff]
format_c(Sign, D, H, M, S, SubSecondTicks) ->
    DayStr = case D of
        0 -> <<"">>;
        _ -> iolist_to_binary(io_lib:format("~B.", [D]))
    end,
    HourStr = iolist_to_binary(io_lib:format("~2..0B", [H])),
    MinStr = iolist_to_binary(io_lib:format("~2..0B", [M])),
    SecStr = iolist_to_binary(io_lib:format("~2..0B", [S])),
    FracStr = case SubSecondTicks of
        0 -> <<"">>;
        _ -> iolist_to_binary(io_lib:format(".~7..0B", [SubSecondTicks]))
    end,
    iolist_to_binary([Sign, DayStr, HourStr, <<":">>, MinStr, <<":">>, SecStr, FracStr]).

%% Format "g": [-][d:]h:mm:ss[.fff]
format_g(Sign, D, H, M, S, Ms) ->
    DayStr = case D of
        0 -> <<"">>;
        _ -> iolist_to_binary(io_lib:format("~B:", [D]))
    end,
    HourStr = iolist_to_binary(io_lib:format("~B", [H])),
    MinStr = iolist_to_binary(io_lib:format("~2..0B", [M])),
    SecStr = iolist_to_binary(io_lib:format("~2..0B", [S])),
    FracStr = case Ms of
        0 -> <<"">>;
        _ -> iolist_to_binary(io_lib:format(".~3..0B", [Ms]))
    end,
    iolist_to_binary([Sign, DayStr, HourStr, <<":">>, MinStr, <<":">>, SecStr, FracStr]).

%% Format "G": [-]d:hh:mm:ss.fffffff
format_big_g(Sign, D, H, M, S, SubSecondTicks) ->
    DayStr = iolist_to_binary(io_lib:format("~B:", [D])),
    HourStr = iolist_to_binary(io_lib:format("~2..0B", [H])),
    MinStr = iolist_to_binary(io_lib:format("~2..0B", [M])),
    SecStr = iolist_to_binary(io_lib:format("~2..0B", [S])),
    FracStr = iolist_to_binary(io_lib:format(".~7..0B", [SubSecondTicks])),
    iolist_to_binary([Sign, DayStr, HourStr, <<":">>, MinStr, <<":">>, SecStr, FracStr]).

%% ============================================================
%% Parse
%% ============================================================

parse(Str) -> parse(Str, undefined).

parse(Str, _Provider) when is_binary(Str) ->
    S = binary_to_list(Str),
    Trimmed = string:trim(S),
    case {string:chr(Trimmed, $.), string:chr(Trimmed, $:)} of
        {0, 0} ->
            %% No dot and no colon: just a number of days
            case string:to_integer(Trimmed) of
                {Days, []} -> Days * ?TICKS_PER_DAY;
                _ -> parse_error(Str)
            end;
        {_, ColonPos} when ColonPos > 0 ->
            parse_with_regex(Trimmed, Str);
        _ ->
            parse_error(Str)
    end.

parse_with_regex(Trimmed, OrigStr) ->
    Pattern = "^(-?)((\\d+)\\.)?(?:0*)([0-9]|0[0-9]|1[0-9]|2[0-3]):(?:0*)([0-5][0-9]|[0-9])(:(?:0*)([0-5][0-9]|[0-9]))?\\.?(\\d+)?$",
    case re:run(Trimmed, Pattern, [{capture, all, list}]) of
        {match, Groups} ->
            parse_groups(Groups, OrigStr);
        nomatch ->
            parse_error(OrigStr)
    end.

parse_groups(Groups, OrigStr) ->
    %% Groups: [Full, Sign, DayDot, DayNum, Hours, Minutes, SecColon, Seconds, Frac]
    %% Indices:  0     1     2       3       4      5         6        7        8
    Sign = case safe_nth(1, Groups) of "-" -> -1; _ -> 1 end,
    D = case safe_nth(3, Groups) of "" -> 0; DS -> list_to_integer(DS) end,
    H = case safe_nth(4, Groups) of "" -> parse_error(OrigStr); HS -> list_to_integer(HS) end,
    M = case safe_nth(5, Groups) of "" -> parse_error(OrigStr); MS -> list_to_integer(MS) end,
    S = case safe_nth(7, Groups) of "" -> 0; SS -> list_to_integer(SS) end,
    FracTicks = case safe_nth(8, Groups) of
        "" -> 0;
        FracStr -> parse_frac_to_ticks(FracStr, OrigStr)
    end,
    Ticks = D * ?TICKS_PER_DAY + H * ?TICKS_PER_HOUR + M * ?TICKS_PER_MINUTE + S * ?TICKS_PER_SECOND + FracTicks,
    Sign * Ticks.

%% Convert fractional seconds string to ticks by padding/truncating to 7 digits
parse_frac_to_ticks(FracStr, OrigStr) ->
    Len = length(FracStr),
    if
        Len =< 7 ->
            %% Pad right with zeros to 7 digits
            Padded = FracStr ++ lists:duplicate(7 - Len, $0),
            list_to_integer(Padded);
        true ->
            parse_error(OrigStr)
    end.

safe_nth(N, List) when N >= length(List) -> "";
safe_nth(N, List) -> lists:nth(N + 1, List).  %% 0-indexed to 1-indexed

parse_error(Str) ->
    Msg = iolist_to_binary(io_lib:format("String '~s' was not recognized as a valid TimeSpan.", [Str])),
    erlang:error(Msg).

%% ============================================================
%% TryParse
%% ============================================================

%% TryParse(str, outRef) - F# out-parameter pattern
try_parse(Str, OutRef) ->
    try
        Value = parse(Str),
        put(OutRef, Value),
        true
    catch
        _:_ -> false
    end.

%% TryParse(str, provider, outRef) - with format provider
try_parse(Str, _Provider, OutRef) ->
    try_parse(Str, OutRef).
