-module(fable_date_only).
-export([
    create/3,
    min_value/0,
    max_value/0,
    year/1,
    month/1,
    day/1,
    day_of_week/1,
    day_of_year/1,
    day_number/1,
    from_day_number/1,
    from_date_time/1,
    add_days/2,
    add_months/2,
    add_years/2,
    to_date_time/3,
    to_string/1, to_string/2, to_string/3,
    parse/1,
    try_parse/2
]).

-type datetime() :: {integer(), 0 | 1 | 2}.

-spec create(integer(), integer(), integer()) -> datetime().
-spec min_value() -> datetime().
-spec max_value() -> datetime().
-spec year(datetime()) -> integer().
-spec month(datetime()) -> integer().
-spec day(datetime()) -> integer().
-spec day_of_week(datetime()) -> integer().
-spec day_of_year(datetime()) -> integer().
-spec day_number(datetime()) -> integer().
-spec from_day_number(integer()) -> datetime().
-spec from_date_time(datetime()) -> datetime().
-spec add_days(datetime(), integer()) -> datetime().
-spec add_months(datetime(), integer()) -> datetime().
-spec add_years(datetime(), integer()) -> datetime().
-spec to_date_time(datetime(), integer(), integer()) -> datetime().
-spec to_string(datetime()) -> binary().
-spec to_string(datetime(), binary()) -> binary().
-spec to_string(datetime(), binary(), term()) -> binary().
-spec parse(binary()) -> datetime().
-spec try_parse(binary(), reference()) -> boolean().

%% DateOnly is represented as a DateTime tuple {Ticks, Kind}
%% with time components always at midnight (00:00:00).
%% Kind is always 0 (Unspecified).

-define(TICKS_PER_DAY, 864000000000).

%% ============================================================
%% Constructors
%% ============================================================

create(Y, M, D) ->
    fable_date:create(Y, M, D).

%% ============================================================
%% Static fields
%% ============================================================

min_value() -> fable_date:create(1, 1, 1).

max_value() -> fable_date:create(9999, 12, 31).

%% ============================================================
%% Properties
%% ============================================================

year(DT) -> fable_date:year(DT).
month(DT) -> fable_date:month(DT).
day(DT) -> fable_date:day(DT).
day_of_week(DT) -> fable_date:day_of_week(DT).
day_of_year(DT) -> fable_date:day_of_year(DT).

day_number({Ticks, _Kind}) ->
    Ticks div ?TICKS_PER_DAY.

%% ============================================================
%% Factory methods
%% ============================================================

from_day_number(DayNum) ->
    Ticks = DayNum * ?TICKS_PER_DAY,
    {Ticks, 0}.

from_date_time({Ticks, _Kind}) ->
    %% Strip time component
    DayTicks = (Ticks div ?TICKS_PER_DAY) * ?TICKS_PER_DAY,
    {DayTicks, 0}.

%% ============================================================
%% Arithmetic
%% ============================================================

add_days(DT, Days) ->
    {Ticks, Kind} = fable_date:add_days(DT, Days),
    %% Strip time just in case
    DayTicks = (Ticks div ?TICKS_PER_DAY) * ?TICKS_PER_DAY,
    {DayTicks, Kind}.

add_months(DT, Months) ->
    {Ticks, Kind} = fable_date:add_months(DT, Months),
    DayTicks = (Ticks div ?TICKS_PER_DAY) * ?TICKS_PER_DAY,
    {DayTicks, Kind}.

add_years(DT, Years) ->
    {Ticks, Kind} = fable_date:add_years(DT, Years),
    DayTicks = (Ticks div ?TICKS_PER_DAY) * ?TICKS_PER_DAY,
    {DayTicks, Kind}.

%% ============================================================
%% Conversion
%% ============================================================

%% to_date_time(DateOnly, TimeOnlyTicks, Kind)
%% TimeOnly is represented as ticks since midnight.
to_date_time({DayTicks, _Kind}, TimeOnlyTicks, NewKind) ->
    {DayTicks + TimeOnlyTicks, NewKind}.

%% ============================================================
%% ToString
%% ============================================================

to_string(DT) -> to_string(DT, <<"d">>).

to_string(DT, Format) -> to_string(DT, Format, undefined).

to_string(DT, Format, _Provider) ->
    Y = fable_date:year(DT),
    M = fable_date:month(DT),
    D = fable_date:day(DT),
    YStr = pad(Y, 4),
    MStr = pad(M, 2),
    DStr = pad(D, 2),
    case Format of
        <<"d">> ->
            iolist_to_binary([MStr, <<"/">>, DStr, <<"/">>, YStr]);
        _ ->
            %% "o" or "O" format
            iolist_to_binary([YStr, <<"-">>, MStr, <<"-">>, DStr])
    end.

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
    %% Delegate to fable_date:parse and strip time
    DT = fable_date:parse(Str),
    from_date_time(DT).

try_parse(Str, OutRef) ->
    try
        Result = parse(Str),
        put(OutRef, Result),
        true
    catch
        _:_ -> false
    end.
