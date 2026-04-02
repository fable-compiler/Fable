-module(fable_date_offset).
-export([
    create/7, create/8,
    from_date/2,
    from_ticks/2,
    from_date_time/3,
    year/1,
    month/1,
    day/1,
    hour/1,
    minute/1,
    second/1,
    millisecond/1,
    microsecond/1,
    ticks/1,
    offset/1,
    date_time/1,
    date/1,
    time_of_day/1,
    day_of_week/1,
    day_of_year/1,
    total_offset_minutes/1,
    utc_ticks/1,
    now/0,
    utc_now/0,
    min_value/0,
    max_value/0,
    unix_epoch/0,
    local_date_time/1,
    utc_date_time/1,
    to_local_time/1,
    to_universal_time/1,
    to_offset/2,
    add/2,
    subtract/2,
    add_years/2,
    add_months/2,
    add_days/2,
    add_hours/2,
    add_minutes/2,
    add_seconds/2,
    add_milliseconds/2,
    add_ticks/2,
    op_addition/2,
    op_subtraction/2,
    compare/2,
    equals/2,
    equals_exact/2,
    from_unix_time_seconds/1,
    from_unix_time_milliseconds/1,
    to_unix_time_seconds/1,
    to_unix_time_milliseconds/1,
    try_parse/2,
    to_string/1, to_string/2, to_string/3
]).

-type datetimeoffset() :: {integer(), 0 | 1 | 2, integer()}.

%% DateTimeOffset is represented as a 3-tuple {Ticks, Kind, OffsetTicks}
%% - Ticks: same as DateTime (100-ns intervals from Jan 1, 0001)
%% - Kind: 0=Unspecified, 1=UTC, 2=Local
%% - OffsetTicks: offset from UTC as TimeSpan ticks

-define(TICKS_PER_MILLISECOND, 10000).
-define(TICKS_PER_SECOND, 10000000).
-define(TICKS_PER_MINUTE, 600000000).
-define(TICKS_PER_HOUR, 36000000000).
-define(TICKS_PER_DAY, 864000000000).

%% .NET epoch offset: ticks from 0001-01-01 to 1970-01-01
-define(UNIX_EPOCH_TICKS, 621355968000000000).

%% ============================================================
%% Constructors
%% ============================================================

%% create(Y, M, D, H, Min, S, OffsetTimeSpan)
create(Y, M, D, H, Min, S, OffsetTicks) when is_integer(OffsetTicks) ->
    DT = fable_date:create(Y, M, D, H, Min, S),
    {Ticks, _Kind} = DT,
    Kind = offset_to_kind(OffsetTicks),
    {Ticks, Kind, OffsetTicks}.

%% create(Y, M, D, H, Min, S, Ms, OffsetTimeSpan)
create(Y, M, D, H, Min, S, Ms, OffsetTicks) when is_integer(OffsetTicks) ->
    DT = fable_date:create(Y, M, D, H, Min, S, Ms),
    {Ticks, _Kind} = DT,
    Kind = offset_to_kind(OffsetTicks),
    {Ticks, Kind, OffsetTicks}.

%% from_date(DateTime, OffsetTimeSpan)
from_date({Ticks, Kind}, OffsetTicks) ->
    {Ticks, Kind, OffsetTicks}.

%% from_ticks(Ticks, OffsetTimeSpan)
from_ticks(Ticks, OffsetTicks) ->
    Kind = offset_to_kind(OffsetTicks),
    {Ticks, Kind, OffsetTicks}.

%% from_date_time(DateOnly, TimeOnlyTicks, OffsetTimeSpan)
%% DateOnly is {DayTicks, Kind}, TimeOnly is ticks since midnight
from_date_time({DayTicks, _Kind}, TimeOnlyTicks, OffsetTicks) ->
    Ticks = DayTicks + TimeOnlyTicks,
    Kind = offset_to_kind(OffsetTicks),
    {Ticks, Kind, OffsetTicks}.

offset_to_kind(0) -> 1;  % UTC
offset_to_kind(_) -> 0.  % Unspecified

%% ============================================================
%% Properties (delegate to fable_date)
%% ============================================================

year({Ticks, Kind, _Offset}) -> fable_date:year({Ticks, Kind}).
month({Ticks, Kind, _Offset}) -> fable_date:month({Ticks, Kind}).
day({Ticks, Kind, _Offset}) -> fable_date:day({Ticks, Kind}).
hour({Ticks, Kind, _Offset}) -> fable_date:hour({Ticks, Kind}).
minute({Ticks, Kind, _Offset}) -> fable_date:minute({Ticks, Kind}).
second({Ticks, Kind, _Offset}) -> fable_date:second({Ticks, Kind}).
millisecond({Ticks, Kind, _Offset}) -> fable_date:millisecond({Ticks, Kind}).
microsecond({Ticks, Kind, _Offset}) -> fable_date:microsecond({Ticks, Kind}).
ticks({Ticks, _Kind, _Offset}) -> Ticks.

%% Offset returns the TimeSpan offset (ticks)
offset({_Ticks, _Kind, OffsetTicks}) -> OffsetTicks.

%% DateTime property: returns a DateTime {Ticks, Kind=Unspecified}
date_time({Ticks, _Kind, _Offset}) -> {Ticks, 0}.

%% Date property: returns DateTime with time zeroed
date({Ticks, Kind, _Offset}) -> fable_date:date({Ticks, Kind}).

%% TimeOfDay: returns TimeSpan (ticks) of the time-of-day component
time_of_day({Ticks, _Kind, _Offset}) ->
    Ticks rem ?TICKS_PER_DAY.

%% DayOfWeek
day_of_week({Ticks, Kind, _Offset}) -> fable_date:day_of_week({Ticks, Kind}).

%% DayOfYear
day_of_year({Ticks, Kind, _Offset}) -> fable_date:day_of_year({Ticks, Kind}).

%% TotalOffsetMinutes
total_offset_minutes({_Ticks, _Kind, OffsetTicks}) ->
    OffsetTicks div ?TICKS_PER_MINUTE.

%% UtcTicks: Ticks adjusted by offset
utc_ticks({Ticks, _Kind, OffsetTicks}) ->
    Ticks - OffsetTicks.

%% ============================================================
%% Static methods
%% ============================================================

now() ->
    {Ticks, _Kind} = fable_date:now(),
    OffsetTicks = local_offset_ticks(),
    {Ticks, 2, OffsetTicks}.

utc_now() ->
    {Ticks, _Kind} = fable_date:utc_now(),
    {Ticks, 1, 0}.

min_value() -> {0, 0, 0}.

max_value() -> {3155378975999999999, 0, 0}.

unix_epoch() ->
    {?UNIX_EPOCH_TICKS, 1, 0}.

%% ============================================================
%% Conversion
%% ============================================================

to_local_time({Ticks, _Kind, OffsetTicks}) ->
    %% Convert to UTC ticks, then to local
    UtcTicks = Ticks - OffsetTicks,
    LocalOffset = local_offset_ticks(),
    {UtcTicks + LocalOffset, 2, LocalOffset}.

to_universal_time({Ticks, _Kind, OffsetTicks}) ->
    {Ticks - OffsetTicks, 1, 0}.

to_offset({Ticks, _Kind, OffsetTicks}, NewOffsetTicks) ->
    %% Convert to UTC, then apply new offset
    UtcTicks = Ticks - OffsetTicks,
    Kind = offset_to_kind(NewOffsetTicks),
    {UtcTicks + NewOffsetTicks, Kind, NewOffsetTicks}.

local_date_time({Ticks, _Kind, OffsetTicks}) ->
    UtcTicks = Ticks - OffsetTicks,
    LocalOffset = local_offset_ticks(),
    {UtcTicks + LocalOffset, 2}.

utc_date_time({Ticks, _Kind, OffsetTicks}) ->
    {Ticks - OffsetTicks, 1}.

%% ============================================================
%% Arithmetic
%% ============================================================

add({Ticks, Kind, OffsetTicks}, TimeSpanTicks) ->
    {Ticks + TimeSpanTicks, Kind, OffsetTicks}.

subtract({Ticks1, _Kind1, Offset1}, {Ticks2, _Kind2, Offset2}) ->
    %% DTO - DTO -> TimeSpan (compare UTC instants)
    (Ticks1 - Offset1) - (Ticks2 - Offset2);
subtract({Ticks, Kind, OffsetTicks}, TimeSpanTicks) when is_integer(TimeSpanTicks) ->
    {Ticks - TimeSpanTicks, Kind, OffsetTicks}.

add_years({Ticks, Kind, OffsetTicks}, Years) ->
    {NewTicks, _} = fable_date:add_years({Ticks, Kind}, Years),
    {NewTicks, Kind, OffsetTicks}.

add_months({Ticks, Kind, OffsetTicks}, Months) ->
    {NewTicks, _} = fable_date:add_months({Ticks, Kind}, Months),
    {NewTicks, Kind, OffsetTicks}.

add_days({Ticks, Kind, OffsetTicks}, Days) ->
    Added = trunc(Days * ?TICKS_PER_DAY),
    {Ticks + Added, Kind, OffsetTicks}.

add_hours({Ticks, Kind, OffsetTicks}, Hours) ->
    Added = trunc(Hours * ?TICKS_PER_HOUR),
    {Ticks + Added, Kind, OffsetTicks}.

add_minutes({Ticks, Kind, OffsetTicks}, Minutes) ->
    Added = trunc(Minutes * ?TICKS_PER_MINUTE),
    {Ticks + Added, Kind, OffsetTicks}.

add_seconds({Ticks, Kind, OffsetTicks}, Seconds) ->
    Added = trunc(Seconds * ?TICKS_PER_SECOND),
    {Ticks + Added, Kind, OffsetTicks}.

add_milliseconds({Ticks, Kind, OffsetTicks}, Milliseconds) ->
    Added = trunc(Milliseconds * ?TICKS_PER_MILLISECOND),
    {Ticks + Added, Kind, OffsetTicks}.

add_ticks({Ticks, Kind, OffsetTicks}, AddedTicks) ->
    {Ticks + AddedTicks, Kind, OffsetTicks}.

op_addition(DTO, TimeSpanTicks) -> add(DTO, TimeSpanTicks).
op_subtraction(DTO, Other) -> subtract(DTO, Other).

%% ============================================================
%% Comparison / Equality
%% ============================================================

%% Compare by UTC instant
compare({Ticks1, _Kind1, Offset1}, {Ticks2, _Kind2, Offset2}) ->
    Utc1 = Ticks1 - Offset1,
    Utc2 = Ticks2 - Offset2,
    if
        Utc1 < Utc2 -> -1;
        Utc1 > Utc2 -> 1;
        true -> 0
    end.

equals({Ticks1, _Kind1, Offset1}, {Ticks2, _Kind2, Offset2}) ->
    (Ticks1 - Offset1) =:= (Ticks2 - Offset2).

equals_exact({Ticks1, _Kind1, Offset1}, {Ticks2, _Kind2, Offset2}) ->
    (Ticks1 - Offset1) =:= (Ticks2 - Offset2) andalso Offset1 =:= Offset2.

%% ============================================================
%% Unix time
%% ============================================================

from_unix_time_seconds(Seconds) ->
    Ticks = ?UNIX_EPOCH_TICKS + Seconds * ?TICKS_PER_SECOND,
    {Ticks, 1, 0}.

from_unix_time_milliseconds(Ms) ->
    Ticks = ?UNIX_EPOCH_TICKS + Ms * ?TICKS_PER_MILLISECOND,
    {Ticks, 1, 0}.

to_unix_time_seconds({Ticks, _Kind, OffsetTicks}) ->
    UtcTicks = Ticks - OffsetTicks,
    (UtcTicks - ?UNIX_EPOCH_TICKS) div ?TICKS_PER_SECOND.

to_unix_time_milliseconds({Ticks, _Kind, OffsetTicks}) ->
    UtcTicks = Ticks - OffsetTicks,
    (UtcTicks - ?UNIX_EPOCH_TICKS) div ?TICKS_PER_MILLISECOND.

%% ============================================================
%% TryParse
%% ============================================================

try_parse(Str, OutRef) ->
    try
        %% Parse the datetime part and create DateTimeOffset with zero offset
        DT = fable_date:parse(Str),
        {Ticks, _Kind} = DT,
        DTO = {Ticks, 0, 0},
        put(OutRef, DTO),
        true
    catch
        _:_ -> false
    end.

%% ============================================================
%% ToString
%% ============================================================

to_string(DTO) -> to_string(DTO, <<"">>).

to_string(DTO, Format) -> to_string(DTO, Format, undefined).

to_string({Ticks, Kind, OffsetTicks}, Format, Provider) ->
    %% Delegate formatting to fable_date, then append offset
    DT = {Ticks, Kind},
    BaseStr = fable_date:to_string(DT, Format, Provider),
    case Format of
        <<"">> ->
            %% Default format: append offset
            OffsetStr = format_offset(OffsetTicks),
            iolist_to_binary([BaseStr, <<" ">>, OffsetStr]);
        _ ->
            %% Custom format: just return the formatted string
            BaseStr
    end.

format_offset(OffsetTicks) ->
    Sign =
        case OffsetTicks >= 0 of
            true -> <<"+">>;
            false -> <<"-">>
        end,
    AbsTicks = abs(OffsetTicks),
    H = AbsTicks div ?TICKS_PER_HOUR,
    M = (AbsTicks rem ?TICKS_PER_HOUR) div ?TICKS_PER_MINUTE,
    iolist_to_binary(io_lib:format("~s~2..0B:~2..0B", [Sign, H, M])).

%% ============================================================
%% Internal helpers
%% ============================================================

local_offset_ticks() ->
    %% Get local offset from UTC in ticks
    UtcSecs = erlang:system_time(second),
    {{UY,UM,UD},{UH,UMin,US}} = calendar:now_to_universal_time({UtcSecs div 1000000, UtcSecs rem 1000000, 0}),
    {{LY,LM,LD},{LH,LMin,LS}} = calendar:now_to_local_time({UtcSecs div 1000000, UtcSecs rem 1000000, 0}),
    UtcDays = calendar:date_to_gregorian_days({UY, UM, UD}),
    LocalDays = calendar:date_to_gregorian_days({LY, LM, LD}),
    UtcSecsOfDay = UH * 3600 + UMin * 60 + US,
    LocalSecsOfDay = LH * 3600 + LMin * 60 + LS,
    DiffSecs = (LocalDays - UtcDays) * 86400 + (LocalSecsOfDay - UtcSecsOfDay),
    DiffSecs * ?TICKS_PER_SECOND.
