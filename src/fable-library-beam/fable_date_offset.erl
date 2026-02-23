-module(fable_date_offset).
-export([
    create/7, create/8,
    from_date/2,
    from_ticks/2,
    year/1,
    month/1,
    day/1,
    hour/1,
    minute/1,
    second/1,
    millisecond/1,
    ticks/1,
    offset/1,
    date_time/1,
    now/0,
    utc_now/0,
    min_value/0,
    to_local_time/1,
    to_universal_time/1,
    try_parse/2,
    to_string/1, to_string/2, to_string/3
]).

-type datetimeoffset() :: {integer(), 0 | 1 | 2, integer()}.

-spec create(integer(), integer(), integer(), integer(), integer(), integer(), integer()) ->
    datetimeoffset().
-spec create(
    integer(), integer(), integer(), integer(), integer(), integer(), integer(), integer()
) -> datetimeoffset().
-spec from_date({integer(), integer()}, integer()) -> datetimeoffset().
-spec from_ticks(integer(), integer()) -> datetimeoffset().
-spec year(datetimeoffset()) -> integer().
-spec month(datetimeoffset()) -> integer().
-spec day(datetimeoffset()) -> integer().
-spec hour(datetimeoffset()) -> integer().
-spec minute(datetimeoffset()) -> integer().
-spec second(datetimeoffset()) -> integer().
-spec millisecond(datetimeoffset()) -> integer().
-spec ticks(datetimeoffset()) -> integer().
-spec offset(datetimeoffset()) -> integer().
-spec date_time(datetimeoffset()) -> {integer(), integer()}.
-spec now() -> datetimeoffset().
-spec utc_now() -> datetimeoffset().
-spec min_value() -> datetimeoffset().
-spec to_local_time(datetimeoffset()) -> datetimeoffset().
-spec to_universal_time(datetimeoffset()) -> datetimeoffset().
-spec try_parse(binary(), reference()) -> boolean().
-spec to_string(datetimeoffset()) -> binary().
-spec to_string(datetimeoffset(), binary()) -> binary().
-spec to_string(datetimeoffset(), binary(), term()) -> binary().

%% DateTimeOffset is represented as a 3-tuple {Ticks, Kind, OffsetTicks}
%% - Ticks: same as DateTime (100-ns intervals from Jan 1, 0001)
%% - Kind: 0=Unspecified, 1=UTC, 2=Local
%% - OffsetTicks: offset from UTC as TimeSpan ticks
%%
%% For most property access, we delegate to fable_date since the
%% underlying datetime components are the same.

-define(TICKS_PER_MILLISECOND, 10000).
-define(TICKS_PER_SECOND, 10000000).
-define(TICKS_PER_MINUTE, 600000000).
-define(TICKS_PER_HOUR, 36000000000).

%% ============================================================
%% Constructors
%% ============================================================

%% create(Y, M, D, H, Min, S, OffsetTimeSpan)
create(Y, M, D, H, Min, S, OffsetTicks) when is_integer(OffsetTicks) ->
    DT = fable_date:create(Y, M, D, H, Min, S),
    {Ticks, _Kind} = DT,
    Kind =
        case OffsetTicks of
            % UTC
            0 -> 1;
            % Unspecified
            _ -> 0
        end,
    {Ticks, Kind, OffsetTicks}.

%% create(Y, M, D, H, Min, S, Ms, OffsetTimeSpan)
create(Y, M, D, H, Min, S, Ms, OffsetTicks) when is_integer(OffsetTicks) ->
    DT = fable_date:create(Y, M, D, H, Min, S, Ms),
    {Ticks, _Kind} = DT,
    Kind =
        case OffsetTicks of
            0 -> 1;
            _ -> 0
        end,
    {Ticks, Kind, OffsetTicks}.

%% from_date(DateTime, OffsetTimeSpan)
from_date({Ticks, Kind}, OffsetTicks) ->
    {Ticks, Kind, OffsetTicks}.

%% from_ticks(Ticks, OffsetTimeSpan)
from_ticks(Ticks, OffsetTicks) ->
    Kind =
        case OffsetTicks of
            0 -> 1;
            _ -> 0
        end,
    {Ticks, Kind, OffsetTicks}.

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
ticks({Ticks, _Kind, _Offset}) -> Ticks.

%% Offset returns the TimeSpan offset (ticks)
offset({_Ticks, _Kind, OffsetTicks}) -> OffsetTicks.

%% DateTime property: returns a DateTime {Ticks, Kind=Unspecified}
date_time({Ticks, _Kind, _Offset}) -> {Ticks, 0}.

%% ============================================================
%% Static methods
%% ============================================================

now() ->
    {Ticks, _Kind} = fable_date:now(),
    {Ticks, 2, 0}.

utc_now() ->
    {Ticks, _Kind} = fable_date:utc_now(),
    {Ticks, 1, 0}.

min_value() -> {0, 0, 0}.

%% ============================================================
%% Conversion
%% ============================================================

to_local_time({Ticks, _Kind, _Offset}) ->
    {Ticks, 2, 0}.

to_universal_time({Ticks, _Kind, OffsetTicks}) ->
    {Ticks - OffsetTicks, 1, 0}.

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
