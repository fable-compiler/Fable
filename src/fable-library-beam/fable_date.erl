-module(fable_date).
-export([
    create/3, create/6, create/7, create/8, create/9,
    from_ticks/1, from_ticks/2,
    year/1, month/1, day/1, hour/1, minute/1, second/1,
    millisecond/1, microsecond/1, ticks/1, kind/1,
    day_of_week/1, day_of_year/1, date/1,
    now/0, utc_now/0, today/0,
    min_value/0, max_value/0,
    is_leap_year/1, days_in_month/2,
    add/2, subtract/2,
    add_years/2, add_months/2,
    add_days/2, add_hours/2, add_minutes/2, add_seconds/2, add_milliseconds/2,
    op_addition/2, op_subtraction/2,
    specify_kind/2, to_local_time/1, to_universal_time/1,
    compare/2, equals/2,
    to_string/1, to_string/2, to_string/3,
    to_short_date_string/1, to_long_date_string/1,
    to_short_time_string/1, to_long_time_string/1,
    parse/1, parse/2,
    try_parse/2, try_parse/3, try_parse/4
]).

%% DateTime is represented as a 2-tuple {Ticks, Kind}
%% Ticks: integer of 100-nanosecond intervals from Jan 1, 0001 (matching .NET)
%% Kind: 0=Unspecified, 1=UTC, 2=Local
%%
%% Key constants (same as fable_timespan):
%%   10,000 ticks      = 1 millisecond
%%   10,000,000 ticks  = 1 second

-define(TICKS_PER_MICROSECOND, 10).
-define(TICKS_PER_MILLISECOND, 10000).
-define(TICKS_PER_SECOND, 10000000).
-define(TICKS_PER_MINUTE, 600000000).
-define(TICKS_PER_HOUR, 36000000000).
-define(TICKS_PER_DAY, 864000000000).

%% Microseconds from Jan 1, 0001 to Unix epoch (Jan 1, 1970)
%% = 62135596800 seconds * 1000000
-define(UNIX_EPOCH_OFFSET_MICROS, 62135596800000000).

%% Gregorian seconds offset: year 0 has 366 days (leap year in proleptic Gregorian)
%% calendar module starts from year 0, .NET from year 1
-define(EPOCH_OFFSET_SECONDS, 31622400).

%% DateTimeKind values
-define(KIND_UNSPECIFIED, 0).
-define(KIND_UTC, 1).
-define(KIND_LOCAL, 2).

%% ============================================================
%% Internal helpers
%% ============================================================

%% Convert date components to ticks
ticks_from_components(Y, M, D, H, Min, S, Ms, Mc) ->
    GregSecs = calendar:datetime_to_gregorian_seconds({{Y, M, D}, {H, Min, S}}),
    NetSecs = GregSecs - ?EPOCH_OFFSET_SECONDS,
    NetSecs * ?TICKS_PER_SECOND + Ms * ?TICKS_PER_MILLISECOND + Mc * ?TICKS_PER_MICROSECOND.

%% Convert ticks to {{Y,M,D},{H,Min,S}} plus sub-second ticks
ticks_to_datetime(Ticks) ->
    TotalSeconds = Ticks div ?TICKS_PER_SECOND,
    SubSecondTicks = Ticks rem ?TICKS_PER_SECOND,
    GregSecs = TotalSeconds + ?EPOCH_OFFSET_SECONDS,
    {{Y, M, D}, {H, Min, S}} = calendar:gregorian_seconds_to_datetime(GregSecs),
    {Y, M, D, H, Min, S, SubSecondTicks}.

%% ============================================================
%% Constructors
%% ============================================================

%% create(Y, M, D) - Kind=Unspecified
create(Y, M, D) ->
    {ticks_from_components(Y, M, D, 0, 0, 0, 0, 0), ?KIND_UNSPECIFIED}.

%% create(Y, M, D, H, Min, S) - Kind=Unspecified
create(Y, M, D, H, Min, S) ->
    {ticks_from_components(Y, M, D, H, Min, S, 0, 0), ?KIND_UNSPECIFIED}.

%% create(Y, M, D, H, Min, S, Ms) - Kind=Unspecified
create(Y, M, D, H, Min, S, Ms) ->
    {ticks_from_components(Y, M, D, H, Min, S, Ms, 0), ?KIND_UNSPECIFIED}.

%% create(Y, M, D, H, Min, S, Ms, Kind) - with Kind
create(Y, M, D, H, Min, S, Ms, Kind) when is_integer(Kind) ->
    {ticks_from_components(Y, M, D, H, Min, S, Ms, 0), Kind}.

%% create(Y, M, D, H, Min, S, Ms, Mc, Kind) - full with microseconds
create(Y, M, D, H, Min, S, Ms, Mc, Kind) ->
    {ticks_from_components(Y, M, D, H, Min, S, Ms, Mc), Kind}.

%% from_ticks(Ticks) - Kind=Unspecified
from_ticks(Ticks) when is_integer(Ticks) ->
    {Ticks, ?KIND_UNSPECIFIED}.

%% from_ticks(Ticks, Kind)
from_ticks(Ticks, Kind) when is_integer(Ticks) ->
    {Ticks, Kind}.

%% ============================================================
%% Properties
%% ============================================================

year({Ticks, _Kind}) ->
    {Y, _, _, _, _, _, _} = ticks_to_datetime(Ticks),
    Y.

month({Ticks, _Kind}) ->
    {_, M, _, _, _, _, _} = ticks_to_datetime(Ticks),
    M.

day({Ticks, _Kind}) ->
    {_, _, D, _, _, _, _} = ticks_to_datetime(Ticks),
    D.

hour({Ticks, _Kind}) ->
    {_, _, _, H, _, _, _} = ticks_to_datetime(Ticks),
    H.

minute({Ticks, _Kind}) ->
    {_, _, _, _, Min, _, _} = ticks_to_datetime(Ticks),
    Min.

second({Ticks, _Kind}) ->
    {_, _, _, _, _, S, _} = ticks_to_datetime(Ticks),
    S.

millisecond({Ticks, _Kind}) ->
    SubSecondTicks = Ticks rem ?TICKS_PER_SECOND,
    SubSecondTicks div ?TICKS_PER_MILLISECOND.

microsecond({Ticks, _Kind}) ->
    SubSecondTicks = Ticks rem ?TICKS_PER_SECOND,
    (SubSecondTicks rem ?TICKS_PER_MILLISECOND) div ?TICKS_PER_MICROSECOND.

ticks({Ticks, _Kind}) -> Ticks.

kind({_Ticks, Kind}) -> Kind.

%% DayOfWeek: .NET uses 0=Sun..6=Sat
%% calendar:day_of_the_week returns 1=Mon..7=Sun
day_of_week({Ticks, _Kind}) ->
    {Y, M, D, _, _, _, _} = ticks_to_datetime(Ticks),
    ErlDow = calendar:day_of_the_week({Y, M, D}),
    ErlDow rem 7.  % 7(Sun)->0, 1(Mon)->1, ..., 6(Sat)->6

day_of_year({Ticks, _Kind}) ->
    {Y, M, D, _, _, _, _} = ticks_to_datetime(Ticks),
    calendar:date_to_gregorian_days({Y, M, D}) - calendar:date_to_gregorian_days({Y, 1, 1}) + 1.

%% Date property: returns DateTime with time zeroed
date({Ticks, Kind}) ->
    {Y, M, D, _, _, _, _} = ticks_to_datetime(Ticks),
    {ticks_from_components(Y, M, D, 0, 0, 0, 0, 0), Kind}.

%% ============================================================
%% Static methods
%% ============================================================

now() ->
    UnixMicro = erlang:system_time(microsecond),
    NetTicks = (UnixMicro + ?UNIX_EPOCH_OFFSET_MICROS) * ?TICKS_PER_MICROSECOND,
    {NetTicks, ?KIND_LOCAL}.

utc_now() ->
    UnixMicro = erlang:system_time(microsecond),
    NetTicks = (UnixMicro + ?UNIX_EPOCH_OFFSET_MICROS) * ?TICKS_PER_MICROSECOND,
    {NetTicks, ?KIND_UTC}.

today() ->
    {Ticks, _} = now(),
    {Y, M, D, _, _, _, _} = ticks_to_datetime(Ticks),
    {ticks_from_components(Y, M, D, 0, 0, 0, 0, 0), ?KIND_LOCAL}.

min_value() -> {0, ?KIND_UNSPECIFIED}.

max_value() -> {3155378975999999999, ?KIND_UNSPECIFIED}.

is_leap_year(Year) ->
    calendar:is_leap_year(Year).

days_in_month(Year, Month) ->
    calendar:last_day_of_the_month(Year, Month).

%% ============================================================
%% Arithmetic
%% ============================================================

%% Add TimeSpan (ticks) to DateTime
add({Ticks, Kind}, TimeSpanTicks) ->
    {Ticks + TimeSpanTicks, Kind}.

%% Subtract: DT - TimeSpan -> DT, DT - DT -> TimeSpan
subtract({Ticks1, Kind}, {Ticks2, _Kind2}) ->
    %% DateTime - DateTime -> TimeSpan (ticks)
    Ticks1 - Ticks2;
subtract({Ticks, Kind}, TimeSpanTicks) when is_integer(TimeSpanTicks) ->
    %% DateTime - TimeSpan -> DateTime
    {Ticks - TimeSpanTicks, Kind}.

add_years({Ticks, Kind}, Years) ->
    {Y, M, D, H, Min, S, SubSecondTicks} = ticks_to_datetime(Ticks),
    NewY = Y + Years,
    %% Clamp day to valid range for new year/month
    MaxDay = calendar:last_day_of_the_month(NewY, M),
    NewD = min(D, MaxDay),
    BaseTicks = ticks_from_components(NewY, M, NewD, H, Min, S, 0, 0),
    {BaseTicks + SubSecondTicks, Kind}.

add_months({Ticks, Kind}, Months) ->
    {Y, M, D, H, Min, S, SubSecondTicks} = ticks_to_datetime(Ticks),
    %% Calculate total months
    TotalMonths = (Y * 12 + (M - 1)) + Months,
    NewY = TotalMonths div 12,
    NewM = (TotalMonths rem 12) + 1,
    %% Handle negative remainder
    {FinalY, FinalM} = case NewM =< 0 of
        true -> {NewY - 1, NewM + 12};
        false -> {NewY, NewM}
    end,
    MaxDay = calendar:last_day_of_the_month(FinalY, FinalM),
    NewD = min(D, MaxDay),
    BaseTicks = ticks_from_components(FinalY, FinalM, NewD, H, Min, S, 0, 0),
    {BaseTicks + SubSecondTicks, Kind}.

add_days({Ticks, Kind}, Days) ->
    Added = trunc(Days * ?TICKS_PER_DAY),
    {Ticks + Added, Kind}.

add_hours({Ticks, Kind}, Hours) ->
    Added = trunc(Hours * ?TICKS_PER_HOUR),
    {Ticks + Added, Kind}.

add_minutes({Ticks, Kind}, Minutes) ->
    Added = trunc(Minutes * ?TICKS_PER_MINUTE),
    {Ticks + Added, Kind}.

add_seconds({Ticks, Kind}, Seconds) ->
    Added = trunc(Seconds * ?TICKS_PER_SECOND),
    {Ticks + Added, Kind}.

add_milliseconds({Ticks, Kind}, Milliseconds) ->
    Added = trunc(Milliseconds * ?TICKS_PER_MILLISECOND),
    {Ticks + Added, Kind}.

%% Operators
op_addition(DT, TimeSpanTicks) -> add(DT, TimeSpanTicks).
op_subtraction(DT, Other) -> subtract(DT, Other).

%% ============================================================
%% Kind operations
%% ============================================================

specify_kind({Ticks, _Kind}, NewKind) ->
    {Ticks, NewKind}.

to_local_time({Ticks, Kind}) ->
    case Kind of
        ?KIND_LOCAL -> {Ticks, ?KIND_LOCAL};
        _ -> {Ticks, ?KIND_LOCAL}
    end.

to_universal_time({Ticks, Kind}) ->
    case Kind of
        ?KIND_UTC -> {Ticks, ?KIND_UTC};
        _ -> {Ticks, ?KIND_UTC}
    end.

%% ============================================================
%% Comparison
%% ============================================================

compare({Ticks1, _}, {Ticks2, _}) ->
    if
        Ticks1 < Ticks2 -> -1;
        Ticks1 > Ticks2 -> 1;
        true -> 0
    end.

equals({Ticks1, _}, {Ticks2, _}) ->
    Ticks1 =:= Ticks2.

%% ============================================================
%% ToString
%% ============================================================

to_string(DT) -> to_string(DT, <<"">>).

to_string(DT, Format) -> to_string(DT, Format, undefined).

to_string({Ticks, Kind}, Format, _Provider) ->
    {Y, M, D, H, Min, S, SubSecondTicks} = ticks_to_datetime(Ticks),
    Ms = SubSecondTicks div ?TICKS_PER_MILLISECOND,
    Mc = (SubSecondTicks rem ?TICKS_PER_MILLISECOND) div ?TICKS_PER_MICROSECOND,
    FmtStr = case Format of
        B when is_binary(B) -> binary_to_list(B);
        L when is_list(L) -> L
    end,
    Result = case FmtStr of
        "" -> format_default(Y, M, D, H, Min, S, Ms, Mc, Kind);
        "d" -> format_short_date(Y, M, D);
        "D" -> format_long_date(Y, M, D);
        "t" -> format_short_time(H, Min);
        "T" -> format_long_time(H, Min, S);
        "O" -> format_roundtrip(Y, M, D, H, Min, S, SubSecondTicks, Kind);
        "o" -> format_roundtrip(Y, M, D, H, Min, S, SubSecondTicks, Kind);
        _ -> format_custom(FmtStr, Y, M, D, H, Min, S, SubSecondTicks, Kind)
    end,
    iolist_to_binary(Result).

format_default(Y, M, D, H, Min, S, _Ms, _Mc, _Kind) ->
    io_lib:format("~2..0B/~2..0B/~4..0B ~2..0B:~2..0B:~2..0B", [M, D, Y, H, Min, S]).

format_short_date(Y, M, D) ->
    io_lib:format("~2..0B/~2..0B/~4..0B", [M, D, Y]).

format_long_date(Y, M, D) ->
    DowName = day_name_long(day_of_week_raw(Y, M, D)),
    MonName = month_name_long(M),
    io_lib:format("~s, ~2..0B ~s ~4..0B", [DowName, D, MonName, Y]).

format_short_time(H, Min) ->
    io_lib:format("~2..0B:~2..0B", [H, Min]).

format_long_time(H, Min, S) ->
    io_lib:format("~2..0B:~2..0B:~2..0B", [H, Min, S]).

format_roundtrip(Y, M, D, H, Min, S, SubSecondTicks, Kind) ->
    KindSuffix = case Kind of
        ?KIND_UTC -> "Z";
        _ -> ""
    end,
    io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B.~7..0B~s",
                  [Y, M, D, H, Min, S, SubSecondTicks, KindSuffix]).

%% Short helper methods
to_short_date_string(DT) -> to_string(DT, <<"d">>).
to_long_date_string(DT) -> to_string(DT, <<"D">>).
to_short_time_string(DT) -> to_string(DT, <<"t">>).
to_long_time_string(DT) -> to_string(DT, <<"T">>).

%% ============================================================
%% Custom format string parser
%% ============================================================

format_custom(FmtStr, Y, M, D, H, Min, S, SubSecondTicks, Kind) ->
    Ms = SubSecondTicks div ?TICKS_PER_MILLISECOND,
    Mc = (SubSecondTicks rem ?TICKS_PER_MILLISECOND) div ?TICKS_PER_MICROSECOND,
    Ctx = #{y => Y, m => M, d => D, h => H, min => Min, s => S,
            ms => Ms, mc => Mc, sub_ticks => SubSecondTicks, kind => Kind},
    parse_format(FmtStr, Ctx, []).

parse_format([], _Ctx, Acc) ->
    lists:reverse(Acc);
parse_format([$\\, C | Rest], Ctx, Acc) ->
    %% Escape character: emit literal
    parse_format(Rest, Ctx, [C | Acc]);
parse_format([$', $' | Rest], Ctx, Acc) ->
    %% Empty quoted string
    parse_format(Rest, Ctx, Acc);
parse_format([$' | Rest], Ctx, Acc) ->
    %% Single-quoted literal string
    {Literal, Rest2} = parse_quoted(Rest, $', []),
    parse_format(Rest2, Ctx, [Literal | Acc]);
parse_format([$" | Rest], Ctx, Acc) ->
    %% Double-quoted literal string
    {Literal, Rest2} = parse_quoted(Rest, $", []),
    parse_format(Rest2, Ctx, [Literal | Acc]);
parse_format([$% | Rest], Ctx, Acc) ->
    %% Format specifier: next char is a single format token
    case Rest of
        [C | Rest2] ->
            Token = format_token(C, 1, Ctx),
            parse_format(Rest2, Ctx, [Token | Acc]);
        [] ->
            parse_format([], Ctx, Acc)
    end;
parse_format([C | Rest], Ctx, Acc) when C =:= $d; C =:= $f; C =:= $F;
                                          C =:= $g; C =:= $h; C =:= $H;
                                          C =:= $K; C =:= $m; C =:= $M;
                                          C =:= $s; C =:= $t; C =:= $y;
                                          C =:= $z ->
    %% Count consecutive same characters
    {Count, Rest2} = count_repeat(C, Rest, 1),
    Token = format_token(C, Count, Ctx),
    parse_format(Rest2, Ctx, [Token | Acc]);
parse_format([$: | Rest], Ctx, Acc) ->
    parse_format(Rest, Ctx, [$: | Acc]);
parse_format([$/ | Rest], Ctx, Acc) ->
    parse_format(Rest, Ctx, [$/ | Acc]);
parse_format([C | Rest], Ctx, Acc) ->
    %% Literal character
    parse_format(Rest, Ctx, [C | Acc]).

parse_quoted([], _Quote, Acc) ->
    {lists:reverse(Acc), []};
parse_quoted([Q | Rest], Q, Acc) ->
    {lists:reverse(Acc), Rest};
parse_quoted([C | Rest], Q, Acc) ->
    parse_quoted(Rest, Q, [C | Acc]).

count_repeat(C, [C | Rest], N) ->
    count_repeat(C, Rest, N + 1);
count_repeat(_C, Rest, N) ->
    {N, Rest}.

format_token($d, Count, Ctx) ->
    #{d := D, y := Y, m := M} = Ctx,
    if
        Count =:= 1 -> integer_to_list(D);
        Count =:= 2 -> io_lib:format("~2..0B", [D]);
        Count =:= 3 -> day_name_short(day_of_week_raw(Y, M, D));
        Count >= 4 -> day_name_long(day_of_week_raw(Y, M, D))
    end;
format_token($f, Count, Ctx) ->
    #{sub_ticks := SubSecondTicks} = Ctx,
    if
        Count >= 1, Count =< 7 ->
            %% Sub-second ticks is up to 7 digits
            Str = io_lib:format("~7..0B", [SubSecondTicks]),
            lists:sublist(lists:flatten(Str), Count);
        true ->
            erlang:error(<<"Invalid format: too many 'f' specifiers">>)
    end;
format_token($F, Count, Ctx) ->
    #{sub_ticks := SubSecondTicks} = Ctx,
    if
        Count >= 1, Count =< 7 ->
            Str = io_lib:format("~7..0B", [SubSecondTicks]),
            Trimmed = lists:sublist(lists:flatten(Str), Count),
            %% Remove trailing zeros
            string:strip(Trimmed, right, $0);
        true ->
            erlang:error(<<"Invalid format: too many 'F' specifiers">>)
    end;
format_token($g, _Count, _Ctx) ->
    "A.D.";
format_token($h, Count, Ctx) ->
    #{h := H} = Ctx,
    H12 = case H rem 12 of
        0 -> 12;
        V -> V
    end,
    if
        Count =:= 1 -> integer_to_list(H12);
        Count >= 2 -> io_lib:format("~2..0B", [H12])
    end;
format_token($H, Count, Ctx) ->
    #{h := H} = Ctx,
    if
        Count =:= 1 -> integer_to_list(H);
        Count >= 2 -> io_lib:format("~2..0B", [H])
    end;
format_token($K, _Count, Ctx) ->
    #{kind := Kind} = Ctx,
    case Kind of
        ?KIND_UTC -> "Z";
        _ -> ""
    end;
format_token($m, Count, Ctx) ->
    #{min := Min} = Ctx,
    if
        Count =:= 1 -> integer_to_list(Min);
        Count >= 2 -> io_lib:format("~2..0B", [Min])
    end;
format_token($M, Count, Ctx) ->
    #{m := M} = Ctx,
    if
        Count =:= 1 -> integer_to_list(M);
        Count =:= 2 -> io_lib:format("~2..0B", [M]);
        Count =:= 3 -> month_name_short(M);
        Count >= 4 -> month_name_long(M)
    end;
format_token($s, Count, Ctx) ->
    #{s := S} = Ctx,
    if
        Count =:= 1 -> integer_to_list(S);
        Count >= 2 -> io_lib:format("~2..0B", [S])
    end;
format_token($t, Count, Ctx) ->
    #{h := H} = Ctx,
    AmPm = case H < 12 of
        true -> "AM";
        false -> "PM"
    end,
    if
        Count =:= 1 -> [hd(AmPm)];
        Count >= 2 -> AmPm
    end;
format_token($y, Count, Ctx) ->
    #{y := Y} = Ctx,
    if
        Count =:= 1 -> integer_to_list(Y rem 100);
        Count =:= 2 -> io_lib:format("~2..0B", [Y rem 100]);
        Count =:= 3 ->
            case Y < 1000 of
                true -> io_lib:format("~3..0B", [Y]);
                false -> integer_to_list(Y)
            end;
        Count =:= 4 -> io_lib:format("~4..0B", [Y]);
        Count >= 5 ->
            Fmt = lists:flatten(io_lib:format("~~~B..0B", [Count])),
            io_lib:format(Fmt, [Y])
    end;
format_token($z, Count, Ctx) ->
    #{kind := Kind} = Ctx,
    %% For UTC kind, offset is 0
    %% For Local/Unspecified, we'd need timezone info; use 0 for UTC
    OffsetMinutes = case Kind of
        ?KIND_UTC -> 0;
        _ -> 0  % Simplified: treat as UTC offset
    end,
    OffsetH = abs(OffsetMinutes) div 60,
    OffsetM = abs(OffsetMinutes) rem 60,
    Sign = case OffsetMinutes >= 0 of true -> "+"; false -> "-" end,
    if
        Count =:= 1 -> io_lib:format("~s~B", [Sign, OffsetH]);
        Count =:= 2 -> io_lib:format("~s~2..0B", [Sign, OffsetH]);
        Count >= 3 -> io_lib:format("~s~2..0B:~2..0B", [Sign, OffsetH, OffsetM])
    end.

%% ============================================================
%% Day/Month name helpers
%% ============================================================

%% Returns 0=Sun..6=Sat (same as DayOfWeek)
day_of_week_raw(Y, M, D) ->
    calendar:day_of_the_week({Y, M, D}) rem 7.

day_name_short(0) -> "Sun";
day_name_short(1) -> "Mon";
day_name_short(2) -> "Tue";
day_name_short(3) -> "Wed";
day_name_short(4) -> "Thu";
day_name_short(5) -> "Fri";
day_name_short(6) -> "Sat".

day_name_long(0) -> "Sunday";
day_name_long(1) -> "Monday";
day_name_long(2) -> "Tuesday";
day_name_long(3) -> "Wednesday";
day_name_long(4) -> "Thursday";
day_name_long(5) -> "Friday";
day_name_long(6) -> "Saturday".

month_name_short(1) -> "Jan";
month_name_short(2) -> "Feb";
month_name_short(3) -> "Mar";
month_name_short(4) -> "Apr";
month_name_short(5) -> "May";
month_name_short(6) -> "Jun";
month_name_short(7) -> "Jul";
month_name_short(8) -> "Aug";
month_name_short(9) -> "Sep";
month_name_short(10) -> "Oct";
month_name_short(11) -> "Nov";
month_name_short(12) -> "Dec".

month_name_long(1) -> "January";
month_name_long(2) -> "February";
month_name_long(3) -> "March";
month_name_long(4) -> "April";
month_name_long(5) -> "May";
month_name_long(6) -> "June";
month_name_long(7) -> "July";
month_name_long(8) -> "August";
month_name_long(9) -> "September";
month_name_long(10) -> "October";
month_name_long(11) -> "November";
month_name_long(12) -> "December".

%% ============================================================
%% Parse
%% ============================================================

parse(Str) -> parse(Str, undefined).

parse(Str, _Provider) when is_binary(Str) ->
    S = string:trim(binary_to_list(Str)),
    parse_string(S, Str).

parse_string(S, OrigStr) ->
    %% Try ISO 8601 first: "2014-09-10T13:50:34.0000000"
    case try_parse_iso(S) of
        {ok, DT} -> DT;
        error ->
            %% Try US date format: "9/10/2014 1:50:34 PM" or "9/10/2014 13:50:34"
            case try_parse_us(S) of
                {ok, DT} -> DT;
                error ->
                    %% Try time-only: "13:50:34" or "1:5:34 AM"
                    case try_parse_time_only(S) of
                        {ok, DT} -> DT;
                        error ->
                            parse_error(OrigStr)
                    end
            end
    end.

try_parse_iso(S) ->
    Pattern = "^(\\d{4})-(\\d{1,2})-(\\d{1,2})T(\\d{1,2}):(\\d{1,2}):(\\d{1,2})(?:\\.(\\d+))?([Zz])?$",
    case re:run(S, Pattern, [{capture, all, list}]) of
        {match, Groups} ->
            Y = list_to_integer(lists:nth(2, Groups)),
            M = list_to_integer(lists:nth(3, Groups)),
            D = list_to_integer(lists:nth(4, Groups)),
            H = list_to_integer(lists:nth(5, Groups)),
            Min = list_to_integer(lists:nth(6, Groups)),
            Sec = list_to_integer(lists:nth(7, Groups)),
            FracStr = safe_group(8, Groups),
            SubSecondTicks = parse_frac_to_ticks(FracStr),
            KindStr = safe_group(9, Groups),
            Kind = case KindStr of
                "Z" -> ?KIND_UTC;
                "z" -> ?KIND_UTC;
                _ -> ?KIND_UNSPECIFIED
            end,
            BaseTicks = ticks_from_components(Y, M, D, H, Min, Sec, 0, 0),
            {ok, {BaseTicks + SubSecondTicks, Kind}};
        nomatch ->
            error
    end.

try_parse_us(S) ->
    Pattern = "^(\\d{1,2})/(\\d{1,2})/(\\d{4})(?:\\s+(\\d{1,2}):(\\d{1,2})(?::(\\d{1,2}))?(?:\\s+(AM|PM|am|pm))?)?$",
    case re:run(S, Pattern, [{capture, all, list}]) of
        {match, Groups} ->
            M = list_to_integer(lists:nth(2, Groups)),
            D = list_to_integer(lists:nth(3, Groups)),
            Y = list_to_integer(lists:nth(4, Groups)),
            %% Validate month and day
            case M >= 1 andalso M =< 12 andalso D >= 1 andalso D =< 31 of
                false -> error;
                true ->
                    H0 = case safe_group(5, Groups) of
                        "" -> 0;
                        HS -> list_to_integer(HS)
                    end,
                    Min = case safe_group(6, Groups) of
                        "" -> 0;
                        MS -> list_to_integer(MS)
                    end,
                    Sec = case safe_group(7, Groups) of
                        "" -> 0;
                        SS -> list_to_integer(SS)
                    end,
                    AmPm = safe_group(8, Groups),
                    H = adjust_ampm(H0, AmPm),
                    {ok, {ticks_from_components(Y, M, D, H, Min, Sec, 0, 0), ?KIND_UNSPECIFIED}}
            end;
        nomatch ->
            error
    end.

try_parse_time_only(S) ->
    Pattern = "^(\\d{1,2}):(\\d{1,2})(?::(\\d{1,2}))?(?:\\s+(AM|PM|am|pm))?$",
    case re:run(S, Pattern, [{capture, all, list}]) of
        {match, Groups} ->
            H0 = list_to_integer(lists:nth(2, Groups)),
            Min = list_to_integer(lists:nth(3, Groups)),
            Sec = case safe_group(4, Groups) of
                "" -> 0;
                SS -> list_to_integer(SS)
            end,
            AmPm = safe_group(5, Groups),
            H = adjust_ampm(H0, AmPm),
            %% Use current date
            {NowTicks, _} = now(),
            {Y, M, D, _, _, _, _} = ticks_to_datetime(NowTicks),
            {ok, {ticks_from_components(Y, M, D, H, Min, Sec, 0, 0), ?KIND_UNSPECIFIED}};
        nomatch ->
            error
    end.

adjust_ampm(H, AmPm) ->
    case string:uppercase(AmPm) of
        "PM" ->
            case H < 12 of
                true -> H + 12;
                false -> H
            end;
        "AM" ->
            case H >= 12 of
                true -> H - 12;
                false -> H
            end;
        _ -> H
    end.

parse_frac_to_ticks("") -> 0;
parse_frac_to_ticks(FracStr) ->
    Len = length(FracStr),
    if
        Len =< 7 ->
            Padded = FracStr ++ lists:duplicate(7 - Len, $0),
            list_to_integer(Padded);
        true ->
            %% Truncate to 7 digits
            list_to_integer(lists:sublist(FracStr, 7))
    end.

safe_group(N, List) when N > length(List) -> "";
safe_group(N, List) -> lists:nth(N, List).

parse_error(Str) ->
    Msg = iolist_to_binary(io_lib:format("String '~s' was not recognized as a valid DateTime.", [Str])),
    erlang:error(Msg).

%% ============================================================
%% TryParse
%% ============================================================

%% TryParse(str, outRef)
try_parse(Str, OutRef) ->
    try
        Value = parse(Str),
        put(OutRef, Value),
        true
    catch
        _:_ -> false
    end.

%% TryParse(str, provider, outRef)
try_parse(Str, _Provider, OutRef) ->
    try_parse(Str, OutRef).

%% TryParse(str, provider, styles, outRef)
try_parse(Str, _Provider, _Styles, OutRef) ->
    try_parse(Str, OutRef).
