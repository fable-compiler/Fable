-module(fable_stopwatch).
-export([
    create/0,
    start_new/0,
    start/1,
    stop/1,
    reset/1,
    restart/1,
    is_running/1,
    elapsed_milliseconds/1,
    elapsed_ticks/1,
    elapsed/1,
    get_elapsed_time/1,
    get_elapsed_time/2
]).

-spec create() -> reference().
-spec start_new() -> reference().
-spec start(reference()) -> ok.
-spec stop(reference()) -> ok.
-spec reset(reference()) -> ok.
-spec restart(reference()) -> ok.
-spec is_running(reference()) -> boolean().
-spec elapsed_milliseconds(reference()) -> integer().
-spec elapsed_ticks(reference()) -> integer().
-spec elapsed(reference()) -> integer().
-spec get_elapsed_time(integer()) -> integer().
-spec get_elapsed_time(integer(), integer()) -> integer().

create() ->
    Ref = make_ref(),
    put(Ref, #{start_time => undefined, elapsed => 0, is_running => false}),
    Ref.

start_new() ->
    Ref = create(),
    start(Ref),
    Ref.

start(Ref) ->
    State = get(Ref),
    case maps:get(is_running, State) of
        true ->
            ok;
        false ->
            put(Ref, State#{start_time => erlang:monotonic_time(microsecond), is_running => true}),
            ok
    end.

stop(Ref) ->
    State = get(Ref),
    case maps:get(is_running, State) of
        false ->
            ok;
        true ->
            Now = erlang:monotonic_time(microsecond),
            Start = maps:get(start_time, State),
            Acc = maps:get(elapsed, State) + (Now - Start),
            put(Ref, State#{start_time => undefined, elapsed => Acc, is_running => false}),
            ok
    end.

reset(Ref) ->
    put(Ref, #{start_time => undefined, elapsed => 0, is_running => false}),
    ok.

restart(Ref) ->
    put(Ref, #{start_time => erlang:monotonic_time(microsecond), elapsed => 0, is_running => true}),
    ok.

is_running(Ref) -> maps:get(is_running, get(Ref)).

get_elapsed_us(Ref) ->
    State = get(Ref),
    Acc = maps:get(elapsed, State),
    case maps:get(is_running, State) of
        false -> Acc;
        true -> Acc + (erlang:monotonic_time(microsecond) - maps:get(start_time, State))
    end.

elapsed_milliseconds(Ref) -> get_elapsed_us(Ref) div 1000.
%% invariant: ElapsedTicks uses the same microsecond units as Frequency and GetTimestamp.
elapsed_ticks(Ref) -> get_elapsed_us(Ref).
elapsed(Ref) -> get_elapsed_us(Ref) * 10.

get_elapsed_time(Start) -> get_elapsed_time(Start, erlang:monotonic_time(microsecond)).
get_elapsed_time(Start, End) -> (End - Start) * 10.
