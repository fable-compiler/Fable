-module(fable_stopwatch).
-export([create/0, start_new/0, start/1, stop/1, reset/1,
         is_running/1, elapsed_milliseconds/1, elapsed_ticks/1, elapsed/1]).

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
        true -> ok;
        false ->
            put(Ref, State#{start_time => erlang:monotonic_time(microsecond), is_running => true}),
            ok
    end.

stop(Ref) ->
    State = get(Ref),
    case maps:get(is_running, State) of
        false -> ok;
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

is_running(Ref) -> maps:get(is_running, get(Ref)).

get_elapsed_us(Ref) ->
    State = get(Ref),
    Acc = maps:get(elapsed, State),
    case maps:get(is_running, State) of
        false -> Acc;
        true -> Acc + (erlang:monotonic_time(microsecond) - maps:get(start_time, State))
    end.

elapsed_milliseconds(Ref) -> get_elapsed_us(Ref) div 1000.
elapsed_ticks(Ref) -> get_elapsed_us(Ref) * 10.
elapsed(Ref) -> get_elapsed_us(Ref) * 10.
