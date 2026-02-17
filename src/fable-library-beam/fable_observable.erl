-module(fable_observable).
-export([subscribe/2, add/2, choose/2, filter/2, map/2,
         merge/2, pairwise/1, partition/2, scan/3, split/2]).

%% Subscribe callback to observable, return IDisposable map.
subscribe(Callback, Source) ->
    Observer = #{on_next => Callback, on_error => fun(_) -> ok end, on_completed => fun() -> ok end},
    call_subscribe(Source, Observer).

%% Add callback (same as subscribe).
add(Callback, Source) ->
    subscribe(Callback, Source).

%% Internal: safely call a function, routing success/error.
protect(F, Succeed, Fail) ->
    try
        Succeed(F())
    catch
        _:E -> Fail(E)
    end.

%% Choose (filter+map via Option).
choose(Chooser, Source) ->
    #{subscribe => fun(Observer) ->
        OnNext = fun(T) ->
            protect(
                fun() -> Chooser(T) end,
                fun(U) ->
                    case U of
                        undefined -> ok;
                        _ -> (maps:get(on_next, Observer))(U)
                    end
                end,
                maps:get(on_error, Observer)
            )
        end,
        Obv = #{on_next => OnNext,
                on_error => maps:get(on_error, Observer),
                on_completed => maps:get(on_completed, Observer)},
        call_subscribe(Source, Obv)
    end}.

%% Filter by predicate.
filter(Predicate, Source) ->
    choose(fun(X) ->
        case Predicate(X) of
            true -> X;
            false -> undefined
        end
    end, Source).

%% Map/transform values.
map(Mapping, Source) ->
    #{subscribe => fun(Observer) ->
        OnNext = fun(Value) ->
            protect(
                fun() -> Mapping(Value) end,
                maps:get(on_next, Observer),
                maps:get(on_error, Observer)
            )
        end,
        Obv = #{on_next => OnNext,
                on_error => maps:get(on_error, Observer),
                on_completed => maps:get(on_completed, Observer)},
        call_subscribe(Source, Obv)
    end}.

%% Merge two observables.
merge(Source1, Source2) ->
    #{subscribe => fun(Observer) ->
        Stopped = fable_utils:new_ref(false),
        Completed1 = fable_utils:new_ref(false),
        Completed2 = fable_utils:new_ref(false),
        OnNext = fun(Value) ->
            case get(Stopped) of
                true -> ok;
                false -> (maps:get(on_next, Observer))(Value)
            end
        end,
        OnError = fun(Error) ->
            case get(Stopped) of
                true -> ok;
                false ->
                    put(Stopped, true),
                    (maps:get(on_error, Observer))(Error)
            end
        end,
        OnCompleted1 = fun() ->
            case get(Stopped) of
                true -> ok;
                false ->
                    put(Completed1, true),
                    case get(Completed2) of
                        true ->
                            put(Stopped, true),
                            (maps:get(on_completed, Observer))();
                        false -> ok
                    end
            end
        end,
        Obv1 = #{on_next => OnNext, on_error => OnError, on_completed => OnCompleted1},
        H1 = call_subscribe(Source1, Obv1),
        OnCompleted2 = fun() ->
            case get(Stopped) of
                true -> ok;
                false ->
                    put(Completed2, true),
                    case get(Completed1) of
                        true ->
                            put(Stopped, true),
                            (maps:get(on_completed, Observer))();
                        false -> ok
                    end
            end
        end,
        Obv2 = #{on_next => OnNext, on_error => OnError, on_completed => OnCompleted2},
        H2 = call_subscribe(Source2, Obv2),
        #{dispose => fun() ->
            fable_utils:safe_dispose(H1),
            fable_utils:safe_dispose(H2)
        end}
    end}.

%% Pairwise: emit pairs of consecutive values.
pairwise(Source) ->
    #{subscribe => fun(Observer) ->
        Last = fable_utils:new_ref(undefined),
        HasLast = fable_utils:new_ref(false),
        OnNext = fun(Value) ->
            case get(HasLast) of
                true ->
                    Prev = get(Last),
                    put(Last, Value),
                    (maps:get(on_next, Observer))({Prev, Value});
                false ->
                    put(Last, Value),
                    put(HasLast, true)
            end
        end,
        Obv = #{on_next => OnNext,
                on_error => maps:get(on_error, Observer),
                on_completed => maps:get(on_completed, Observer)},
        call_subscribe(Source, Obv)
    end}.

%% Partition by predicate: returns {TrueObservable, FalseObservable}.
partition(Predicate, Source) ->
    {filter(Predicate, Source),
     filter(fun(X) -> not Predicate(X) end, Source)}.

%% Scan: stateful accumulation.
scan(Collector, State, Source) ->
    #{subscribe => fun(Observer) ->
        StateRef = fable_utils:new_ref(State),
        OnNext = fun(T) ->
            protect(
                fun() -> Collector(get(StateRef), T) end,
                fun(U) ->
                    put(StateRef, U),
                    (maps:get(on_next, Observer))(U)
                end,
                maps:get(on_error, Observer)
            )
        end,
        Obv = #{on_next => OnNext,
                on_error => maps:get(on_error, Observer),
                on_completed => maps:get(on_completed, Observer)},
        call_subscribe(Source, Obv)
    end}.

%% Split by Choice discriminator: returns {Choice1Observable, Choice2Observable}.
split(Splitter, Source) ->
    {choose(fun(V) ->
        case Splitter(V) of
            {choice1_of2, X} -> X;
            _ -> undefined
        end
    end, Source),
     choose(fun(V) ->
        case Splitter(V) of
            {choice2_of2, X} -> X;
            _ -> undefined
        end
    end, Source)}.

%% Internal: call the subscribe function on an observable.
%% Handles both map-based observables (from this module) and
%% object expressions / class instances (via iface_get).
call_subscribe(Source, Observer) when is_map(Source) ->
    case maps:is_key(subscribe, Source) of
        true -> (maps:get(subscribe, Source))(Observer);
        false -> error({badarg, no_subscribe, Source})
    end;
call_subscribe(Source, Observer) when is_reference(Source) ->
    %% Class instance or object expression stored in process dict
    SubscribeFn = fable_utils:iface_get(subscribe, Source),
    SubscribeFn(Observer).
