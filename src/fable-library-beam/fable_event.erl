-module(fable_event).
-export([new_event/0, trigger/2, trigger/3, publish/1,
         add/2, choose/2, filter/2, map/2, merge/2,
         pairwise/1, partition/2, scan/3, split/2,
         create_event/2]).

-spec new_event() -> reference().
new_event() ->
    Ref = erlang:make_ref(),
    put(Ref, []),
    Ref.

-spec trigger(reference(), term()) -> ok.
trigger(Ref, Value) ->
    trigger(Ref, undefined, Value).

-spec trigger(reference(), term(), term()) -> ok.
trigger(Ref, Sender, Value) ->
    Delegates = get(Ref),
    lists:foreach(fun(F) ->
        case erlang:fun_info(F, arity) of
            {arity, 1} -> F(Value);
            {arity, 2} -> F(Sender, Value);
            _ -> F(Value)
        end
    end, Delegates),
    ok.

-spec publish(reference()) -> map().
publish(Ref) ->
    #{
        add_handler => fun(H) -> put(Ref, get(Ref) ++ [H]), ok end,
        remove_handler => fun(H) ->
            put(Ref, lists:filter(fun(X) -> X =/= H end, get(Ref))),
            ok
        end,
        subscribe => fun(Observer) ->
            Callback = fun(Value) -> (maps:get(on_next, Observer))(Value) end,
            put(Ref, get(Ref) ++ [Callback]),
            #{dispose => fun() ->
                put(Ref, lists:filter(fun(X) -> X =/= Callback end, get(Ref))),
                ok
            end}
        end,
        add => fun(F) -> put(Ref, get(Ref) ++ [F]), ok end
    }.

%% Event module functions

-spec add(fun((term()) -> ok), map()) -> ok.
add(Callback, Source) ->
    fable_observable:subscribe(Callback, Source),
    ok.

-spec choose(fun((term()) -> term()), map()) -> map().
choose(Chooser, SourceEvent) ->
    Ev = new_event(),
    Pub = publish(Ev),
    add(fun(T) ->
        case Chooser(T) of
            undefined -> ok;
            U -> trigger(Ev, U)
        end
    end, SourceEvent),
    Pub.

-spec filter(fun((term()) -> boolean()), map()) -> map().
filter(Predicate, SourceEvent) ->
    choose(fun(X) ->
        case Predicate(X) of
            true -> X;
            false -> undefined
        end
    end, SourceEvent).

-spec map(fun((term()) -> term()), map()) -> map().
map(Mapping, SourceEvent) ->
    Ev = new_event(),
    Pub = publish(Ev),
    add(fun(T) -> trigger(Ev, Mapping(T)) end, SourceEvent),
    Pub.

-spec merge(map(), map()) -> map().
merge(Event1, Event2) ->
    Ev = new_event(),
    Pub = publish(Ev),
    Fn = fun(X) -> trigger(Ev, X) end,
    add(Fn, Event1),
    add(Fn, Event2),
    Pub.

-spec pairwise(map()) -> map().
pairwise(SourceEvent) ->
    Ev = new_event(),
    Pub = publish(Ev),
    Last = fable_utils:new_ref(undefined),
    HasLast = fable_utils:new_ref(false),
    add(fun(Next) ->
        case get(HasLast) of
            true ->
                Prev = get(Last),
                put(Last, Next),
                trigger(Ev, {Prev, Next});
            false ->
                put(Last, Next),
                put(HasLast, true)
        end
    end, SourceEvent),
    Pub.

-spec partition(fun((term()) -> boolean()), map()) -> {map(), map()}.
partition(Predicate, SourceEvent) ->
    {filter(Predicate, SourceEvent),
     filter(fun(X) -> not Predicate(X) end, SourceEvent)}.

-spec scan(fun((term(), term()) -> term()), term(), map()) -> map().
scan(Collector, State, SourceEvent) ->
    Ev = new_event(),
    Pub = publish(Ev),
    StateRef = fable_utils:new_ref(State),
    add(fun(T) ->
        NewState = Collector(get(StateRef), T),
        put(StateRef, NewState),
        trigger(Ev, NewState)
    end, SourceEvent),
    Pub.

-spec split(fun((term()) -> term()), map()) -> {map(), map()}.
split(Splitter, SourceEvent) ->
    {choose(fun(V) ->
        case Splitter(V) of
            {choice1_of2, X} -> X;
            _ -> undefined
        end
    end, SourceEvent),
     choose(fun(V) ->
        case Splitter(V) of
            {choice2_of2, X} -> X;
            _ -> undefined
        end
    end, SourceEvent)}.

-spec create_event(fun(), fun()) -> map().
create_event(AddHandler, RemoveHandler) ->
    #{
        add_handler => AddHandler,
        remove_handler => RemoveHandler,
        subscribe => fun(Observer) ->
            H = fun(_Sender, Value) -> (maps:get(on_next, Observer))(Value) end,
            AddHandler(H),
            #{dispose => fun() -> RemoveHandler(H), ok end}
        end
    }.
