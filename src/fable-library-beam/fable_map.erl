-module(fable_map).
-export([try_find/2, fold/3, fold_back/3, map/2, filter/2,
         exists/2, forall/2, iterate/2, find_key/2, try_find_key/2,
         partition/2, try_get_value/2, try_get_value/3,
         pick/2, try_pick/2, min_key_value/1, max_key_value/1, change/3]).

%% Fable compiles multi-arg F# lambdas as curried functions:
%%   fun k v -> ...  =>  fun(K) -> fun(V) -> ... end end
%% So we use curried application: (Fn(K))(V) instead of Fn(K, V).

try_find(Key, Map) ->
    case maps:find(Key, Map) of {ok, V} -> V; error -> undefined end.

fold(Fn, State, Map) ->
    maps:fold(fun(K, V, Acc) -> ((Fn(Acc))(K))(V) end, State, Map).

fold_back(Fn, Map, State) ->
    maps:fold(fun(K, V, Acc) -> ((Fn(K))(V))(Acc) end, State, Map).

map(Fn, Map) ->
    maps:map(fun(K, V) -> (Fn(K))(V) end, Map).

filter(Fn, Map) ->
    maps:filter(fun(K, V) -> (Fn(K))(V) end, Map).

exists(Fn, Map) ->
    lists:any(fun({K, V}) -> (Fn(K))(V) end, maps:to_list(Map)).

forall(Fn, Map) ->
    lists:all(fun({K, V}) -> (Fn(K))(V) end, maps:to_list(Map)).

iterate(Fn, Map) ->
    maps:foreach(fun(K, V) -> (Fn(K))(V) end, Map).

find_key(Fn, Map) ->
    case lists:dropwhile(fun({K, V}) -> not (Fn(K))(V) end, maps:to_list(Map)) of
        [{K, _}|_] -> K;
        [] -> erlang:error(<<"key_not_found">>)
    end.

try_find_key(Fn, Map) ->
    case lists:dropwhile(fun({K, V}) -> not (Fn(K))(V) end, maps:to_list(Map)) of
        [{K, _}|_] -> K;
        [] -> undefined
    end.

partition(Fn, Map) ->
    {maps:filter(fun(K, V) -> (Fn(K))(V) end, Map),
     maps:filter(fun(K, V) -> not (Fn(K))(V) end, Map)}.

try_get_value(Key, Map) ->
    case maps:find(Key, Map) of {ok, V} -> {true, V}; error -> {false, undefined} end.

%% TryGetValue with out-parameter: sets the out-ref and returns bool.
try_get_value(Key, Map, OutRef) ->
    case maps:find(Key, Map) of
        {ok, V} -> put(OutRef, V), true;
        error -> false
    end.

pick(Fn, Map) ->
    case try_pick(Fn, Map) of
        undefined -> erlang:error(<<"key_not_found">>);
        V -> V
    end.

try_pick(_Fn, []) -> undefined;
try_pick(Fn, [{K, V}|T]) ->
    case (Fn(K))(V) of
        undefined -> try_pick(Fn, T);
        Result -> Result
    end;
try_pick(Fn, Map) when is_map(Map) ->
    try_pick(Fn, maps:to_list(Map)).

min_key_value(Map) ->
    lists:min(maps:to_list(Map)).

max_key_value(Map) ->
    lists:max(maps:to_list(Map)).

change(Key, Fn, Map) ->
    case maps:find(Key, Map) of
        {ok, V} ->
            case Fn(V) of
                undefined -> maps:remove(Key, Map);
                NewV -> maps:put(Key, NewV, Map)
            end;
        error ->
            case Fn(undefined) of
                undefined -> Map;
                NewV -> maps:put(Key, NewV, Map)
            end
    end.
