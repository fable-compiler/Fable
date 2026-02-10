-module(fable_dictionary).
-export([
    create_empty/0, create_from_list/1,
    add/3, get_item/2, set_item/3,
    try_get_value/2, try_get_value/3, contains_key/2, contains_value/2,
    remove/2, clear/1,
    get_count/1, get_keys/1, get_values/1,
    get_enumerator/1
]).

%% Dictionary is a mutable map stored in the process dictionary.
%% Representation: a reference pointing to an Erlang map via put/get.

create_empty() ->
    Ref = make_ref(),
    put(Ref, #{}),
    Ref.

%% Create from a list of {Key, Value} tuples.
create_from_list(Items) when is_list(Items) ->
    Ref = make_ref(),
    Map = lists:foldl(fun({K, V}, Acc) -> maps:put(K, V, Acc) end, #{}, Items),
    put(Ref, Map),
    Ref;
create_from_list(Ref) when is_reference(Ref) ->
    %% From another Dictionary ref: copy
    create_from_list(maps:to_list(get(Ref))).

%% Add: throws if key already exists
add(DictRef, Key, Value) ->
    Map = get(DictRef),
    case maps:is_key(Key, Map) of
        true ->
            erlang:error({badkey, Key});
        false ->
            put(DictRef, maps:put(Key, Value, Map)),
            ok
    end.

%% get_Item: throws if key not found
get_item(DictRef, Key) ->
    Map = get(DictRef),
    case maps:find(Key, Map) of
        {ok, Value} -> Value;
        error -> erlang:error({badkey, Key})
    end.

%% set_Item: upsert (add or update)
set_item(DictRef, Key, Value) ->
    put(DictRef, maps:put(Key, Value, get(DictRef))),
    ok.

%% TryGetValue (2-arg): returns {true, Value} or {false, undefined}
try_get_value(DictRef, Key) ->
    Map = get(DictRef),
    case maps:find(Key, Map) of
        {ok, Value} -> {true, Value};
        error -> {false, undefined}
    end.

%% TryGetValue (3-arg): sets OutRef and returns just bool
%% F# desugars `let ok, v = dic.TryGetValue(k)` using an out-ref
try_get_value(DictRef, Key, OutRef) ->
    Map = get(DictRef),
    case maps:find(Key, Map) of
        {ok, Value} ->
            put(OutRef, Value),
            true;
        error ->
            false
    end.

contains_key(DictRef, Key) ->
    maps:is_key(Key, get(DictRef)).

contains_value(DictRef, Value) ->
    lists:member(Value, maps:values(get(DictRef))).

%% Remove: returns true if key was present
remove(DictRef, Key) ->
    Map = get(DictRef),
    case maps:is_key(Key, Map) of
        true ->
            put(DictRef, maps:remove(Key, Map)),
            true;
        false ->
            false
    end.

clear(DictRef) ->
    put(DictRef, #{}),
    ok.

get_count(DictRef) ->
    maps:size(get(DictRef)).

get_keys(DictRef) ->
    maps:keys(get(DictRef)).

get_values(DictRef) ->
    maps:values(get(DictRef)).

%% GetEnumerator: return list of {Key, Value} tuples for iteration
get_enumerator(DictRef) ->
    fable_utils:get_enumerator(maps:to_list(get(DictRef))).
