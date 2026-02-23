-module(fable_dictionary).
-export([
    create_empty/0,
    create_from_list/1,
    add/3,
    get_item/2,
    set_item/3,
    try_get_value/2, try_get_value/3,
    contains_key/2,
    contains_value/2,
    remove/2,
    clear/1,
    get_count/1,
    get_keys/1,
    get_values/1,
    get_enumerator/1
]).

-spec create_empty() -> reference().
-spec create_from_list(list() | reference() | term()) -> reference().
-spec add(reference(), term(), term()) -> ok.
-spec get_item(reference(), term()) -> term().
-spec set_item(reference(), term(), term()) -> ok.
-spec try_get_value(reference(), term()) -> {boolean(), term()}.
-spec try_get_value(reference(), term(), reference()) -> boolean().
-spec contains_key(reference(), term()) -> boolean().
-spec contains_value(reference(), term()) -> boolean().
-spec remove(reference(), term()) -> boolean().
-spec clear(reference()) -> ok.
-spec get_count(reference()) -> non_neg_integer().
-spec get_keys(reference()) -> list().
-spec get_values(reference()) -> list().
-spec get_enumerator(reference()) -> reference().

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
    %% Could be another Dictionary ref (copy) or a lazy seq ref
    Stored = get(Ref),
    case is_map(Stored) of
        true ->
            case maps:is_key(get_enumerator, Stored) of
                true ->
                    %% Lazy seq ref: enumerate to list
                    create_from_list(fable_utils:to_list(Ref));
                false ->
                    %% Dictionary ref: copy
                    create_from_list(maps:to_list(Stored))
            end;
        false when is_list(Stored) ->
            %% Array ref or list-backed ref
            create_from_list(Stored);
        false ->
            create_from_list(fable_utils:to_list(Ref))
    end;
create_from_list(Other) ->
    %% Any other enumerable (maps, lazy seq objects, etc.)
    create_from_list(fable_utils:to_list(Other)).

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
    case Map of
        #{Key := Value} ->
            Value;
        #{} ->
            erlang:error({badkey, Key})
    end.

%% set_Item: upsert (add or update)
set_item(DictRef, Key, Value) ->
    put(DictRef, maps:put(Key, Value, get(DictRef))),
    ok.

%% TryGetValue (2-arg): returns {true, Value} or {false, undefined}
try_get_value(DictRef, Key) ->
    Map = get(DictRef),
    case Map of
        #{Key := Value} ->
            {true, Value};
        #{} ->
            {false, undefined}
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
