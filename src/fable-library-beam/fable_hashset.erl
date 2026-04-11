-module(fable_hashset).
-export([
    create_empty/0,
    create_from_list/1,
    add/2,
    remove/2,
    contains/2,
    get_count/1,
    clear/1,
    union_with/2,
    intersect_with/2,
    except_with/2,
    symmetric_except_with/2,
    is_subset_of/2,
    is_superset_of/2,
    is_proper_subset_of/2,
    is_proper_superset_of/2,
    overlaps/2,
    set_equals/2,
    copy_to/2,
    copy_to/3,
    copy_to/4,
    get_enumerator/1
]).

-spec create_empty() -> reference().
-spec create_from_list(list() | term()) -> reference().
-spec add(reference(), term()) -> boolean().
-spec remove(reference(), term()) -> boolean().
-spec contains(reference(), term()) -> boolean().
-spec get_count(reference()) -> non_neg_integer().
-spec clear(reference()) -> ok.
-spec union_with(reference(), reference()) -> ok.
-spec intersect_with(reference(), reference() | term()) -> ok.
-spec except_with(reference(), reference() | term()) -> ok.
-spec symmetric_except_with(reference(), reference() | term()) -> ok.
-spec is_subset_of(reference(), reference() | term()) -> boolean().
-spec is_superset_of(reference(), reference() | term()) -> boolean().
-spec is_proper_subset_of(reference(), reference() | term()) -> boolean().
-spec is_proper_superset_of(reference(), reference() | term()) -> boolean().
-spec overlaps(reference(), reference() | term()) -> boolean().
-spec set_equals(reference(), reference() | term()) -> boolean().
-spec copy_to(reference(), reference()) -> ok.
-spec copy_to(reference(), reference(), non_neg_integer()) -> ok.
-spec copy_to(reference(), reference(), non_neg_integer(), non_neg_integer()) -> ok.
-spec get_enumerator(reference()) -> reference().

%% HashSet is a mutable set stored in the process dictionary.
%% Representation: a reference pointing to an Erlang map #{Item => true}.

create_empty() ->
    Ref = make_ref(),
    put(Ref, #{}),
    Ref.

%% Create from a list of items (deduplicates).
create_from_list(Items) when is_list(Items) ->
    Ref = make_ref(),
    Map = lists:foldl(fun(Item, Acc) -> maps:put(Item, true, Acc) end, #{}, Items),
    put(Ref, Map),
    Ref;
create_from_list(Other) ->
    %% From any enumerable: convert to list first (handles refs, lazy seqs, strings, etc.)
    create_from_list(fable_utils:to_list(Other)).

other_to_map(OtherRef) when is_reference(OtherRef) ->
    case get(OtherRef) of
        undefined -> maps:from_keys(fable_utils:to_list(OtherRef), true);
        Map when is_map(Map) -> Map;
        List when is_list(List) -> maps:from_keys(List, true);
        Other -> maps:from_keys(fable_utils:to_list(Other), true)
    end;
other_to_map(Other) ->
    maps:from_keys(fable_utils:to_list(Other), true).

%% Add: returns true if item was not already present
add(SetRef, Item) ->
    Map = get(SetRef),
    case maps:is_key(Item, Map) of
        true ->
            false;
        false ->
            put(SetRef, maps:put(Item, true, Map)),
            true
    end.

%% Remove: returns true if item was present
remove(SetRef, Item) ->
    Map = get(SetRef),
    case maps:is_key(Item, Map) of
        true ->
            put(SetRef, maps:remove(Item, Map)),
            true;
        false ->
            false
    end.

contains(SetRef, Item) ->
    maps:is_key(Item, get(SetRef)).

get_count(SetRef) ->
    maps:size(get(SetRef)).

clear(SetRef) ->
    put(SetRef, #{}),
    ok.

%% UnionWith: adds all items from Other into this set
union_with(SetRef, OtherRef) ->
    Map = get(SetRef),
    OtherMap = other_to_map(OtherRef),
    put(SetRef, maps:merge(Map, OtherMap)),
    ok.

%% IntersectWith: keeps only items that are also in Other
intersect_with(SetRef, OtherRef) ->
    Map = get(SetRef),
    OtherMap = other_to_map(OtherRef),
    NewMap = maps:filter(fun(K, _V) -> maps:is_key(K, OtherMap) end, Map),
    put(SetRef, NewMap),
    ok.

%% ExceptWith: removes all items that are in Other
except_with(SetRef, OtherRef) ->
    Map = get(SetRef),
    OtherMap = other_to_map(OtherRef),
    NewMap = maps:filter(fun(K, _V) -> not maps:is_key(K, OtherMap) end, Map),
    put(SetRef, NewMap),
    ok.

symmetric_except_with(SetRef, OtherRef) ->
    Map = get(SetRef),
    OtherMap = other_to_map(OtherRef),

    NewMap = maps:fold(
        fun(K, _V, Acc) ->
            case maps:is_key(K, Acc) of
                true -> maps:remove(K, Acc);
                false -> maps:put(K, true, Acc)
            end
        end,
        Map,
        OtherMap
    ),

    put(SetRef, NewMap),
    ok.

is_subset_of(SetRef, OtherRef) ->
    Map = get(SetRef),
    OtherMap = other_to_map(OtherRef),
    maps:fold(fun(K, _V, Acc) -> Acc andalso maps:is_key(K, OtherMap) end, true, Map).

is_superset_of(SetRef, OtherRef) ->
    is_subset_of(OtherRef, SetRef).

is_proper_subset_of(SetRef, OtherRef) ->
    is_subset_of(SetRef, OtherRef) andalso (get_count(SetRef) < get_count(OtherRef)).

is_proper_superset_of(SetRef, OtherRef) ->
    is_proper_subset_of(OtherRef, SetRef).

overlaps(SetRef, OtherRef) ->
    Map = get(SetRef),
    OtherMap = other_to_map(OtherRef),
    maps:fold(fun(K, _V, Acc) -> Acc orelse maps:is_key(K, OtherMap) end, false, Map).

set_equals(SetRef, OtherRef) ->
    Map = get(SetRef),
    OtherMap = other_to_map(OtherRef),
    maps:size(Map) =:= maps:size(OtherMap) andalso is_subset_of(SetRef, OtherRef).

%% CopyTo: copy elements into a target array (process-dict ref)
copy_to(SetRef, ArrRef) ->
    Items = maps:keys(get(SetRef)),
    Target = get(ArrRef),
    Count = length(Items),
    Blitted = fable_resize_array:blit(Items, 0, Target, 0, Count),
    put(ArrRef, Blitted),
    ok.

copy_to(SetRef, ArrRef, TargetIndex) ->
    Items = maps:keys(get(SetRef)),
    Target = get(ArrRef),
    Count = length(Items),
    Blitted = fable_resize_array:blit(Items, 0, Target, TargetIndex, Count),
    put(ArrRef, Blitted),
    ok.

copy_to(SetRef, ArrRef, TargetIndex, Count) ->
    Items = maps:keys(get(SetRef)),
    Target = get(ArrRef),
    Blitted = fable_resize_array:blit(Items, 0, Target, TargetIndex, Count),
    put(ArrRef, Blitted),
    ok.

%% GetEnumerator: return list of items for iteration
get_enumerator(SetRef) ->
    fable_utils:get_enumerator(maps:keys(get(SetRef))).
