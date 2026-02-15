-module(fable_resize_array).
-export([
    set_item/3,
    remove/2, remove_all/2, remove_at/2,
    insert/3, insert_range/3,
    get_range/3,
    index_of/2, index_of/3,
    find/3, find_last/2, find_all/2, find_index/2, find_last_index/2,
    exists/2,
    sort_with/2,
    for_each/2,
    convert_all/2
]).

%% Replace item at index, return new list (or binary)
set_item(Bin, Index, Value) when is_binary(Bin) ->
    <<Before:Index/binary, _:1/binary, After/binary>> = Bin,
    <<Before/binary, Value:8, After/binary>>;
set_item(List, Index, Value) ->
    {Before, [_ | After]} = lists:split(Index, List),
    Before ++ [Value | After].

%% Remove first occurrence, return {Removed, NewList}
remove(List, Item) ->
    remove_first(List, Item, []).

remove_first([], _Item, _Acc) ->
    {false, lists:reverse(_Acc)};
remove_first([Item | Rest], Item, Acc) ->
    {true, lists:reverse(Acc) ++ Rest};
remove_first([H | Rest], Item, Acc) ->
    remove_first(Rest, Item, [H | Acc]).

%% Remove all matching predicate, return count removed
remove_all(List, Pred) ->
    {Kept, Count} = lists:foldl(fun(X, {Acc, C}) ->
        case Pred(X) of
            true -> {Acc, C + 1};
            false -> {[X | Acc], C}
        end
    end, {[], 0}, List),
    {Count, lists:reverse(Kept)}.

%% Remove at index
remove_at(List, Index) ->
    {Before, [_ | After]} = lists:split(Index, List),
    Before ++ After.

%% Insert item at index
insert(List, Index, Item) ->
    {Before, After} = lists:split(Index, List),
    Before ++ [Item | After].

%% Insert range at index
insert_range(List, Index, Items) ->
    {Before, After} = lists:split(Index, List),
    Before ++ Items ++ After.

%% Get range: sublist by index and count
get_range(List, Index, Count) ->
    lists:sublist(List, Index + 1, Count).

%% Index of item (-1 if not found)
index_of(List, Item) ->
    index_of_impl(List, Item, 0).

index_of(List, Item, StartIndex) ->
    SubList = lists:nthtail(StartIndex, List),
    case index_of_impl(SubList, Item, 0) of
        -1 -> -1;
        Idx -> Idx + StartIndex
    end.

index_of_impl([], _Item, _Idx) -> -1;
index_of_impl([Item | _], Item, Idx) -> Idx;
index_of_impl([_ | Rest], Item, Idx) -> index_of_impl(Rest, Item, Idx + 1).

%% Find first matching predicate, return Default when not found
find(_Pred, [], Default) -> Default;
find(Pred, [H | T], Default) ->
    case Pred(H) of
        true -> H;
        false -> find(Pred, T, Default)
    end.

%% Find last matching predicate
find_last(List, Pred) ->
    find_last_impl(lists:reverse(List), Pred).

find_last_impl([], _Pred) -> erlang:error(<<"Key not found">>);
find_last_impl([H | T], Pred) ->
    case Pred(H) of
        true -> H;
        false -> find_last_impl(T, Pred)
    end.

%% Find all matching predicate
find_all(List, Pred) ->
    lists:filter(Pred, List).

%% Find index of first matching predicate (-1 if not found)
find_index(List, Pred) ->
    find_index_impl(List, Pred, 0).

find_index_impl([], _Pred, _Idx) -> -1;
find_index_impl([H | T], Pred, Idx) ->
    case Pred(H) of
        true -> Idx;
        false -> find_index_impl(T, Pred, Idx + 1)
    end.

%% Find last index matching predicate (-1 if not found)
find_last_index(List, Pred) ->
    find_last_index_impl(lists:reverse(List), Pred, length(List) - 1).

find_last_index_impl([], _Pred, _Idx) -> -1;
find_last_index_impl([H | T], Pred, Idx) ->
    case Pred(H) of
        true -> Idx;
        false -> find_last_index_impl(T, Pred, Idx - 1)
    end.

%% Exists: any element matches predicate
exists(List, Pred) ->
    lists:any(Pred, List).

%% Sort with comparison function (Comparison<T> delegate = 2-arg function)
sort_with(List, CompareFn) ->
    lists:sort(fun(A, B) ->
        case CompareFn(A, B) of
            R when R =< 0 -> true;
            _ -> false
        end
    end, List).

%% ForEach
for_each(List, Fn) ->
    lists:foreach(Fn, List).

%% ConvertAll (map)
convert_all(List, Fn) ->
    lists:map(Fn, List).
