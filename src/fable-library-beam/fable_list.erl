-module(fable_list).
-export([fold/3, fold_back/3, reduce/2, reduce_back/2, map_indexed/2,
         sort_by/2, sort_by_descending/2, sort_with/2,
         find/2, try_find/2, find_index/2, try_find_index/2,
         find_back/2, try_find_back/2,
         find_index_back/2, try_find_index_back/2,
         choose/2, collect/2,
         sum_by/2, min_by/2, max_by/2, indexed/1, zip/2, zip3/3,
         init/2, replicate/2, scan/3, scan_back/3,
         try_head/1, try_last/1, try_item/2, exactly_one/1, try_exactly_one/1,
         distinct/1, distinct_by/2, pairwise/1,
         exists2/3, forall2/3, map2/3, map3/4, mapi2/3,
         iter2/3, iteri/2, iteri2/3, iter_indexed/2,
         average/1, average_by/2, count_by/2, group_by/2,
         unfold/2, split_at/2, chunk_by_size/2, windowed/2,
         split_into/2, except/2, all_pairs/2, permute/2,
         map_fold/3, map_fold_back/3, pick/2, try_pick/2,
         transpose/1, compare_with/3,
         update_at/3, insert_at/3, insert_many_at/3,
         remove_at/2, remove_many_at/3,
         index_of_value/2]).

%% Fable compiles multi-arg F# lambdas as curried functions:
%%   fun acc x -> acc + x  =>  fun(Acc) -> fun(X) -> Acc + X end end
%% So we use curried application: (Fn(Acc))(Item) instead of Fn(Acc, Item).

fold(Fn, State, List) ->
    lists:foldl(fun(Item, Acc) -> (Fn(Acc))(Item) end, State, List).

fold_back(Fn, List, State) ->
    lists:foldr(fun(Item, Acc) -> (Fn(Item))(Acc) end, State, List).

reduce(Fn, List) ->
    lists:foldl(fun(Item, Acc) -> (Fn(Acc))(Item) end, hd(List), tl(List)).

reduce_back(Fn, List) ->
    lists:foldr(fun(Item, Acc) -> (Fn(Item))(Acc) end, lists:last(List), lists:droplast(List)).

map_indexed(Fn, List) ->
    element(1, lists:mapfoldl(fun(E, I) -> {(Fn(I))(E), I + 1} end, 0, List)).

sort_by(Fn, List) ->
    lists:sort(fun(A, B) -> Fn(A) =< Fn(B) end, List).

sort_by_descending(Fn, List) ->
    lists:reverse(lists:sort(fun(A, B) -> Fn(A) =< Fn(B) end, List)).

sort_with(Fn, List) ->
    lists:sort(fun(A, B) -> (Fn(A))(B) =< 0 end, List).

find(Fn, List) ->
    case lists:dropwhile(fun(E) -> not Fn(E) end, List) of
        [H|_] -> H;
        [] -> erlang:error(<<"key_not_found">>)
    end.

try_find(Fn, List) ->
    case lists:dropwhile(fun(E) -> not Fn(E) end, List) of
        [H|_] -> H;
        [] -> undefined
    end.

find_index(Fn, List) ->
    find_index(Fn, List, 0).
find_index(Fn, [H|T], I) ->
    case Fn(H) of true -> I; false -> find_index(Fn, T, I + 1) end;
find_index(_Fn, [], _I) ->
    erlang:error(<<"key_not_found">>).

try_find_index(Fn, List) ->
    try_find_index(Fn, List, 0).
try_find_index(Fn, [H|T], I) ->
    case Fn(H) of true -> I; false -> try_find_index(Fn, T, I + 1) end;
try_find_index(_Fn, [], _I) ->
    undefined.

find_back(Fn, List) ->
    find(Fn, lists:reverse(List)).

try_find_back(Fn, List) ->
    try_find(Fn, lists:reverse(List)).

choose(Fn, List) ->
    lists:filtermap(fun(E) -> case Fn(E) of undefined -> false; V -> {true, V} end end, List).

collect(Fn, List) ->
    lists:append(lists:map(Fn, List)).

sum_by(Fn, List) ->
    lists:sum(lists:map(Fn, List)).

min_by(Fn, List) ->
    element(2, lists:min(lists:map(fun(E) -> {Fn(E), E} end, List))).

max_by(Fn, List) ->
    element(2, lists:max(lists:map(fun(E) -> {Fn(E), E} end, List))).

indexed(List) ->
    lists:zip(lists:seq(0, length(List) - 1), List).

zip(L1, L2) ->
    lists:zipwith(fun(A, B) -> {A, B} end, L1, L2).

zip3(L1, L2, L3) ->
    lists:zipwith3(fun(A, B, C) -> {A, B, C} end, L1, L2, L3).

init(Count, Fn) ->
    lists:map(Fn, lists:seq(0, Count - 1)).

replicate(Count, Value) ->
    lists:duplicate(Count, Value).

scan(Fn, State, List) ->
    {Result, _} = lists:mapfoldl(fun(Item, Acc) ->
        New = (Fn(Acc))(Item),
        {New, New}
    end, State, List),
    [State | Result].

scan_back(Fn, List, State) ->
    {Result, _} = lists:mapfoldr(fun(Item, Acc) ->
        New = (Fn(Item))(Acc),
        {New, New}
    end, State, List),
    Result ++ [State].

try_head([H|_]) -> H;
try_head([]) -> undefined.

try_last([]) -> undefined;
try_last(List) -> lists:last(List).

try_item(Index, List) when Index >= 0, Index < length(List) ->
    lists:nth(Index + 1, List);
try_item(_, _) ->
    undefined.

exactly_one([X]) -> X;
exactly_one(_) -> erlang:error(<<"Sequence contains more than one element">>).

try_exactly_one([X]) -> X;
try_exactly_one(_) -> undefined.

distinct(List) ->
    distinct(List, #{}).
distinct([], _Seen) -> [];
distinct([H|T], Seen) ->
    case maps:is_key(H, Seen) of
        true -> distinct(T, Seen);
        false -> [H | distinct(T, Seen#{H => true})]
    end.

distinct_by(Fn, List) ->
    distinct_by(Fn, List, #{}).
distinct_by(_Fn, [], _Seen) -> [];
distinct_by(Fn, [H|T], Seen) ->
    Key = Fn(H),
    case maps:is_key(Key, Seen) of
        true -> distinct_by(Fn, T, Seen);
        false -> [H | distinct_by(Fn, T, Seen#{Key => true})]
    end.

pairwise([]) -> [];
pairwise([_]) -> [];
pairwise([A,B|T]) -> [{A, B} | pairwise([B|T])].

exists2(Fn, L1, L2) ->
    lists:any(fun({A, B}) -> (Fn(A))(B) end, lists:zip(L1, L2)).

forall2(Fn, L1, L2) ->
    lists:all(fun({A, B}) -> (Fn(A))(B) end, lists:zip(L1, L2)).

map2(Fn, L1, L2) ->
    lists:zipwith(fun(A, B) -> (Fn(A))(B) end, L1, L2).

map3(Fn, L1, L2, L3) ->
    lists:zipwith3(fun(A, B, C) -> ((Fn(A))(B))(C) end, L1, L2, L3).

mapi2(Fn, L1, L2) ->
    Zipped = lists:zip(L1, L2),
    element(1, lists:mapfoldl(fun({A, B}, I) -> {((Fn(I))(A))(B), I + 1} end, 0, Zipped)).

iter2(Fn, L1, L2) ->
    lists:foreach(fun({A, B}) -> (Fn(A))(B) end, lists:zip(L1, L2)).

iteri(Fn, List) ->
    lists:foldl(fun(E, I) -> (Fn(I))(E), I + 1 end, 0, List),
    ok.

iteri2(Fn, L1, L2) ->
    Zipped = lists:zip(L1, L2),
    lists:foldl(fun({A, B}, I) -> ((Fn(I))(A))(B), I + 1 end, 0, Zipped),
    ok.

iter_indexed(Fn, List) -> iteri(Fn, List).

average(List) ->
    lists:sum(List) / length(List).

average_by(Fn, List) ->
    lists:sum(lists:map(Fn, List)) / length(List).

count_by(Fn, List) ->
    Grouped = lists:foldl(fun(E, Acc) ->
        Key = Fn(E),
        Count = maps:get(Key, Acc, 0),
        Acc#{Key => Count + 1}
    end, #{}, List),
    maps:to_list(Grouped).

group_by(Fn, List) ->
    Grouped = lists:foldl(fun(E, Acc) ->
        Key = Fn(E),
        Vals = maps:get(Key, Acc, []),
        Acc#{Key => Vals ++ [E]}
    end, #{}, List),
    maps:to_list(Grouped).

unfold(Fn, State) ->
    case Fn(State) of
        undefined -> [];
        {A, B} -> [A | unfold(Fn, B)]
    end.

split_at(Index, List) ->
    {lists:sublist(List, Index), lists:nthtail(Index, List)}.

chunk_by_size(Size, List) ->
    chunk_by_size(Size, List, []).
chunk_by_size(_Size, [], Acc) -> lists:reverse(Acc);
chunk_by_size(Size, List, Acc) ->
    {Chunk, Rest} = case length(List) >= Size of
        true -> {lists:sublist(List, Size), lists:nthtail(Size, List)};
        false -> {List, []}
    end,
    chunk_by_size(Size, Rest, [Chunk | Acc]).

windowed(Size, List) ->
    windowed(Size, List, []).
windowed(Size, List, Acc) when length(List) < Size -> lists:reverse(Acc);
windowed(Size, [_|T] = List, Acc) ->
    Window = lists:sublist(List, Size),
    windowed(Size, T, [Window | Acc]).

split_into(Count, List) ->
    Len = length(List),
    ActualCount = min(Count, Len),
    Base = Len div ActualCount,
    Extra = Len rem ActualCount,
    split_into(ActualCount, List, Base, Extra, []).
split_into(0, _, _, _, Acc) -> lists:reverse(Acc);
split_into(N, List, Base, Extra, Acc) ->
    Size = Base + (case Extra > 0 of true -> 1; false -> 0 end),
    {Chunk, Rest} = {lists:sublist(List, Size), lists:nthtail(Size, List)},
    split_into(N - 1, Rest, Base, max(Extra - 1, 0), [Chunk | Acc]).

except(ExceptList, List) ->
    ExceptSet = maps:from_list([{E, true} || E <- ExceptList]),
    [E || E <- List, not maps:is_key(E, ExceptSet)].

all_pairs(L1, L2) ->
    [{A, B} || A <- L1, B <- L2].

permute(IndexFn, List) ->
    Arr = list_to_tuple(List),
    Len = tuple_size(Arr),
    Result = erlang:make_tuple(Len, undefined),
    Result2 = lists:foldl(fun(I, Acc) ->
        TargetIdx = IndexFn(I),
        setelement(TargetIdx + 1, Acc, element(I + 1, Arr))
    end, Result, lists:seq(0, Len - 1)),
    tuple_to_list(Result2).

map_fold(Fn, State, List) ->
    lists:mapfoldl(fun(Item, Acc) ->
        {Mapped, NewAcc} = (Fn(Acc))(Item),
        {Mapped, NewAcc}
    end, State, List).

map_fold_back(Fn, List, State) ->
    lists:mapfoldr(fun(Item, Acc) ->
        {Mapped, NewAcc} = (Fn(Item))(Acc),
        {Mapped, NewAcc}
    end, State, List).

pick(Fn, List) ->
    case try_pick(Fn, List) of
        undefined -> erlang:error(<<"key_not_found">>);
        V -> V
    end.

try_pick(Fn, []) -> undefined;
try_pick(Fn, [H|T]) ->
    case Fn(H) of
        undefined -> try_pick(Fn, T);
        V -> V
    end.

find_index_back(Fn, List) ->
    case try_find_index_back(Fn, List) of
        undefined -> erlang:error(<<"key_not_found">>);
        Idx -> Idx
    end.

try_find_index_back(Fn, List) ->
    try_find_index_back(Fn, lists:reverse(List), length(List) - 1).
try_find_index_back(_Fn, [], _Idx) -> undefined;
try_find_index_back(Fn, [H|T], Idx) ->
    case Fn(H) of
        true -> Idx;
        false -> try_find_index_back(Fn, T, Idx - 1)
    end.

transpose([]) -> [];
transpose([[] | _]) -> [];
transpose(Lists) ->
    Heads = [hd(L) || L <- Lists],
    Tails = [tl(L) || L <- Lists],
    [Heads | transpose(Tails)].

compare_with(_Fn, [], []) -> 0;
compare_with(_Fn, [], _) -> -1;
compare_with(_Fn, _, []) -> 1;
compare_with(Fn, [H1|T1], [H2|T2]) ->
    case (Fn(H1))(H2) of
        0 -> compare_with(Fn, T1, T2);
        N -> N
    end.

update_at(Idx, Value, List) ->
    {Before, [_|After]} = lists:split(Idx, List),
    Before ++ [Value | After].

insert_at(Idx, Value, List) ->
    {Before, After} = lists:split(Idx, List),
    Before ++ [Value | After].

insert_many_at(Idx, Values, List) ->
    {Before, After} = lists:split(Idx, List),
    Before ++ Values ++ After.

remove_at(Idx, List) ->
    {Before, [_|After]} = lists:split(Idx, List),
    Before ++ After.

remove_many_at(Idx, Count, List) ->
    {Before, Rest} = lists:split(Idx, List),
    After = lists:nthtail(Count, Rest),
    Before ++ After.

index_of_value(Value, List) ->
    index_of_value(Value, List, 0).
index_of_value(_Value, [], _Idx) -> -1;
index_of_value(Value, [Value|_], Idx) -> Idx;
index_of_value(Value, [_|T], Idx) -> index_of_value(Value, T, Idx + 1).
