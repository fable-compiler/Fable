-module(fable_seq).
-export([delay/1, singleton/1, unfold/2,
         initialize/2, take/2, skip/2,
         take_while/2, skip_while/2, truncate/2,
         pairwise/1, windowed/2, chunk_by_size/2,
         except/2, distinct/1, distinct_by/2,
         group_by/2, count_by/2,
         item/2, head/1, last/1,
         find_index/2, try_find_index/2,
         map2/3, map_indexed/2, map_indexed2/3,
         iter_indexed/2, iter2/3,
         fold2/4, fold_back2/4,
         scan/3, scan_back/3,
         reduce_back/2,
         for_all2/3, exists2/3,
         compare_with/3,
         zip3/3, pick/2, try_pick/2]).

%% Fable compiles multi-arg F# lambdas as curried functions:
%%   fun acc x -> acc + x  =>  fun(Acc) -> fun(X) -> Acc + X end end
%% So we use curried application: (Fn(Acc))(Item) instead of Fn(Acc, Item).

%% Seq-specific operations (not 1:1 with lists:*)

delay(Fn) ->
    %% Eager evaluation: call the thunk immediately to produce a list
    Fn(ok).

singleton(X) -> [X].

unfold(Fn, State) ->
    case Fn(State) of
        undefined -> [];
        {Value, Next} -> [Value | unfold(Fn, Next)]
    end.

initialize(Count, Fn) ->
    lists:map(Fn, lists:seq(0, Count - 1)).

take(N, List) ->
    lists:sublist(List, N).

skip(N, List) ->
    lists:nthtail(N, List).

truncate(N, List) ->
    lists:sublist(List, N).

take_while(Fn, List) ->
    lists:takewhile(Fn, List).

skip_while(Fn, List) ->
    lists:dropwhile(Fn, List).

pairwise([]) -> [];
pairwise([_]) -> [];
pairwise(List) ->
    lists:zip(lists:droplast(List), tl(List)).

windowed(N, List) ->
    windowed_acc(N, List, []).

windowed_acc(N, List, Acc) ->
    case length(List) >= N of
        true -> windowed_acc(N, tl(List), [lists:sublist(List, N) | Acc]);
        false -> lists:reverse(Acc)
    end.

chunk_by_size(N, List) ->
    chunk_acc(N, List, []).

chunk_acc(_N, [], Acc) ->
    lists:reverse(Acc);
chunk_acc(N, List, Acc) ->
    {Chunk, Rest} = take_and_drop(N, List),
    chunk_acc(N, Rest, [Chunk | Acc]).

take_and_drop(N, List) ->
    {lists:sublist(List, N), safe_nthtail(N, List)}.

safe_nthtail(N, List) ->
    case length(List) =< N of
        true -> [];
        false -> lists:nthtail(N, List)
    end.

distinct(List) ->
    distinct_acc(List, #{}, []).

distinct_acc([], _Seen, Acc) ->
    lists:reverse(Acc);
distinct_acc([H|T], Seen, Acc) ->
    case maps:is_key(H, Seen) of
        true -> distinct_acc(T, Seen, Acc);
        false -> distinct_acc(T, maps:put(H, true, Seen), [H | Acc])
    end.

distinct_by(Fn, List) ->
    distinct_by_acc(Fn, List, #{}, []).

distinct_by_acc(_Fn, [], _Seen, Acc) ->
    lists:reverse(Acc);
distinct_by_acc(Fn, [H|T], Seen, Acc) ->
    Key = Fn(H),
    case maps:is_key(Key, Seen) of
        true -> distinct_by_acc(Fn, T, Seen, Acc);
        false -> distinct_by_acc(Fn, T, maps:put(Key, true, Seen), [H | Acc])
    end.

except(Excluded, List) ->
    ExSet = maps:from_list([{E, true} || E <- Excluded]),
    [X || X <- List, not maps:is_key(X, ExSet)].

group_by(Fn, List) ->
    Map = lists:foldl(
        fun(X, Acc) ->
            Key = Fn(X),
            case maps:find(Key, Acc) of
                {ok, Vals} -> maps:put(Key, Vals ++ [X], Acc);
                error -> maps:put(Key, [X], Acc)
            end
        end,
        #{},
        List),
    maps:to_list(Map).

count_by(Fn, List) ->
    Map = lists:foldl(
        fun(X, Acc) ->
            Key = Fn(X),
            case maps:find(Key, Acc) of
                {ok, Count} -> maps:put(Key, Count + 1, Acc);
                error -> maps:put(Key, 1, Acc)
            end
        end,
        #{},
        List),
    maps:to_list(Map).

item(Index, List) ->
    lists:nth(Index + 1, List).

head([H|_]) -> H.

last(List) -> lists:last(List).

find_index(Fn, List) ->
    find_index_acc(Fn, List, 0).

find_index_acc(_Fn, [], _I) ->
    erlang:error(<<"key_not_found">>);
find_index_acc(Fn, [H|T], I) ->
    case Fn(H) of
        true -> I;
        false -> find_index_acc(Fn, T, I + 1)
    end.

try_find_index(Fn, List) ->
    try_find_index_acc(Fn, List, 0).

try_find_index_acc(_Fn, [], _I) ->
    undefined;
try_find_index_acc(Fn, [H|T], I) ->
    case Fn(H) of
        true -> I;
        false -> try_find_index_acc(Fn, T, I + 1)
    end.

map2(Fn, L1, L2) ->
    lists:zipwith(fun(A, B) -> (Fn(A))(B) end, L1, L2).

map_indexed(Fn, List) ->
    element(1, lists:mapfoldl(fun(E, I) -> {(Fn(I))(E), I + 1} end, 0, List)).

map_indexed2(Fn, L1, L2) ->
    Zipped = lists:zip(L1, L2),
    element(1, lists:mapfoldl(fun({A, B}, I) -> {((Fn(I))(A))(B), I + 1} end, 0, Zipped)).

iter_indexed(Fn, List) ->
    iter_indexed_acc(Fn, List, 0).

iter_indexed_acc(_Fn, [], _I) -> ok;
iter_indexed_acc(Fn, [H|T], I) ->
    (Fn(I))(H),
    iter_indexed_acc(Fn, T, I + 1).

iter2(Fn, L1, L2) ->
    lists:foreach(fun({A, B}) -> (Fn(A))(B) end, lists:zip(L1, L2)).

fold2(Fn, State, L1, L2) ->
    lists:foldl(fun({A, B}, Acc) -> ((Fn(Acc))(A))(B) end, State, lists:zip(L1, L2)).

fold_back2(Fn, L1, L2, State) ->
    lists:foldr(fun({A, B}, Acc) -> ((Fn(A))(B))(Acc) end, State, lists:zip(L1, L2)).

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

reduce_back(Fn, List) ->
    [Last | Rest] = lists:reverse(List),
    lists:foldr(fun(Item, Acc) -> (Fn(Item))(Acc) end, Last, lists:reverse(Rest)).

for_all2(Fn, L1, L2) ->
    lists:all(fun({A, B}) -> (Fn(A))(B) end, lists:zip(L1, L2)).

exists2(Fn, L1, L2) ->
    lists:any(fun({A, B}) -> (Fn(A))(B) end, lists:zip(L1, L2)).

compare_with(Fn, L1, L2) ->
    compare_with_acc(Fn, L1, L2).

compare_with_acc(_Fn, [], []) -> 0;
compare_with_acc(_Fn, [], _) -> -1;
compare_with_acc(_Fn, _, []) -> 1;
compare_with_acc(Fn, [H1|T1], [H2|T2]) ->
    case (Fn(H1))(H2) of
        0 -> compare_with_acc(Fn, T1, T2);
        N -> N
    end.

zip3(L1, L2, L3) ->
    lists:zipwith3(fun(A, B, C) -> {A, B, C} end, L1, L2, L3).

pick(Fn, List) ->
    case try_pick(Fn, List) of
        undefined -> erlang:error(<<"key_not_found">>);
        V -> V
    end.

try_pick(_Fn, []) -> undefined;
try_pick(Fn, [H|T]) ->
    case Fn(H) of
        undefined -> try_pick(Fn, T);
        V -> V
    end.
