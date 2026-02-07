-module(fable_list).
-export([fold/3, fold_back/3, reduce/2, map_indexed/2,
         sort_by/2, sort_by_descending/2, sort_with/2,
         find/2, try_find/2, choose/2, collect/2,
         sum_by/2, min_by/2, max_by/2, indexed/1, zip/2]).

%% Fable compiles multi-arg F# lambdas as curried functions:
%%   fun acc x -> acc + x  =>  fun(Acc) -> fun(X) -> Acc + X end end
%% So we use curried application: (Fn(Acc))(Item) instead of Fn(Acc, Item).

fold(Fn, State, List) ->
    lists:foldl(fun(Item, Acc) -> (Fn(Acc))(Item) end, State, List).

fold_back(Fn, List, State) ->
    lists:foldr(fun(Item, Acc) -> (Fn(Item))(Acc) end, State, List).

reduce(Fn, List) ->
    lists:foldl(fun(Item, Acc) -> (Fn(Acc))(Item) end, hd(List), tl(List)).

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
