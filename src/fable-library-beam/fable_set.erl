-module(fable_set).
-export([fold/3, fold_back/3, map/2, filter/2, exists/2, forall/2,
         iterate/2, partition/2, union_many/1, intersect_many/1,
         is_proper_subset/2, is_proper_superset/2]).

%% Fable compiles multi-arg F# lambdas as curried functions:
%%   fun acc elem -> ...  =>  fun(Acc) -> fun(Elem) -> ... end end
%% So we use curried application: (Fn(Acc))(Elem) instead of Fn(Acc, Elem).

fold(Fn, State, Set) ->
    lists:foldl(fun(E, Acc) -> (Fn(Acc))(E) end, State, Set).

fold_back(Fn, Set, State) ->
    lists:foldr(fun(E, Acc) -> (Fn(E))(Acc) end, State, Set).

map(Fn, Set) ->
    ordsets:from_list(lists:map(Fn, Set)).

filter(Fn, Set) ->
    ordsets:filter(Fn, Set).

exists(Fn, Set) ->
    lists:any(Fn, Set).

forall(Fn, Set) ->
    lists:all(Fn, Set).

iterate(Fn, Set) ->
    lists:foreach(Fn, Set).

partition(Fn, Set) ->
    lists:partition(Fn, Set).

union_many(Sets) ->
    lists:foldl(fun ordsets:union/2, [], Sets).

intersect_many([H|T]) ->
    lists:foldl(fun ordsets:intersection/2, H, T).

is_proper_subset(A, B) ->
    ordsets:is_subset(A, B) andalso (A =/= B).

is_proper_superset(A, B) ->
    ordsets:is_subset(B, A) andalso (A =/= B).
