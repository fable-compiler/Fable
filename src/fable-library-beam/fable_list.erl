-module(fable_list).
-compile({no_auto_import, [ref_to_list/1]}).
-export([
    fold/3,
    fold_back/3,
    fold2/4,
    fold_back2/4,
    reduce/2,
    reduce_back/2,
    map_indexed/2,
    sort_by/2,
    sort_by/3,
    sort_by_descending/2,
    sort_by_descending/3,
    sort_with/2,
    find/2,
    try_find/2,
    find_index/2,
    try_find_index/2,
    find_back/2,
    try_find_back/2,
    find_index_back/2,
    try_find_index_back/2,
    choose/2,
    collect/2,
    sum_by/2,
    min_by/2,
    min_by/3,
    max_by/2,
    max_by/3,
    min_with/2,
    max_with/2,
    indexed/1,
    zip/2,
    zip3/3,
    init/2,
    replicate/2,
    scan/3,
    scan_back/3,
    try_head/1,
    try_last/1,
    try_item/2,
    exactly_one/1,
    try_exactly_one/1,
    distinct/1,
    distinct_by/2,
    pairwise/1,
    exists2/3,
    forall2/3,
    array_exists2/3,
    array_forall2/3,
    map2/3,
    map3/4,
    mapi2/3,
    iter2/3,
    iteri/2,
    iteri2/3,
    iter_indexed/2,
    average/1,
    average_by/2,
    count_by/2,
    group_by/2,
    unfold/2,
    split_at/2,
    chunk_by_size/2,
    windowed/2,
    split_into/2,
    except/2,
    all_pairs/2,
    permute/2,
    map_fold/3,
    map_fold_back/3,
    pick/2,
    try_pick/2,
    transpose/1,
    compare_with/3,
    update_at/3,
    insert_at/3,
    insert_many_at/3,
    remove_at/2,
    remove_many_at/3,
    index_of_value/2,
    get_slice/3,
    sub/3,
    blit/5,
    take/2,
    random_shuffle_by/2,
    random_shuffle_with/2,
    random_shuffle/1,
    random_choice_by/2,
    random_choice_with/2,
    random_choice/1,
    random_choices_by/3,
    random_choices_with/3,
    random_choices/2,
    random_sample_by/3,
    random_sample_with/3,
    random_sample/2
]).

-spec fold(fun(), term(), list() | reference() | map()) -> term().
-spec fold_back(fun(), list(), term()) -> term().
-spec fold2(fun(), term(), list(), list()) -> term().
-spec fold_back2(fun(), list(), list(), term()) -> term().
-spec reduce(fun(), list()) -> term().
-spec reduce_back(fun(), list()) -> term().
-spec map_indexed(fun(), list()) -> list().
-spec sort_by(fun(), list()) -> list().
-spec sort_by_descending(fun(), list()) -> list().
-spec sort_with(fun(), list()) -> list().
-spec find(fun(), list()) -> term().
-spec try_find(fun(), list()) -> term() | undefined.
-spec find_index(fun(), list()) -> non_neg_integer().
-spec try_find_index(fun(), list()) -> non_neg_integer() | undefined.
-spec find_back(fun(), list()) -> term().
-spec try_find_back(fun(), list()) -> term() | undefined.
-spec find_index_back(fun(), list()) -> non_neg_integer().
-spec try_find_index_back(fun(), list()) -> non_neg_integer() | undefined.
-spec choose(fun(), list()) -> list().
-spec collect(fun(), list()) -> list().
-spec sum_by(fun(), list()) -> number().
-spec min_by(fun(), list()) -> term().
-spec max_by(fun(), list()) -> term().
-spec indexed(list()) -> list({non_neg_integer(), term()}).
-spec zip(list(), list()) -> list({term(), term()}).
-spec zip3(list(), list(), list()) -> list({term(), term(), term()}).
-spec init(non_neg_integer(), fun()) -> list().
-spec replicate(non_neg_integer(), term()) -> list().
-spec scan(fun(), term(), list()) -> list().
-spec scan_back(fun(), list(), term()) -> list().
-spec try_head(list()) -> term() | undefined.
-spec try_last(list()) -> term() | undefined.
-spec try_item(non_neg_integer(), list()) -> term() | undefined.
-spec exactly_one(list()) -> term().
-spec try_exactly_one(list()) -> term() | undefined.
-spec distinct(list()) -> list().
-spec distinct_by(fun(), list()) -> list().
-spec pairwise(list()) -> list({term(), term()}).
-spec exists2(fun(), list(), list()) -> boolean().
-spec forall2(fun(), list(), list()) -> boolean().
-spec array_exists2(fun(), list(), list()) -> boolean().
-spec array_forall2(fun(), list(), list()) -> boolean().
-spec map2(fun(), list(), list()) -> list().
-spec map3(fun(), list(), list(), list()) -> list().
-spec mapi2(fun(), list(), list()) -> list().
-spec iter2(fun(), list(), list()) -> ok.
-spec iteri(fun(), list()) -> ok.
-spec iteri2(fun(), list(), list()) -> ok.
-spec iter_indexed(fun(), list()) -> ok.
-spec average(list()) -> number().
-spec average_by(fun(), list()) -> number().
-spec count_by(fun(), list()) -> list({term(), non_neg_integer()}).
-spec group_by(fun(), list()) -> list({term(), list()}).
-spec unfold(fun(), term()) -> list().
-spec split_at(non_neg_integer(), list()) -> {list(), list()}.
-spec chunk_by_size(integer(), list()) -> list(list()).
-spec windowed(integer(), list()) -> list(list()).
-spec split_into(integer(), list()) -> list(list()).
-spec except(list(), list()) -> list().
-spec all_pairs(list(), list()) -> list({term(), term()}).
-spec permute(fun(), list()) -> list().
-spec map_fold(fun(), term(), list()) -> {list(), term()}.
-spec map_fold_back(fun(), list(), term()) -> {list(), term()}.
-spec pick(fun(), list()) -> term().
-spec try_pick(fun(), list()) -> term() | undefined.
-spec transpose(list(list())) -> list(list()).
-spec compare_with(fun(), list(), list()) -> integer().
-spec update_at(non_neg_integer(), term(), list()) -> list().
-spec insert_at(non_neg_integer(), term(), list()) -> list().
-spec insert_many_at(non_neg_integer(), list(), list()) -> list().
-spec remove_at(non_neg_integer(), list()) -> list().
-spec remove_many_at(non_neg_integer(), non_neg_integer(), list()) -> list().
-spec index_of_value(term(), list()) -> integer().
-spec get_slice(non_neg_integer() | undefined, non_neg_integer() | undefined, list()) -> list().

%% Fable compiles multi-arg F# lambdas as curried functions:
%%   fun acc x -> acc + x  =>  fun(Acc) -> fun(X) -> Acc + X end end
%% So we use curried application: (Fn(Acc))(Item) instead of Fn(Acc, Item).

fold(Fn, State, List) when is_list(List) ->
    lists:foldl(fun(Item, Acc) -> (Fn(Acc))(Item) end, State, List);
fold(Fn, State, Ref) when is_reference(Ref) ->
    %% Process dict ref (HashSet, ResizeArray): extract underlying data
    fold(Fn, State, ref_to_list(Ref));
fold(Fn, State, Map) when is_map(Map) ->
    %% HashSet internal map: iterate over keys
    fold(Fn, State, maps:keys(Map)).

%% Convert a process dict ref to a list for iteration.
ref_to_list(Ref) ->
    Inner = get(Ref),
    case Inner of
        %% ResizeArray
        L when is_list(L) -> L;
        %% HashSet
        M when is_map(M) -> maps:keys(M)
    end.

fold_back(Fn, List, State) ->
    lists:foldr(fun(Item, Acc) -> (Fn(Item))(Acc) end, State, List).

fold2(Fn, State, L1, L2) ->
    Zipped = lists:zip(L1, L2),
    lists:foldl(fun({A, B}, Acc) -> ((Fn(Acc))(A))(B) end, State, Zipped).

fold_back2(Fn, L1, L2, State) ->
    Zipped = lists:zip(L1, L2),
    lists:foldr(fun({A, B}, Acc) -> ((Fn(A))(B))(Acc) end, State, Zipped).

reduce(Fn, List) ->
    lists:foldl(fun(Item, Acc) -> (Fn(Acc))(Item) end, hd(List), tl(List)).

reduce_back(Fn, List) ->
    lists:foldr(fun(Item, Acc) -> (Fn(Item))(Acc) end, lists:last(List), lists:droplast(List)).

map_indexed(Fn, List) ->
    element(1, lists:mapfoldl(fun(E, I) -> {(Fn(I))(E), I + 1} end, 0, List)).

sort_by(Fn, List) ->
    lists:sort(fun(A, B) -> Fn(A) =< Fn(B) end, List).

sort_by(Fn, Comparer, List) ->
    lists:sort(fun(A, B) -> Comparer(Fn(A), Fn(B)) =< 0 end, List).

sort_by_descending(Fn, List) ->
    lists:reverse(lists:sort(fun(A, B) -> Fn(A) =< Fn(B) end, List)).

sort_by_descending(Fn, Comparer, List) ->
    lists:reverse(lists:sort(fun(A, B) -> Comparer(Fn(A), Fn(B)) =< 0 end, List)).

sort_with(Fn, List) ->
    lists:sort(fun(A, B) -> (Fn(A))(B) =< 0 end, List).

index_not_found() ->
    erlang:error(#{
        exn_type => key_not_found_exception,
        message => <<"An index satisfying the predicate was not found in the collection.">>
    }).

find(Fn, List) ->
    case lists:dropwhile(fun(E) -> not Fn(E) end, List) of
        [H | _] -> H;
        [] -> index_not_found()
    end.

try_find(Fn, List) ->
    case lists:dropwhile(fun(E) -> not Fn(E) end, List) of
        [H | _] -> fable_option:some(H);
        [] -> undefined
    end.

find_index(Fn, List) ->
    find_index(Fn, List, 0).
find_index(Fn, [H | T], I) ->
    case Fn(H) of
        true -> I;
        false -> find_index(Fn, T, I + 1)
    end;
find_index(_Fn, [], _I) ->
    index_not_found().

try_find_index(Fn, List) ->
    try_find_index(Fn, List, 0).
try_find_index(Fn, [H | T], I) ->
    case Fn(H) of
        true -> I;
        false -> try_find_index(Fn, T, I + 1)
    end;
try_find_index(_Fn, [], _I) ->
    undefined.

find_back(Fn, List) ->
    find(Fn, lists:reverse(List)).

try_find_back(Fn, List) ->
    try_find(Fn, lists:reverse(List)).

choose(Fn, List) ->
    lists:filtermap(
        fun(E) ->
            case Fn(E) of
                undefined -> false;
                V -> {true, fable_option:unwrap(V)}
            end
        end,
        List
    ).

collect(Fn, List) ->
    lists:append(lists:map(Fn, List)).

sum_by(Fn, List) ->
    lists:sum(lists:map(Fn, List)).

min_by(Fn, List) ->
    element(2, lists:min(lists:map(fun(E) -> {Fn(E), E} end, List))).

min_by(Fn, Comparer, [Head | Tail]) ->
    {_, Result} = lists:foldl(
        fun(Item, {BestKey, Best}) ->
            ItemKey = Fn(Item),
            case Comparer(ItemKey, BestKey) < 0 of
                true -> {ItemKey, Item};
                false -> {BestKey, Best}
            end
        end,
        {Fn(Head), Head},
        Tail
    ),
    Result.

max_by(Fn, List) ->
    element(2, lists:max(lists:map(fun(E) -> {Fn(E), E} end, List))).

max_by(Fn, Comparer, [Head | Tail]) ->
    {_, Result} = lists:foldl(
        fun(Item, {BestKey, Best}) ->
            ItemKey = Fn(Item),
            case Comparer(ItemKey, BestKey) > 0 of
                true -> {ItemKey, Item};
                false -> {BestKey, Best}
            end
        end,
        {Fn(Head), Head},
        Tail
    ),
    Result.

min_with(Comparer, [Head | Tail]) ->
    lists:foldl(
        fun(Item, Best) ->
            case Comparer(Item, Best) < 0 of
                true -> Item;
                false -> Best
            end
        end,
        Head,
        Tail
    ).

max_with(Comparer, [Head | Tail]) ->
    lists:foldl(
        fun(Item, Best) ->
            case Comparer(Item, Best) > 0 of
                true -> Item;
                false -> Best
            end
        end,
        Head,
        Tail
    ).

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
    {Result, _} = lists:mapfoldl(
        fun(Item, Acc) ->
            New = (Fn(Acc))(Item),
            {New, New}
        end,
        State,
        List
    ),
    [State | Result].

scan_back(Fn, List, State) ->
    {Result, _} = lists:mapfoldr(
        fun(Item, Acc) ->
            New = (Fn(Item))(Acc),
            {New, New}
        end,
        State,
        List
    ),
    Result ++ [State].

try_head([H | _]) -> fable_option:some(H);
try_head([]) -> undefined.

try_last([]) -> undefined;
try_last(List) -> fable_option:some(lists:last(List)).

try_item(Index, List) when Index >= 0, Index < length(List) ->
    fable_option:some(lists:nth(Index + 1, List));
try_item(_, _) ->
    undefined.

exactly_one([X]) -> X;
exactly_one([]) -> invalid_arg(<<"source">>, <<"The input sequence was empty.">>);
exactly_one(_) -> invalid_arg(<<"source">>, <<"The input sequence contains more than one element.">>).

try_exactly_one([X]) -> fable_option:some(X);
try_exactly_one(_) -> undefined.

distinct(List) ->
    distinct(List, #{}).
distinct([], _Seen) ->
    [];
distinct([H | T], Seen) ->
    case maps:is_key(H, Seen) of
        true -> distinct(T, Seen);
        false -> [H | distinct(T, Seen#{H => true})]
    end.

distinct_by(Fn, List) ->
    distinct_by(Fn, List, #{}).
distinct_by(_Fn, [], _Seen) ->
    [];
distinct_by(Fn, [H | T], Seen) ->
    Key = Fn(H),
    case maps:is_key(Key, Seen) of
        true -> distinct_by(Fn, T, Seen);
        false -> [H | distinct_by(Fn, T, Seen#{Key => true})]
    end.

pairwise([]) -> [];
pairwise([_]) -> [];
pairwise([A, B | T]) -> [{A, B} | pairwise([B | T])].

exists2(Fn, [A | T1], [B | T2]) ->
    case (Fn(A))(B) of
        true -> true;
        false -> exists2(Fn, T1, T2)
    end;
exists2(_Fn, [], []) ->
    false;
exists2(_Fn, _L1, _L2) ->
    lists_had_different_lengths(<<"list2">>).

forall2(Fn, [A | T1], [B | T2]) ->
    case (Fn(A))(B) of
        true -> forall2(Fn, T1, T2);
        false -> false
    end;
forall2(_Fn, [], []) ->
    true;
forall2(_Fn, [], _L2) ->
    lists_had_different_lengths(<<"list1">>);
forall2(_Fn, _L1, []) ->
    lists_had_different_lengths(<<"list2">>).

array_exists2(Fn, A1, A2) when length(A1) =:= length(A2) ->
    exists2(Fn, A1, A2);
array_exists2(_Fn, _A1, _A2) ->
    invalid_arg(<<"array1">>, <<"The arrays have different lengths.">>).

array_forall2(Fn, A1, A2) when length(A1) =:= length(A2) ->
    forall2(Fn, A1, A2);
array_forall2(_Fn, _A1, _A2) ->
    invalid_arg(<<"array1">>, <<"The arrays have different lengths.">>).

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
    lists:foldl(
        fun(E, I) ->
            (Fn(I))(E),
            I + 1
        end,
        0,
        List
    ),
    ok.

iteri2(Fn, L1, L2) ->
    Zipped = lists:zip(L1, L2),
    lists:foldl(
        fun({A, B}, I) ->
            ((Fn(I))(A))(B),
            I + 1
        end,
        0,
        Zipped
    ),
    ok.

iter_indexed(Fn, List) -> iteri(Fn, List).

average(List) ->
    lists:sum(List) / length(List).

average_by(Fn, List) ->
    lists:sum(lists:map(Fn, List)) / length(List).

count_by(Fn, List) ->
    Grouped = lists:foldl(
        fun(E, Acc) ->
            Key = Fn(E),
            Count = maps:get(Key, Acc, 0),
            Acc#{Key => Count + 1}
        end,
        #{},
        List
    ),
    maps:to_list(Grouped).

group_by(Fn, List) ->
    Grouped = lists:foldl(
        fun(E, Acc) ->
            Key = Fn(E),
            Vals = maps:get(Key, Acc, []),
            Acc#{Key => Vals ++ [E]}
        end,
        #{},
        List
    ),
    maps:to_list(Grouped).

unfold(Fn, State) ->
    case Fn(State) of
        undefined -> [];
        {A, B} -> [A | unfold(Fn, B)]
    end.

split_at(Index, List) ->
    {lists:sublist(List, Index), lists:nthtail(Index, List)}.

chunk_by_size(Size, _List) when Size < 1 ->
    invalid_arg(<<"chunkSize">>, <<"The input must be positive.">>);
chunk_by_size(Size, List) ->
    chunk_by_size(Size, List, []).
chunk_by_size(_Size, [], Acc) ->
    lists:reverse(Acc);
chunk_by_size(Size, List, Acc) ->
    {Chunk, Rest} =
        case length(List) >= Size of
            true -> {lists:sublist(List, Size), lists:nthtail(Size, List)};
            false -> {List, []}
        end,
    chunk_by_size(Size, Rest, [Chunk | Acc]).

windowed(Size, _List) when Size < 1 ->
    invalid_arg(<<"windowSize">>, <<"The input must be positive.">>);
windowed(Size, List) ->
    windowed(Size, List, []).
windowed(Size, List, Acc) when length(List) < Size -> lists:reverse(Acc);
windowed(Size, [_ | T] = List, Acc) ->
    Window = lists:sublist(List, Size),
    windowed(Size, T, [Window | Acc]).

split_into(Count, _List) when Count < 1 ->
    invalid_arg(<<"count">>, <<"The input must be positive.">>);
split_into(_Count, []) ->
    [];
split_into(Count, List) ->
    Len = length(List),
    ActualCount = min(Count, Len),
    Base = Len div ActualCount,
    Extra = Len rem ActualCount,
    split_into(ActualCount, List, Base, Extra, []).
split_into(0, _, _, _, Acc) ->
    lists:reverse(Acc);
split_into(N, List, Base, Extra, Acc) ->
    Size =
        Base +
            (case Extra > 0 of
                true -> 1;
                false -> 0
            end),
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
    Result2 = lists:foldl(
        fun(I, Acc) ->
            TargetIdx = IndexFn(I),
            setelement(TargetIdx + 1, Acc, element(I + 1, Arr))
        end,
        Result,
        lists:seq(0, Len - 1)
    ),
    tuple_to_list(Result2).

map_fold(Fn, State, List) ->
    lists:mapfoldl(
        fun(Item, Acc) ->
            {Mapped, NewAcc} = (Fn(Acc))(Item),
            {Mapped, NewAcc}
        end,
        State,
        List
    ).

map_fold_back(Fn, List, State) ->
    lists:mapfoldr(
        fun(Item, Acc) ->
            {Mapped, NewAcc} = (Fn(Item))(Acc),
            {Mapped, NewAcc}
        end,
        State,
        List
    ).

pick(Fn, List) ->
    case try_pick(Fn, List) of
        undefined -> index_not_found();
        V -> V
    end.

try_pick(_Fn, []) ->
    undefined;
try_pick(Fn, [H | T]) ->
    case Fn(H) of
        undefined -> try_pick(Fn, T);
        V -> V
    end.

find_index_back(Fn, List) ->
    case try_find_index_back(Fn, List) of
        undefined -> index_not_found();
        Idx -> Idx
    end.

try_find_index_back(Fn, List) ->
    try_find_index_back(Fn, lists:reverse(List), length(List) - 1).
try_find_index_back(_Fn, [], _Idx) ->
    undefined;
try_find_index_back(Fn, [H | T], Idx) ->
    case Fn(H) of
        true -> Idx;
        false -> try_find_index_back(Fn, T, Idx - 1)
    end.

transpose([]) ->
    [];
transpose([First | Rest] = Lists) ->
    Len = length(First),
    case lists:all(fun(L) -> length(L) =:= Len end, Rest) of
        true -> transpose_rows(Lists);
        false -> invalid_arg(<<"array.[0]">>, <<"The arrays have different lengths.">>)
    end.

transpose_rows([[] | _]) ->
    [];
transpose_rows(Lists) ->
    Heads = [hd(L) || L <- Lists],
    Tails = [tl(L) || L <- Lists],
    [Heads | transpose_rows(Tails)].

compare_with(_Fn, [], []) ->
    0;
compare_with(_Fn, [], _) ->
    -1;
compare_with(_Fn, _, []) ->
    1;
compare_with(Fn, [H1 | T1], [H2 | T2]) ->
    case (Fn(H1))(H2) of
        0 -> compare_with(Fn, T1, T2);
        N -> N
    end.

update_at(Idx, Value, List) ->
    {Before, [_ | After]} = lists:split(Idx, List),
    Before ++ [Value | After].

insert_at(Idx, Value, List) ->
    {Before, After} = lists:split(Idx, List),
    Before ++ [Value | After].

insert_many_at(Idx, Values, List) ->
    {Before, After} = lists:split(Idx, List),
    Before ++ Values ++ After.

remove_at(Idx, List) ->
    {Before, [_ | After]} = lists:split(Idx, List),
    Before ++ After.

remove_many_at(Idx, Count, List) ->
    {Before, Rest} = lists:split(Idx, List),
    After = lists:nthtail(Count, Rest),
    Before ++ After.

index_of_value(Value, List) ->
    index_of_value(Value, List, 0).
index_of_value(_Value, [], _Idx) -> -1;
index_of_value(Value, [Value | _], Idx) -> Idx;
index_of_value(Value, [_ | T], Idx) -> index_of_value(Value, T, Idx + 1).

get_slice(Lower, Upper, List) ->
    Start =
        case Lower of
            undefined -> 0;
            _ -> Lower
        end,
    End =
        case Upper of
            undefined -> erlang:length(List) - 1;
            _ -> Upper
        end,
    lists:sublist(List, Start + 1, End - Start + 1).

-spec sub(list(), integer(), integer()) -> list().
sub(_List, StartIndex, _Count) when StartIndex < 0 ->
    invalid_arg(<<"startIndex">>, <<"The input must be non-negative.">>);
sub(_List, _StartIndex, Count) when Count < 0 ->
    invalid_arg(<<"count">>, <<"The input must be non-negative.">>);
sub(List, 0, Count) ->
    case lists:sublist(List, Count) of
        Sub when length(Sub) =:= Count -> Sub;
        _ -> argument_out_of_range(<<"count">>, <<"The index is outside the legal range.">>)
    end;
sub([_ | T], StartIndex, Count) ->
    sub(T, StartIndex - 1, Count);
sub([], _StartIndex, _Count) ->
    argument_out_of_range(<<"count">>, <<"The index is outside the legal range.">>).

%% HashSet.CopyTo calls fable_resize_array:blit/5 directly, its count may exceed the set
-spec blit(list(), integer(), list(), integer(), integer()) -> list().
blit(_Source, _SourceIdx, _Target, _TargetIdx, Count) when Count < 0 ->
    argument_out_of_range(<<"length">>, <<"Non-negative number required.">>);
blit(_Source, SourceIdx, _Target, _TargetIdx, _Count) when SourceIdx < 0 ->
    argument_out_of_range(<<"sourceIndex">>, <<"Non-negative number required.">>);
blit(Source, SourceIdx, _Target, _TargetIdx, Count) when SourceIdx + Count > length(Source) ->
    invalid_arg(
        <<"sourceArray">>,
        <<"Source array was not long enough. Check the source index, length, and the array's lower bounds.">>
    );
blit(_Source, _SourceIdx, _Target, TargetIdx, _Count) when TargetIdx < 0 ->
    argument_out_of_range(<<"destinationIndex">>, <<"Non-negative number required.">>);
blit(_Source, _SourceIdx, Target, TargetIdx, Count) when TargetIdx + Count > length(Target) ->
    invalid_arg(
        <<"destinationArray">>,
        <<"Destination array was not long enough. Check the destination index, length, and the array's lower bounds.">>
    );
blit(Source, SourceIdx, Target, TargetIdx, Count) ->
    fable_resize_array:blit(Source, SourceIdx, Target, TargetIdx, Count).

-spec take(non_neg_integer(), list()) -> list().
take(Count, List) ->
    case erlang:length(List) >= Count of
        true -> lists:sublist(List, Count);
        false ->
            erlang:error(#{
                exn_type => invalid_operation_exception,
                message => <<"The input sequence has an insufficient number of elements.">>
            })
    end.

%% ---- Random functions --------------------------------------------------------
%% These implement Array/List random* operations directly on plain Erlang lists,
%% avoiding any delegation through seq.erl (which would cause circular recursion).
%%
%% Calling convention: Randomizer is fun(ok) -> float()  (F# unit -> float curried).
%% Random (System.Random) is a reference() holding per-instance PRNG state.

-spec random_shuffle_by(fun((ok) -> float()), list()) -> list().
random_shuffle_by(Randomizer, Xs) ->
    shuffle_loop(Randomizer, erlang:list_to_tuple(Xs), erlang:length(Xs), 0).

-spec random_shuffle_with(reference(), list()) -> list().
random_shuffle_with(Random, Xs) ->
    random_shuffle_by(fun(_) -> fable_random:next_double(Random) end, Xs).

-spec random_shuffle(list()) -> list().
random_shuffle(Xs) ->
    random_shuffle_with(fable_random:new(), Xs).

-spec random_choice_by(fun((ok) -> float()), list()) -> term().
random_choice_by(_Randomizer, []) ->
    invalid_arg(<<"source">>, <<"The input sequence was empty.">>);
random_choice_by(Randomizer, Xs) ->
    lists:nth(random_next(Randomizer, 0, erlang:length(Xs)) + 1, Xs).

-spec random_choice_with(reference(), list()) -> term().
random_choice_with(Random, Xs) ->
    random_choice_by(fun(_) -> fable_random:next_double(Random) end, Xs).

-spec random_choice(list()) -> term().
random_choice(Xs) ->
    random_choice_with(fable_random:new(), Xs).

-spec random_choices_by(fun((ok) -> float()), non_neg_integer(), list()) -> list().
random_choices_by(Randomizer, Count, Xs) ->
    if
        Count < 0 ->
            negative_count(Count);
        Count > 0 andalso Xs =:= [] ->
            invalid_arg(<<"source">>, <<"The input sequence was empty.">>);
        true ->
            Arr = erlang:list_to_tuple(Xs),
            Len = erlang:tuple_size(Arr),
            choices_loop(Randomizer, Arr, Len, Count, [])
    end.

%% Tail-recursive helper for random_choices_by: builds result in reverse then flips.
choices_loop(_Randomizer, _Arr, _Len, 0, Acc) ->
    lists:reverse(Acc);
choices_loop(Randomizer, Arr, Len, N, Acc) ->
    E = erlang:element(random_next(Randomizer, 0, Len) + 1, Arr),
    choices_loop(Randomizer, Arr, Len, N - 1, [E | Acc]).

-spec random_choices_with(reference(), non_neg_integer(), list()) -> list().
random_choices_with(Random, Count, Xs) ->
    random_choices_by(fun(_) -> fable_random:next_double(Random) end, Count, Xs).

-spec random_choices(non_neg_integer(), list()) -> list().
random_choices(Count, Xs) ->
    random_choices_with(fable_random:new(), Count, Xs).

-spec random_sample_by(fun((ok) -> float()), non_neg_integer(), list()) -> list().
random_sample_by(Randomizer, Count, Xs) ->
    if
        Count < 0 ->
            negative_count(Count);
        true ->
            Len = erlang:length(Xs),
            if
                Len =:= 0 andalso Count > 0 ->
                    invalid_arg(<<"source">>, <<"The input sequence was empty.">>);
                Count > Len ->
                    invalid_arg(<<"count">>, <<"The input sequence has an insufficient number of elements.">>);
                true ->
                    Arr = erlang:list_to_tuple(Xs),
                    case Len =< max_set_size(Count) of
                        true -> sample_pool(Randomizer, Arr, Len, Count, 0, []);
                        false -> sample_set(Randomizer, Arr, Len, Count, #{}, [])
                    end
            end
    end.

-spec random_sample_with(reference(), non_neg_integer(), list()) -> list().
random_sample_with(Random, Count, Xs) ->
    random_sample_by(fun(_) -> fable_random:next_double(Random) end, Count, Xs).

-spec random_sample(non_neg_integer(), list()) -> list().
random_sample(Count, Xs) ->
    random_sample_with(fable_random:new(), Count, Xs).

%% FSharp.Core's sampling: a shrinking pool for small inputs, otherwise redraw seen indices.
max_set_size(Count) when Count > 5 ->
    21 + erlang:trunc(math:pow(4, math:ceil(math:log(Count * 3) / math:log(4))));
max_set_size(_Count) ->
    21.

sample_pool(_Randomizer, _Pool, _Len, Count, I, Acc) when I >= Count ->
    lists:reverse(Acc);
sample_pool(Randomizer, Pool, Len, Count, I, Acc) ->
    J = random_next(Randomizer, 0, Len - I),
    Item = erlang:element(J + 1, Pool),
    Pool2 = erlang:setelement(J + 1, Pool, erlang:element(Len - I, Pool)),
    sample_pool(Randomizer, Pool2, Len, Count, I + 1, [Item | Acc]).

sample_set(_Randomizer, _Arr, _Len, 0, _Selected, Acc) ->
    lists:reverse(Acc);
sample_set(Randomizer, Arr, Len, N, Selected, Acc) ->
    J = random_next(Randomizer, 0, Len),
    case maps:is_key(J, Selected) of
        true ->
            sample_set(Randomizer, Arr, Len, N, Selected, Acc);
        false ->
            Item = erlang:element(J + 1, Arr),
            sample_set(Randomizer, Arr, Len, N - 1, Selected#{J => true}, [Item | Acc])
    end.

%% FSharp.Core's forward Fisher-Yates shuffle.
shuffle_loop(_Randomizer, Arr, Len, I) when I >= Len - 1 ->
    erlang:tuple_to_list(Arr);
shuffle_loop(Randomizer, Arr, Len, I) ->
    J = random_next(Randomizer, I, Len),
    EI = erlang:element(I + 1, Arr),
    EJ = erlang:element(J + 1, Arr),
    Arr2 = erlang:setelement(I + 1, erlang:setelement(J + 1, Arr, EI), EJ),
    shuffle_loop(Randomizer, Arr2, Len, I + 1).

%% Like FSharp.Core's Random.next: an index in [Min, Max) from a randomizer result in [0.0, 1.0).
random_next(Randomizer, Min, Max) ->
    R = Randomizer(ok),
    if
        R < 0.0; R >= 1.0 ->
            randomizer_out_of_range(R);
        true ->
            erlang:trunc(R * (Max - Min)) + Min
    end.

negative_count(Count) ->
    invalid_arg(<<"count">>, <<"The input must be non-negative.\ncount = ", (erlang:integer_to_binary(Count))/binary>>).

randomizer_out_of_range(R) ->
    erlang:error(#{
        exn_type => argument_out_of_range_exception,
        message => iolist_to_binary(
            io_lib:format(
                "The index is outside the legal range.~nrandomizer returned ~w, should be in range [0.0, 1.0). (Parameter 'randomizer')",
                [R]
            )
        ),
        param_name => <<"randomizer">>
    }).

invalid_arg(ParamName, Message) ->
    erlang:error(#{
        exn_type => argument_exception,
        message => fable_utils:argument_exception_message(Message, ParamName),
        param_name => ParamName
    }).

argument_out_of_range(ParamName, Message) ->
    erlang:error(#{
        exn_type => argument_out_of_range_exception,
        message => fable_utils:argument_exception_message(Message, ParamName),
        param_name => ParamName
    }).

lists_had_different_lengths(ParamName) ->
    invalid_arg(ParamName, <<"The lists had different lengths.">>).
