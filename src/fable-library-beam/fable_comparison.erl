-module(fable_comparison).
-export([compare/2, equals/2, hash/1]).

-spec equals(term(), term()) -> boolean().
-spec compare(term(), term()) -> -1 | 0 | 1.
-spec hash(term()) -> non_neg_integer().

%% Deep equality that handles ref-wrapped arrays (process dict refs)
%% and atomics-backed byte arrays ({byte_array, Size, AtomicsRef}).
equals(A, A) -> true;
equals({byte_array, _, _} = A, {byte_array, _, _} = B) ->
    fable_utils:byte_array_to_list(A) =:= fable_utils:byte_array_to_list(B);
equals(A, B) when is_reference(A), is_reference(B) ->
    case {get(A), get(B)} of
        {undefined, undefined} -> A =:= B;
        {VA, VB} -> deep_equals(VA, VB)
    end;
equals(A, B) when is_reference(A) ->
    deep_equals(get(A), B);
equals(A, B) when is_reference(B) ->
    deep_equals(A, get(B));
equals(A, B) when is_list(A), is_list(B) ->
    deep_equals_list(A, B);
equals(A, B) when is_tuple(A), is_tuple(B) ->
    deep_equals_tuple(A, B, 1, erlang:tuple_size(A));
equals(A, B) when is_map(A), is_map(B) ->
    deep_equals_map(A, B);
equals(_, _) -> false.

deep_equals(A, A) -> true;
deep_equals({byte_array, _, _} = A, {byte_array, _, _} = B) ->
    fable_utils:byte_array_to_list(A) =:= fable_utils:byte_array_to_list(B);
deep_equals(A, B) when is_reference(A), is_reference(B) ->
    case {get(A), get(B)} of
        {undefined, undefined} -> A =:= B;
        {VA, VB} -> deep_equals(VA, VB)
    end;
deep_equals(A, B) when is_reference(A) ->
    deep_equals(get(A), B);
deep_equals(A, B) when is_reference(B) ->
    deep_equals(A, get(B));
deep_equals(A, B) when is_list(A), is_list(B) ->
    deep_equals_list(A, B);
deep_equals(A, B) when is_tuple(A), is_tuple(B) ->
    deep_equals_tuple(A, B, 1, erlang:tuple_size(A));
deep_equals(A, B) when is_map(A), is_map(B) ->
    deep_equals_map(A, B);
deep_equals(_, _) -> false.

deep_equals_list([], []) -> true;
deep_equals_list([H1|T1], [H2|T2]) ->
    deep_equals(H1, H2) andalso deep_equals_list(T1, T2);
deep_equals_list(_, _) -> false.

deep_equals_tuple(_A, _B, I, Size) when I > Size -> true;
deep_equals_tuple(A, B, I, Size) ->
    deep_equals(erlang:element(I, A), erlang:element(I, B))
    andalso deep_equals_tuple(A, B, I + 1, Size).

deep_equals_map(A, B) ->
    maps:size(A) =:= maps:size(B) andalso
    lists:all(fun(K) ->
        maps:is_key(K, B) andalso deep_equals(maps:get(K, A), maps:get(K, B))
    end, maps:keys(A)).

%% Deep compare that handles ref-wrapped arrays and byte arrays.
compare({byte_array, _, _} = A, {byte_array, _, _} = B) ->
    compare_list(fable_utils:byte_array_to_list(A), fable_utils:byte_array_to_list(B));
compare(A, B) when is_reference(A), is_reference(B) ->
    case {get(A), get(B)} of
        {undefined, undefined} ->
            if A < B -> -1; A > B -> 1; true -> 0 end;
        {VA, VB} -> compare(VA, VB)
    end;
compare(A, B) when is_reference(A) ->
    compare(get(A), B);
compare(A, B) when is_reference(B) ->
    compare(A, get(B));
compare(A, B) when is_list(A), is_list(B) ->
    compare_list(A, B);
compare(A, B) when is_tuple(A), is_tuple(B) ->
    compare_tuple(A, B, 1, erlang:tuple_size(A));
compare(A, B) when A < B -> -1;
compare(A, B) when A > B -> 1;
compare(_, _) -> 0.

compare_list([], []) -> 0;
compare_list([], _) -> -1;
compare_list(_, []) -> 1;
compare_list([H1|T1], [H2|T2]) ->
    case compare(H1, H2) of
        0 -> compare_list(T1, T2);
        R -> R
    end.

compare_tuple(_A, _B, I, Size) when I > Size -> 0;
compare_tuple(A, B, I, Size) ->
    case compare(erlang:element(I, A), erlang:element(I, B)) of
        0 -> compare_tuple(A, B, I + 1, Size);
        R -> R
    end.

%% Hash that derefs refs and byte arrays before hashing.
hash({byte_array, _, _} = V) -> erlang:phash2(fable_utils:byte_array_to_list(V));
hash(V) when is_reference(V) ->
    case get(V) of
        undefined -> erlang:phash2(V);
        Val -> erlang:phash2(deref_for_hash(Val))
    end;
hash(V) -> erlang:phash2(deref_for_hash(V)).

deref_for_hash({byte_array, _, _} = V) -> fable_utils:byte_array_to_list(V);
deref_for_hash(V) when is_reference(V) ->
    case get(V) of
        undefined -> V;
        Val -> deref_for_hash(Val)
    end;
deref_for_hash(L) when is_list(L) -> lists:map(fun deref_for_hash/1, L);
deref_for_hash(T) when is_tuple(T) ->
    list_to_tuple(lists:map(fun deref_for_hash/1, tuple_to_list(T)));
deref_for_hash(M) when is_map(M) ->
    maps:map(fun(_K, V) -> deref_for_hash(V) end, M);
deref_for_hash(V) -> V.
