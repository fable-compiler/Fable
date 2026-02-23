-module(fable_parallel).
-export([
    parallel_map/2,
    parallel_mapi/2,
    parallel_init/2,
    parallel_iter/2,
    parallel_iteri/2,
    parallel_collect/2,
    parallel_choose/2,
    parallel_for/3
]).

-spec parallel_map(fun(), list()) -> list().
-spec parallel_mapi(fun(), list()) -> list().
-spec parallel_init(non_neg_integer(), fun()) -> list().
-spec parallel_iter(fun(), list()) -> ok.
-spec parallel_iteri(fun(), list()) -> ok.
-spec parallel_collect(fun(), list()) -> list().
-spec parallel_choose(fun(), list()) -> list().
-spec parallel_for(integer(), integer(), fun()) -> ok.

%% Map: apply Fn to each element in parallel, return results in order.
%% Input List is already deref'd by Replacements.
parallel_map(Fn, List) ->
    Self = self(),
    Len = erlang:length(List),
    Indexed = lists:zip(lists:seq(0, Len - 1), List),
    Pids = lists:map(
        fun({Idx, Elem}) ->
            spawn_monitor(fun() -> Self ! {parallel_result, Idx, Fn(Elem)} end)
        end,
        Indexed
    ),
    collect_ordered(Len, Pids).

%% MapIndexed: apply Fn(index)(elem) in parallel (curried).
parallel_mapi(Fn, List) ->
    Self = self(),
    Len = erlang:length(List),
    Indexed = lists:zip(lists:seq(0, Len - 1), List),
    Pids = lists:map(
        fun({Idx, Elem}) ->
            spawn_monitor(fun() -> Self ! {parallel_result, Idx, (Fn(Idx))(Elem)} end)
        end,
        Indexed
    ),
    collect_ordered(Len, Pids).

%% Initialize: create array of Count elements using Fn(index) in parallel.
parallel_init(Count, Fn) ->
    Self = self(),
    Pids = lists:map(
        fun(Idx) ->
            spawn_monitor(fun() -> Self ! {parallel_result, Idx, Fn(Idx)} end)
        end,
        lists:seq(0, Count - 1)
    ),
    collect_ordered(Count, Pids).

%% Iterate: apply Fn to each element in parallel (for side effects).
parallel_iter(Fn, List) ->
    Self = self(),
    Len = erlang:length(List),
    Pids = lists:map(
        fun(Elem) ->
            spawn_monitor(fun() ->
                Fn(Elem),
                Self ! {parallel_done}
            end)
        end,
        List
    ),
    collect_done(Len, Pids).

%% IterateIndexed: apply Fn(index)(elem) in parallel (curried, for side effects).
parallel_iteri(Fn, List) ->
    Self = self(),
    Len = erlang:length(List),
    Indexed = lists:zip(lists:seq(0, Len - 1), List),
    Pids = lists:map(
        fun({Idx, Elem}) ->
            spawn_monitor(fun() ->
                (Fn(Idx))(Elem),
                Self ! {parallel_done}
            end)
        end,
        Indexed
    ),
    collect_done(Len, Pids).

%% Collect: like map but Fn returns arrays, flatten results.
%% Each child's Fn returns a ref-wrapped array (process-dict), so we must
%% deref in the child before sending the result back.
parallel_collect(Fn, List) ->
    Self = self(),
    Len = erlang:length(List),
    Indexed = lists:zip(lists:seq(0, Len - 1), List),
    Pids = lists:map(
        fun({Idx, Elem}) ->
            spawn_monitor(fun() ->
                Result = Fn(Elem),
                %% Deref array ref created in this child process
                Derefed = deref_if_ref(Result),
                Self ! {parallel_result, Idx, Derefed}
            end)
        end,
        Indexed
    ),
    lists:append(collect_ordered(Len, Pids)).

%% Choose: apply Fn in parallel, keep Some results.
%% Options use smart constructor: Some(x) -> x or {some, x}, None -> undefined.
parallel_choose(Fn, List) ->
    Mapped = parallel_map(Fn, List),
    [fable_option:value(V) || V <- Mapped, V =/= undefined].

%% Parallel.For(fromInclusive, toExclusive, body): run body(i) for each i in parallel.
parallel_for(From, To, Body) ->
    Self = self(),
    Count = To - From,
    Pids = lists:map(
        fun(I) ->
            spawn_monitor(fun() ->
                Body(I),
                Self ! {parallel_done}
            end)
        end,
        lists:seq(From, To - 1)
    ),
    collect_done(Count, Pids).

%% Internal: collect N ordered results into a list.
%% Pids is a list of {Pid, MonitorRef} from spawn_monitor.
collect_ordered(0, _Pids) ->
    [];
collect_ordered(N, Pids) ->
    collect_ordered(N, #{}, Pids).

collect_ordered(N, Acc, Pids) ->
    receive
        {parallel_result, Idx, Val} ->
            Acc2 = maps:put(Idx, Val, Acc),
            case maps:size(Acc2) of
                N -> [maps:get(I, Acc2) || I <- lists:seq(0, N - 1)];
                _ -> collect_ordered(N, Acc2, Pids)
            end;
        {'DOWN', _Ref, process, _Pid, normal} ->
            %% Normal exit, ignore
            collect_ordered(N, Acc, Pids);
        {'DOWN', _Ref, process, _Pid, Reason} ->
            erlang:error({parallel_child_crashed, Reason})
    end.

%% Internal: wait for N done signals.
collect_done(0, _Pids) ->
    ok;
collect_done(N, Pids) ->
    receive
        {parallel_done} -> collect_done(N - 1, Pids);
        {'DOWN', _Ref, process, _Pid, normal} -> collect_done(N, Pids);
        {'DOWN', _Ref, process, _Pid, Reason} -> erlang:error({parallel_child_crashed, Reason})
    end.

%% Internal: deref a process-dict ref if it is a reference, otherwise pass through.
deref_if_ref(Ref) when is_reference(Ref) -> erlang:get(Ref);
deref_if_ref(Other) -> Other.
