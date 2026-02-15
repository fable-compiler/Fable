-module(fable_async).
-export([start_immediate/1, start_immediate/2,
         run_synchronously/1, run_synchronously/2,
         start_with_continuations/4, start_with_continuations/5,
         sleep/1, parallel/1, sequential/1,
         catch_async/1, ignore/1, from_continuations/1,
         start_as_task/1,
         cancellation_token/0, create_cancellation_token/0, create_cancellation_token/1]).

%% Default context: run inline in current process
default_ctx(CancelToken) ->
    #{on_success => fun(_) -> ok end,
      on_error => fun(E) -> erlang:error(E) end,
      on_cancel => fun(_) -> ok end,
      cancel_token => CancelToken}.

%% StartImmediate: run with default context (fire-and-forget)
start_immediate(Computation) -> start_immediate(Computation, undefined).
start_immediate(Computation, CancelToken) ->
    Computation(default_ctx(CancelToken)).

%% RunSynchronously: run CPS chain in current process, capture result
run_synchronously(Computation) -> run_synchronously(Computation, undefined).
run_synchronously(Computation, CancelToken) ->
    %% Use a unique ref as key to store result in process dict
    Ref = make_ref(),
    Ctx = #{on_success => fun(V) -> put(Ref, {ok, V}) end,
            on_error => fun(E) -> put(Ref, {error, E}) end,
            on_cancel => fun(_) -> put(Ref, {cancelled}) end,
            cancel_token => CancelToken},
    try Computation(Ctx)
    catch _:Err -> put(Ref, {error, Err})
    end,
    Result = erase(Ref),
    case Result of
        {ok, Value} -> Value;
        {error, Error} -> erlang:error(Error);
        {cancelled} -> erlang:error(operation_cancelled);
        undefined -> erlang:error(async_no_result)
    end.

%% StartWithContinuations
start_with_continuations(Comp, OnSuccess, OnError, OnCancel) ->
    start_with_continuations(Comp, OnSuccess, OnError, OnCancel, undefined).
start_with_continuations(Comp, OnSuccess, OnError, OnCancel, Token) ->
    Ctx = #{on_success => OnSuccess, on_error => OnError,
            on_cancel => OnCancel, cancel_token => Token},
    try Comp(Ctx)
    catch _:Err -> OnError(Err)
    end.

%% Sleep: pause current process, with cancellation support
sleep(Milliseconds) ->
    fun(Ctx) ->
        Token = maps:get(cancel_token, Ctx),
        case Token of
            undefined ->
                timer:sleep(Milliseconds),
                (maps:get(on_success, Ctx))(ok);
            _ ->
                %% Check if already cancelled before sleeping
                case fable_cancellation:is_cancellation_requested(Token) of
                    true ->
                        (maps:get(on_cancel, Ctx))(ok);
                    false ->
                        %% Set up a timer to send us a wake-up message
                        Self = self(),
                        TimerRef = make_ref(),
                        timer:apply_after(Milliseconds, erlang, send, [Self, {sleep_done, TimerRef}]),
                        %% Register a cancellation listener to wake us up early
                        RegId = fable_cancellation:register(Token, fun(_) ->
                            Self ! {sleep_cancelled, TimerRef}
                        end),
                        %% Wait for either sleep completion or cancellation
                        receive
                            {sleep_done, TimerRef} ->
                                (maps:get(on_success, Ctx))(ok);
                            {sleep_cancelled, TimerRef} ->
                                (maps:get(on_cancel, Ctx))(ok);
                            {cancel_token, Token} ->
                                %% Timer-based cancel via cancel_after
                                fable_cancellation:cancel(Token),
                                (maps:get(on_cancel, Ctx))(ok)
                        end,
                        %% Clean up registration if we have one
                        case RegId of
                            undefined -> ok;
                            _ -> ok
                        end
                end
        end
    end.

%% Parallel: spawn one process per computation, collect results in order
parallel(Computations) ->
    fun(Ctx) ->
        Self = self(),
        Indexed = lists:zip(lists:seq(1, length(Computations)), Computations),
        lists:foreach(fun({Idx, Comp}) ->
            spawn(fun() ->
                try
                    Result = fable_async:run_synchronously(Comp),
                    Self ! {async_parallel, Idx, {ok, Result}}
                catch _:Err ->
                    Self ! {async_parallel, Idx, {error, Err}}
                end
            end)
        end, Indexed),
        Results = collect_parallel(length(Computations), #{}),
        case Results of
            {ok, Map} ->
                Ordered = [maps:get(I, Map) || I <- lists:seq(1, length(Computations))],
                (maps:get(on_success, Ctx))(Ordered);
            {error, Err} ->
                (maps:get(on_error, Ctx))(Err)
        end
    end.

collect_parallel(0, Acc) -> {ok, Acc};
collect_parallel(N, Acc) ->
    receive
        {async_parallel, Idx, {ok, Val}} ->
            collect_parallel(N - 1, maps:put(Idx, Val, Acc));
        {async_parallel, _Idx, {error, Err}} ->
            {error, Err}
    end.

%% Sequential: run computations one by one, collect results
sequential(Computations) ->
    fun(Ctx) ->
        Results = lists:map(fun(Comp) -> run_synchronously(Comp) end, Computations),
        (maps:get(on_success, Ctx))(Results)
    end.

%% Catch: wrap result in Choice (ok/error tuple)
catch_async(Computation) ->
    fun(Ctx) ->
        Ctx2 = #{
            on_success => fun(V) -> (maps:get(on_success, Ctx))({choice1of2, V}) end,
            on_error => fun(E) -> (maps:get(on_success, Ctx))({choice2of2, E}) end,
            on_cancel => maps:get(on_cancel, Ctx),
            cancel_token => maps:get(cancel_token, Ctx)
        },
        try Computation(Ctx2)
        catch _:Err -> (maps:get(on_success, Ctx))({choice2of2, Err})
        end
    end.

%% Ignore: discard result
ignore(Computation) ->
    fable_async_builder:bind(Computation, fun(_) -> fable_async_builder:return(ok) end).

%% FromContinuations: lower-level primitive
from_continuations(F) ->
    fun(Ctx) ->
        (F({maps:get(on_success, Ctx), maps:get(on_error, Ctx), maps:get(on_cancel, Ctx)}))
    end.

%% StartAsTask: simplified — just run synchronously for now
start_as_task(Computation) ->
    run_synchronously(Computation).

%% CancellationToken: returns async that extracts cancel_token from context
cancellation_token() ->
    fun(Ctx) ->
        Token = maps:get(cancel_token, Ctx),
        (maps:get(on_success, Ctx))(Token)
    end.

%% Create cancellation token (delegates to fable_cancellation)
create_cancellation_token() -> fable_cancellation:create().
create_cancellation_token(Arg) -> fable_cancellation:create(Arg).
