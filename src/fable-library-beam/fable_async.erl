-module(fable_async).
-export([
    start_immediate/1, start_immediate/2,
    start_child/1, start_child/2,
    run_synchronously/1, run_synchronously/2,
    start_with_continuations/4, start_with_continuations/5,
    sleep/1,
    await_event/1, await_event/2,
    parallel/1,
    sequential/1,
    catch_async/1,
    ignore/1,
    from_continuations/1,
    start_as_task/1,
    cancellation_token/0,
    create_cancellation_token/0, create_cancellation_token/1,
    wrap_error/1,
    record_stacktrace/2,
    clear_stacktrace/0
]).

-type async_ctx() :: #{
    on_success := fun(),
    on_error := fun(),
    on_cancel := fun(),
    cancel_token := reference() | undefined
}.
-type async(T) :: fun((async_ctx()) -> T).

-spec start_immediate(async(term())) -> term().
-spec start_immediate(async(term()), reference() | undefined) -> term().
-spec start_child(async(term())) -> async(async(term())).
-spec start_child(async(term()), non_neg_integer() | undefined) -> async(async(term())).
-spec run_synchronously(async(term())) -> term().
-spec run_synchronously(async(term()), reference() | undefined) -> term().
-spec start_with_continuations(async(term()), fun(), fun(), fun()) -> term().
-spec start_with_continuations(async(term()), fun(), fun(), fun(), reference() | undefined) ->
    term().
-spec sleep(non_neg_integer()) -> async(ok).
-spec parallel(list() | reference()) -> async(reference()).
-spec sequential(list()) -> async(list()).
-spec catch_async(async(term())) -> async(term()).
-spec ignore(async(term())) -> async(ok).
-spec from_continuations(fun()) -> async(term()).
-spec start_as_task(async(term())) -> term().
-spec cancellation_token() -> async(reference() | undefined).
-spec create_cancellation_token() -> reference().
-spec create_cancellation_token(term()) -> reference().
-spec record_stacktrace(term(), erlang:stacktrace()) -> ok.
-spec clear_stacktrace() -> ok.

%% An error inside an async computation does not propagate as an exception: it is caught where it
%% happens and handed to the context's `on_error` as a bare term, so by the time
%% `run_synchronously` re-raises it the original stacktrace is long gone and the failure is
%% reported at the re-raise instead of where it happened. Every catch site in the async runtime
%% (here and in `fable_async_builder`) that hands an error onward therefore parks the stacktrace
%% it caught, so the re-raise can restore it.
%%
%% A parked entry is tagged with the enclosing `run_synchronously` call's token and only that call
%% can consume it. Untagged, a stacktrace parked for an error that was later *handled* (a
%% `try_with` whose handler succeeded) would linger in the process dictionary and could be
%% restored onto an unrelated later error that happened to be an equal term — attaching a
%% plausible but wrong stacktrace, which is the very failure this mechanism exists to remove.
-define(STACKTRACE_KEY, '$fable_async_stacktrace').
-define(RUN_KEY, '$fable_async_run').

%% @private Park the stacktrace of an error being handed to an `on_error` continuation.
record_stacktrace(Err, Stacktrace) ->
    case get(?RUN_KEY) of
        %% Outside any run_synchronously nothing can restore it, and parking it would only leave
        %% a stale entry behind in a long-lived process.
        undefined ->
            ok;
        Run ->
            put(?STACKTRACE_KEY, {Run, Err, Stacktrace}),
            ok
    end.

%% @private Drop the parked stacktrace, if any. Called where an error is handled rather than
%% propagated, so it cannot outlive the failure it belongs to.
clear_stacktrace() ->
    erase(?STACKTRACE_KEY),
    ok.

%% Re-raise with the stacktrace captured where the error was raised, when the parked one belongs
%% to this error and to this run — `erlang:error/1` would build a fresh one pointing at this line.
%% Errors that crossed a process boundary, or that were rewritten on the way up (`wrap_error`), do
%% not match and fall back to a fresh stacktrace.
reraise(Run, {Run, Err, Stacktrace}, Err) -> erlang:raise(error, Err, Stacktrace);
reraise(_Run, _Parked, Err) -> erlang:error(Err).

%% Default context: run inline in current process
default_ctx(CancelToken) ->
    #{
        on_success => fun(_) -> ok end,
        on_error => fun(E) -> erlang:error(E) end,
        on_cancel => fun(_) -> ok end,
        cancel_token => CancelToken
    }.

%% StartImmediate: run with default context (fire-and-forget)
start_immediate(Computation) -> start_immediate(Computation, undefined).
start_immediate(Computation, CancelToken) ->
    Computation(default_ctx(CancelToken)).

%% StartChild: run the computation concurrently and return an inner async that
%% waits for (and yields) its result. Mirrors .NET Async.StartChild: the child
%% starts immediately, and awaiting the returned inner async blocks until it
%% finishes. The optional timeout raises a timeout error if the child does not
%% finish in time.
%%
%% The child runs the computation to completion in its own process, then becomes
%% a small "result holder" that replies to on-demand requests. Because each
%% awaiter sends its own pid and the holder replies to that pid, the inner async
%% can be awaited from any process (not only the one that called start_child)
%% and any number of times — both matching .NET semantics. A unique ResultRef
%% tags every reply so several children never cross-talk. The holder lives until
%% it is killed (e.g. on timeout) or the VM stops, so a child whose result is
%% never awaited leaves an idle process behind.
%%
%% Cancellation tokens are intentionally not propagated to the child: tokens are
%% held in the process dictionary (see fable_cancellation), so they are only
%% meaningful within a single process and cannot be shared with the child's
%% separate process. This matches parallel/1.
start_child(Computation) -> start_child(Computation, undefined).
start_child(Computation, Timeout) ->
    fun(Ctx) ->
        ResultRef = make_ref(),
        Pid = spawn(fun() ->
            Result =
                try
                    {ok, fable_async:run_synchronously(Computation)}
                catch
                    _:Err -> {error, Err}
                end,
            child_result_holder(ResultRef, Result)
        end),
        TimeoutMs =
            case Timeout of
                undefined -> infinity;
                _ -> Timeout
            end,
        Inner = fun(InnerCtx) ->
            OnSuccess = maps:get(on_success, InnerCtx),
            OnError = maps:get(on_error, InnerCtx),
            Pid ! {get_child_result, ResultRef, self()},
            receive
                {async_child, ResultRef, {ok, Value}} -> OnSuccess(Value);
                {async_child, ResultRef, {error, Err}} -> OnError(wrap_error(Err))
            after TimeoutMs ->
                exit(Pid, kill),
                %% Drop a reply that may have raced in just before the kill so
                %% it does not linger in our mailbox.
                receive
                    {async_child, ResultRef, _} -> ok
                after 0 -> ok
                end,
                OnError(#{message => <<"The operation has timed out."/utf8>>})
            end
        end,
        (maps:get(on_success, Ctx))(Inner)
    end.

%% Serve a computed child result to any awaiter, any number of times.
child_result_holder(ResultRef, Result) ->
    receive
        {get_child_result, ResultRef, Requester} ->
            Requester ! {async_child, ResultRef, Result},
            child_result_holder(ResultRef, Result)
    end.

%% RunSynchronously: run CPS chain in current process, capture result
run_synchronously(Computation) -> run_synchronously(Computation, undefined).
run_synchronously(Computation, CancelToken) ->
    %% Use a unique ref as key to store result in process dict
    Ref = make_ref(),
    %% Take over stacktrace parking for the duration of this run, handing it back afterwards so a
    %% nested run (sequential/1, or one inside a child process) can neither consume nor clobber
    %% the entry of the run it is nested in.
    PrevRun = put(?RUN_KEY, Ref),
    PrevParked = erase(?STACKTRACE_KEY),
    Ctx = #{
        on_success => fun(V) -> put(Ref, {ok, V}) end,
        on_error => fun(E) -> put(Ref, {error, E}) end,
        on_cancel => fun(_) -> put(Ref, {cancelled}) end,
        cancel_token => CancelToken
    },
    try
        Computation(Ctx)
    catch
        _:Err:Stacktrace ->
            record_stacktrace(Err, Stacktrace),
            put(Ref, {error, Err})
    end,
    Result = erase(Ref),
    Parked = erase(?STACKTRACE_KEY),
    put(?RUN_KEY, PrevRun),
    case PrevParked of
        undefined -> ok;
        _ -> put(?STACKTRACE_KEY, PrevParked)
    end,
    case Result of
        {ok, Value} -> Value;
        {error, Error} -> reraise(Ref, Parked, Error);
        {cancelled} -> erlang:error(operation_cancelled);
        undefined -> erlang:error(async_no_result)
    end.

%% StartWithContinuations
start_with_continuations(Comp, OnSuccess, OnError, OnCancel) ->
    start_with_continuations(Comp, OnSuccess, OnError, OnCancel, undefined).
start_with_continuations(Comp, OnSuccess, OnError, OnCancel, Token) ->
    Ctx = #{
        on_success => OnSuccess,
        on_error => OnError,
        on_cancel => OnCancel,
        cancel_token => Token
    },
    try
        Comp(Ctx)
    catch
        %% No stacktrace parking here: the caller supplies OnError and receives the error term
        %% directly, so there is no re-raise for a parked stacktrace to be restored onto.
        _:Err -> OnError(Err)
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

%% AwaitEvent: subscribe once; complete async when event fires.
await_event(Event) -> await_event(Event, undefined).
await_event(Event, CancelAction) ->
    fun(Ctx) ->
        OnSuccess = maps:get(on_success, Ctx),
        OnCancel = maps:get(on_cancel, Ctx),
        Token = maps:get(cancel_token, Ctx),

        Handler = fun F(_Sender, Value) ->
            (maps:get(remove_handler, Event))(F),
            OnSuccess(Value)
        end,

        case Token of
            undefined -> ok;
            _ ->
                fable_cancellation:register(Token, fun(_) ->
                    (maps:get(remove_handler, Event))(Handler),
                    case CancelAction of
                        undefined -> ok;
                        _ -> CancelAction(ok)
                    end,
                    OnCancel(ok)
                end)
        end,

        (maps:get(add_handler, Event))(Handler)
    end.

%% Parallel: spawn one process per computation, collect results in order
parallel(Computations) when is_reference(Computations) ->
    parallel(get(Computations));
parallel(Computations) ->
    fun(Ctx) ->
        Self = self(),
        Indexed = lists:zip(lists:seq(1, length(Computations)), Computations),
        lists:foreach(
            fun({Idx, Comp}) ->
                spawn(fun() ->
                    try
                        Result = fable_async:run_synchronously(Comp),
                        Self ! {async_parallel, Idx, {ok, Result}}
                    catch
                        _:Err ->
                            Self ! {async_parallel, Idx, {error, Err}}
                    end
                end)
            end,
            Indexed
        ),
        Results = collect_parallel(length(Computations), #{}),
        case Results of
            {ok, Map} ->
                Ordered = [maps:get(I, Map) || I <- lists:seq(1, length(Computations))],
                (maps:get(on_success, Ctx))(fable_utils:new_ref(Ordered));
            {error, Err} ->
                (maps:get(on_error, Ctx))(Err)
        end
    end.

collect_parallel(0, Acc) ->
    {ok, Acc};
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
%% Uses atom tags matching Beam union representation: choice1_of2 = Choice1Of2, choice2_of2 = Choice2Of2
catch_async(Computation) ->
    fun(Ctx) ->
        Ctx2 = #{
            on_success => fun(V) -> (maps:get(on_success, Ctx))({choice1_of2, V}) end,
            on_error => fun(E) -> (maps:get(on_success, Ctx))({choice2_of2, wrap_error(E)}) end,
            on_cancel => maps:get(on_cancel, Ctx),
            cancel_token => maps:get(cancel_token, Ctx)
        },
        try
            Computation(Ctx2)
        catch
            _:Err -> (maps:get(on_success, Ctx))({choice2_of2, wrap_error(Err)})
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

%% Wrap raw error values as exception maps so .Message accessor works.
%% Errors from failwith are raw binaries; errors from raise(exn ...) are already maps.
wrap_error(Err) when is_binary(Err) -> #{message => Err};
wrap_error(Err) when is_map(Err) -> Err;
wrap_error(Err) when is_reference(Err) -> Err;
wrap_error(Err) -> #{message => erlang:iolist_to_binary(io_lib:format("~p", [Err]))}.
