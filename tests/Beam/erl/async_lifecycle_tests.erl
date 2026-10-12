%% Native CPS probes complement the F# tests: they exercise cancellation continuations,
%% callback exceptions, and deterministic cross-process handshakes without timing sleeps.
-module(async_lifecycle_tests).
-export([
    test_finalizers_settle_once/0,
    test_nested_cancellation_finalizers/0,
    test_using_settles_once/0,
    test_using_binder_failure/0,
    test_run_synchronously_pending_cancellation/0,
    test_try_with_preserves_channels/0,
    test_failure_racing_cancellation/0,
    test_run_synchronously_timeout/0,
    test_run_synchronously_timeout_preserves_supplied_token/0,
    pending/1
]).

outcome(Comp) ->
    try fable_async:run_synchronously(Comp) of
        Value -> {ok, Value}
    catch error:Reason -> {error, Reason}
    end.

body(success) -> fable_async_builder:return(42);
body(failure) -> fun(Ctx) -> (maps:get(on_error, Ctx))(body_defect) end;
body(throw) -> fun(_) -> erlang:error(body_defect) end;
body(cancel) -> fun(Ctx) -> (maps:get(on_cancel, Ctx))(ok) end.

test_finalizers_settle_once() ->
    lists:foreach(fun(Path) ->
        lists:foreach(fun(Throws) ->
            put(cleanups, 0),
            Comp = fable_async_builder:try_finally(body(Path), fun(_) ->
                put(cleanups, get(cleanups) + 1),
                case Throws of true -> erlang:error(cleanup_defect); false -> ok end
            end),
            Result = outcome(Comp),
            1 = erase(cleanups),
            case {Throws, Path, Result} of
                {true, _, {error, cleanup_defect}} -> ok;
                {false, success, {ok, 42}} -> ok;
                {false, cancel, {error, #{exn_type := operation_canceled_exception}}} -> ok;
                {false, _, {error, body_defect}} -> ok;
                _ -> erlang:error({wrong_finalizer_result, Path, Throws, Result})
            end
        end, [false, true])
    end, [success, failure, throw, cancel]),
    %% A caller's throwing continuation cannot re-enter compensation.
    put(cleanups, 0),
    Comp = fable_async_builder:try_finally(body(success), fun(_) ->
        put(cleanups, get(cleanups) + 1)
    end),
    {error, callback_defect} =
        try Comp(#{
            on_success => fun(_) -> erlang:error(callback_defect) end,
            on_error => fun(_) -> erlang:error(unexpected_error_callback) end,
            on_cancel => fun(_) -> erlang:error(unexpected_cancel_callback) end,
            cancel_token => undefined
        })
        catch error:Reason -> {error, Reason}
        end,
    1 = erase(cleanups),
    ok.

test_nested_cancellation_finalizers() ->
    lists:foreach(fun(OuterThrows) ->
        put(cleanups, []),
        Inner = fable_async_builder:try_finally(body(cancel), fun(_) ->
            put(cleanups, [inner | get(cleanups)]),
            erlang:error(inner_defect)
        end),
        Outer = fable_async_builder:try_finally(Inner, fun(_) ->
            put(cleanups, [outer | get(cleanups)]),
            case OuterThrows of true -> erlang:error(outer_defect); false -> ok end
        end),
        Expected = case OuterThrows of true -> outer_defect; false -> inner_defect end,
        {error, Expected} = outcome(Outer),
        [outer, inner] = erase(cleanups)
    end, [false, true]).

test_using_settles_once() ->
    lists:foreach(fun(Path) ->
        lists:foreach(fun(Arity) ->
            lists:foreach(fun(Throws) ->
                put(disposals, 0),
                Dispose = fun() ->
                    put(disposals, get(disposals) + 1),
                    case Throws of true -> erlang:error(disposal_defect); false -> ok end
                end,
                Callback = case Arity of 0 -> Dispose; 1 -> fun(_) -> Dispose() end end,
                Resource = #{dispose => Callback},
                Result = outcome(fable_async_builder:using(Resource, fun(_) -> body(Path) end)),
                1 = erase(disposals),
                case {Throws, Path, Result} of
                    {true, _, {error, disposal_defect}} -> ok;
                    {false, success, {ok, 42}} -> ok;
                    {false, cancel, {error, #{exn_type := operation_canceled_exception}}} -> ok;
                    {false, _, {error, body_defect}} -> ok;
                    _ -> erlang:error({wrong_using_result, Path, Arity, Throws, Result})
                end
            end, [false, true])
        end, [0, 1])
    end, [success, failure, throw, cancel]),
    {ok, 42} = outcome(fable_async_builder:using(undefined, fun(_) -> body(success) end)),
    ok.

test_using_binder_failure() ->
    put(disposals, 0),
    Resource = #{dispose => fun(_) -> put(disposals, get(disposals) + 1) end},
    Comp = fable_async_builder:using(Resource, fun(_) -> erlang:error(binder_defect) end),
    0 = get(disposals),
    {error, binder_defect} = outcome(Comp),
    1 = erase(disposals),
    ok.

%% Signal readiness only after registering the wake-up; cancellation can now interrupt
%% an admitted pending operation, regardless of scheduler interleaving.
pending(Parent) ->
    fun(Ctx) ->
        Owner = self(),
        Ref = make_ref(),
        Registration = fable_cancellation:register(maps:get(cancel_token, Ctx), fun(_) ->
            Owner ! {cancel, Ref}
        end),
        Parent ! {pending, Owner},
        receive {cancel, Ref} -> ok after 5000 -> erlang:error(pending_not_cancelled) end,
        fable_utils:safe_dispose(Registration),
        (maps:get(on_cancel, Ctx))(ok)
    end.

test_run_synchronously_pending_cancellation() ->
    lists:foreach(fun pending_cancellation/1, [undefined, -1, 5000]).

pending_cancellation(Timeout) ->
    Parent = self(),
    Token = fable_cancellation:create(),
    {Pid, Monitor} = spawn_monitor(fun() ->
        Comp = fable_async_builder:bind(fable_async:cancellation_token(), fun(Actual) ->
            Token = Actual,
            pending(Parent)
        end),
        Result = try fable_async:run_synchronously(Comp, Timeout, Token)
        catch error:Reason -> Reason end,
        exit({result, Result})
    end),
    try
        receive
            {pending, _Worker} -> ok;
            {'DOWN', Monitor, process, Pid, EarlyReason} -> erlang:error({worker_failed_before_pending, EarlyReason})
        after 5000 -> erlang:error(worker_not_pending)
        end,
        fable_cancellation:cancel(Token),
        receive
            {'DOWN', Monitor, process, Pid, {result, #{exn_type := operation_canceled_exception}}} -> ok;
            {'DOWN', Monitor, process, Pid, Reason} -> erlang:error({wrong_cancellation, Reason})
        after 5000 -> erlang:error(worker_not_completed)
        end
    after
        exit(Pid, kill),
        erlang:demonitor(Monitor, [flush])
    end.

test_try_with_preserves_channels() ->
    Comp = fable_async_builder:try_with(body(cancel), fun(_) ->
        erlang:error(cancellation_entered_error_handler)
    end),
    {error, #{exn_type := operation_canceled_exception}} = outcome(Comp),
    {error, #{exn_type := operation_canceled_exception}} = outcome(fable_async:catch_async(body(cancel))),
    {ok, handled} = outcome(fable_async_builder:try_with(body(failure), fun(_) ->
        fable_async_builder:return(handled)
    end)),
    ok.

test_failure_racing_cancellation() ->
    Token = fable_cancellation:create(),
    Comp = fun(_) ->
        fable_cancellation:cancel(Token),
        erlang:error(real_defect)
    end,
    {error, real_defect} = try fable_async:run_synchronously(Comp, undefined, Token)
    catch error:Reason -> {error, Reason} end,
    false = fable_utils:is_exception_type(fable_async:wrap_error(<<"operation_cancelled">>), operation_canceled_exception),
    ok.

test_run_synchronously_timeout() ->
    Parent = self(),
    Comp = fable_async_builder:try_finally(pending(Parent), fun(_) -> Parent ! timeout_cleanup end),
    {error, #{exn_type := timeout_exception}} =
        try fable_async:run_synchronously(Comp, 1000, undefined)
        catch error:Reason -> {error, Reason} end,
    receive {pending, _} -> ok after 5000 -> erlang:error(timeout_worker_not_started) end,
    receive timeout_cleanup -> ok after 5000 -> erlang:error(timeout_skipped_cleanup) end,
    %% No late result or DOWN message may remain in the caller mailbox.
    receive Message -> erlang:error({timeout_mailbox_leak, Message}) after 0 -> ok end.

test_run_synchronously_timeout_preserves_supplied_token() ->
    Token = fable_cancellation:create(),
    Comp = fable_async_builder:bind(fable_async:cancellation_token(), fun(Actual) ->
        Token = Actual,
        fable_async_builder:return(42)
    end),
    42 = fable_async:run_synchronously(Comp, 5000, Token),
    {choice1_of2, 42} = fable_async:run_synchronously(fable_async:catch_async(Comp), 5000, Token),
    false = fable_cancellation:is_cancellation_requested(Token),
    ok.
