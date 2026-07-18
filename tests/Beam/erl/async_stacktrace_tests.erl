%% Stacktrace handling in the async runtime.
%%
%% These are native Erlang tests rather than F# ones because they assert on Erlang stack frames,
%% which have no counterpart on .NET — the F# suite runs there first, so it cannot express them.
-module(async_stacktrace_tests).

-export([
    test_run_synchronously_keeps_original_stacktrace/0,
    test_handled_stacktrace_is_not_reused/0,
    test_nested_run_keeps_own_stacktrace/0
]).

%% The frame every test below looks for: an error raised at a known place in this module.
boom(Term) -> erlang:error(Term).

%% An async computation that raises from inside a bind, i.e. through the runtime's catch sites
%% rather than straight out of run_synchronously.
raising_async(Term) ->
    fable_async_builder:bind(
        fable_async_builder:return(ok),
        fun(_) -> fun(_Ctx) -> boom(Term) end end
    ).

run_catching(Comp) ->
    try fable_async:run_synchronously(Comp) of
        Value -> {ok, Value}
    catch
        error:Err:Stacktrace -> {error, Err, Stacktrace}
    end.

has_boom_on_top([{?MODULE, boom, _, _} | _]) -> true;
has_boom_on_top(_) -> false.

%% The failure is reported where it happened, not at run_synchronously's re-raise.
test_run_synchronously_keeps_original_stacktrace() ->
    {error, boom_here, Stacktrace} = run_catching(raising_async(boom_here)),
    case has_boom_on_top(Stacktrace) of
        true -> ok;
        false -> erlang:error({expected_original_frame_on_top, Stacktrace})
    end.

%% A stacktrace parked for an error that was *handled* must not be restored onto a later,
%% unrelated error that happens to be an equal term.
test_handled_stacktrace_is_not_reused() ->
    Comp = fable_async_builder:bind(
        %% Raise `same_term` and handle it: parks a stacktrace, then discards it.
        fable_async_builder:try_with(
            raising_async(same_term),
            fun(_Err) -> fable_async_builder:return(ok) end
        ),
        %% Then fail with an equal term that carries no stacktrace of its own.
        fun(_) ->
            fable_async:from_continuations(
                fun({_OnSuccess, OnError, _OnCancel}) -> OnError(same_term) end
            )
        end
    ),
    {error, same_term, Stacktrace} = run_catching(Comp),
    case has_boom_on_top(Stacktrace) of
        true -> erlang:error({stale_stacktrace_restored, Stacktrace});
        false -> ok
    end.

%% A run nested inside another (here via sequential/1) must not consume or clobber the outer run's
%% parked entry: the outer failure still reports its own origin.
test_nested_run_keeps_own_stacktrace() ->
    Comp = fable_async_builder:bind(
        fable_async:sequential([fable_async_builder:return(ok)]),
        fun(_) -> raising_async(outer_boom) end
    ),
    {error, outer_boom, Stacktrace} = run_catching(Comp),
    case has_boom_on_top(Stacktrace) of
        true -> ok;
        false -> erlang:error({expected_original_frame_on_top, Stacktrace})
    end.
