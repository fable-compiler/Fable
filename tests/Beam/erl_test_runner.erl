-module(erl_test_runner).
-export([main/1]).

main([Dir]) ->
    %% Load all .beam files in the directory
    Beams = filelib:wildcard(filename:join(Dir, "*.beam")),
    Modules = [list_to_atom(filename:basename(F, ".beam")) || F <- Beams,
               filename:basename(F, ".beam") =/= "erl_test_runner",
               string:find(filename:basename(F, ".beam"), "_tests") =/= nomatch],
    %% For each module, find exported functions with arity 1 (test functions)
    Results = lists:foldl(fun(Mod, Acc) ->
        code:purge(Mod),
        code:load_file(Mod),
        Exports = Mod:module_info(exports),
        %% Run the module initializer (main/0) before the module's tests, if present.
        %% F# evaluates module-level bindings (including mutable values and `do` actions)
        %% once before any module code runs; main/0 carries that initialization, so it must
        %% execute before the test functions that read module-level state.
        case lists:member({main, 0}, Exports) of
            true ->
                try Mod:main()
                catch InitClass:InitReason ->
                    %% Don't abort the run on a broken initializer, but log it so a
                    %% failing main/0 is diagnosable instead of surfacing later as
                    %% silent `undefined` reads of module-level state.
                    io:format("  WARN init failed ~s:main/0 - ~p:~p~n", [Mod, InitClass, InitReason]),
                    ok
                end;
            false -> ok
        end,
        TestFuns = [{Mod, F} || {F, 0} <- Exports,
                     F =/= module_info,
                     F =/= main,
                     lists:prefix("test_", atom_to_list(F))],
        lists:foldl(fun({M, F}, {Pass, Fail}) ->
            try
                M:F(),
                io:format("  PASS ~s:~s~n", [M, F]),
                {Pass + 1, Fail}
            catch
                error:Reason ->
                    io:format("  FAIL ~s:~s - ~p~n", [M, F, Reason]),
                    {Pass, Fail + 1};
                Class:Reason ->
                    io:format("  FAIL ~s:~s - ~p:~p~n", [M, F, Class, Reason]),
                    {Pass, Fail + 1}
            end
        end, Acc, TestFuns)
    end, {0, 0}, Modules),
    {Pass, Fail} = Results,
    io:format("~n~p passed, ~p failed~n", [Pass, Fail]),
    case Fail of
        0 -> ok;
        _ -> halt(1)
    end.
