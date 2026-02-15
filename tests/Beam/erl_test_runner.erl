-module(erl_test_runner).
-export([main/1]).

main([Dir]) ->
    %% Load all .beam files in the directory
    Beams = filelib:wildcard(filename:join(Dir, "*.beam")),
    Modules = [list_to_atom(filename:basename(F, ".beam")) || F <- Beams,
               filename:basename(F, ".beam") =/= "erl_test_runner",
               lists:suffix("_tests", filename:basename(F, ".beam"))],
    %% For each module, find exported functions with arity 1 (test functions)
    Results = lists:foldl(fun(Mod, Acc) ->
        code:purge(Mod),
        code:load_file(Mod),
        Exports = Mod:module_info(exports),
        TestFuns = [{Mod, F} || {F, 0} <- Exports,
                     F =/= module_info,
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
