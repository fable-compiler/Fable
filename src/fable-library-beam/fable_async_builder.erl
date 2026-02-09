-module(fable_async_builder).
-export([singleton/0, singleton/1, task/0, task/1, run/1,
         delay/1, return/1, return_from/1,
         bind/2, combine/2, zero/0, zero/1,
         try_with/2, try_finally/2, using/2,
         while/2, for/2]).

%% The singleton builder instance (just an atom tag)
singleton() -> async_builder.
singleton(_Unit) -> async_builder.

%% Task builder singleton (alias for async builder in Erlang)
task() -> task_builder.
task(_Unit) -> task_builder.

%% Run: execute a delayed computation (identity for CPS — just returns the computation)
run(Computation) -> Computation.

%% Return: wrap value in immediate success continuation
return(Value) ->
    fun(Ctx) -> (maps:get(on_success, Ctx))(Value) end.

%% ReturnFrom: identity — computation is already an Async<T>
return_from(Computation) -> Computation.

%% Zero: return unit
zero() -> return(ok).
zero(_Unit) -> return(ok).

%% Delay: defer generator execution (cold semantics)
delay(Generator) ->
    fun(Ctx) -> (Generator(ok))(Ctx) end.

%% Bind: monadic bind — run computation, pass result to binder
bind(Computation, Binder) ->
    fun(Ctx) ->
        Ctx2 = Ctx#{on_success => fun(X) ->
            try (Binder(X))(Ctx)
            catch _:Err -> (maps:get(on_error, Ctx))(Err)
            end
        end},
        Computation(Ctx2)
    end.

%% Combine: run first, ignore result, run second
combine(Comp1, Comp2) ->
    bind(Comp1, fun(_) -> Comp2 end).

%% TryWith: catch errors in computation, pass to handler
try_with(Computation, Handler) ->
    fun(Ctx) ->
        Ctx2 = Ctx#{on_error => fun(Err) ->
            try (Handler(Err))(Ctx)
            catch _:Err2 -> (maps:get(on_error, Ctx))(Err2)
            end
        end},
        try Computation(Ctx2)
        catch _:Err ->
            try (Handler(Err))(Ctx)
            catch _:Err2 -> (maps:get(on_error, Ctx))(Err2)
            end
        end
    end.

%% TryFinally: ensure compensation runs on any exit path
try_finally(Computation, Compensation) ->
    fun(Ctx) ->
        Ctx2 = Ctx#{
            on_success => fun(X) -> Compensation(), (maps:get(on_success, Ctx))(X) end,
            on_error => fun(E) -> Compensation(), (maps:get(on_error, Ctx))(E) end,
            on_cancel => fun(E) -> Compensation(), (maps:get(on_cancel, Ctx))(E) end
        },
        try Computation(Ctx2)
        catch _:Err -> Compensation(), (maps:get(on_error, Ctx))(Err)
        end
    end.

%% Using: TryFinally with IDisposable (Dispose = no-op in Erlang for now)
using(Resource, Binder) ->
    try_finally(Binder(Resource), fun() -> ok end).

%% While: recursive bind while guard is true
while(Guard, Computation) ->
    case Guard(ok) of
        true -> bind(Computation, fun(_) -> while(Guard, Computation) end);
        false -> zero()
    end.

%% For: iterate over list, bind body for each element
for(List, Body) ->
    case List of
        [] -> zero();
        [H|T] -> bind(Body(H), fun(_) -> for(T, Body) end)
    end.
