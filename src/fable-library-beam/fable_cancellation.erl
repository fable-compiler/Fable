-module(fable_cancellation).
-export([
    create/0, create/1,
    cancel/1,
    cancel_after/2,
    is_cancellation_requested/1,
    throw_if_cancellation_requested/1,
    register/2, register/3
]).

-spec create() -> reference().
-spec create(undefined | boolean() | integer()) -> reference().
-spec cancel(reference() | undefined) -> ok.
-spec cancel_after(reference() | undefined, non_neg_integer()) -> ok.
-spec is_cancellation_requested(reference() | undefined) -> boolean().
-spec throw_if_cancellation_requested(reference() | undefined) -> ok.
-spec register(reference() | undefined, fun()) -> map() | undefined.
-spec register(reference() | undefined, fun(), term()) -> map() | undefined.

-define(SERVER, fable_cancellation_server).

%% CancellationToken/CancellationTokenSource runtime.
%%
%% decision: stores the cancellation bit in atomics so token state is shared and garbage-collected
%% with the token instead of being tied to any BEAM process or ETS owner
%% decision: serializes registration, cancellation, and timers through one runtime-wide broker —
%% this closes the register-versus-cancel race without leaking one process per token
%% invariant: the broker never invokes a callback for direct cancellation — Cancel runs callbacks
%% synchronously in its caller, matching CancellationTokenSource.Cancel semantics
%% tradeoff: cross-process callbacks cannot capture process-dictionary-backed Fable mutable values
%% because Cancel runs them in its caller and CancelAfter uses a short-lived worker

create() -> create(undefined).

create(undefined) ->
    atomics:new(1, []);
create(true) ->
    Token = create(undefined),
    atomics:put(Token, 1, 1),
    Token;
create(false) ->
    create(undefined);
create(Ms) when is_integer(Ms) ->
    Token = create(undefined),
    cancel_after(Token, Ms),
    Token.

cancel(undefined) ->
    ok;
cancel(Token) ->
    case call({cancel, Token}) of
        {invoke, Listeners} -> invoke_listeners(Listeners);
        already_cancelled -> ok
    end.

cancel_after(undefined, _Ms) ->
    ok;
cancel_after(Token, Ms) ->
    call({cancel_after, Token, Ms}),
    ok.

is_cancellation_requested(undefined) ->
    false;
is_cancellation_requested(Token) ->
    atomics:get(Token, 1) =:= 1.

throw_if_cancellation_requested(undefined) ->
    ok;
throw_if_cancellation_requested(Token) ->
    case is_cancellation_requested(Token) of
        true -> erlang:error(operation_cancelled);
        false -> ok
    end.

register(Token, F) -> register(Token, F, ok).

register(undefined, _F, _State) ->
    undefined;
register(Token, F, State) ->
    Id = make_ref(),
    case call({register, Token, Id, F, State}) of
        registered ->
            #{dispose => fun(_) -> unregister(Token, Id) end};
        invoke ->
            invoke_listener({F, State}),
            #{dispose => fun(_) -> ok end}
    end.

unregister(Token, Id) ->
    call({unregister, Token, Id}),
    ok.

%% Internal helpers

invoke_listeners(Listeners) ->
    lists:foreach(fun invoke_listener/1, Listeners).

invoke_listener({F, State}) ->
    try
        F(State)
    catch
        _:_ -> ok
    end.

call(Request) ->
    Server = server(),
    Ref = make_ref(),
    Monitor = erlang:monitor(process, Server),
    Server ! {call, self(), Ref, Request},
    receive
        {Ref, Reply} ->
            erlang:demonitor(Monitor, [flush]),
            Reply;
        {'DOWN', Monitor, process, Server, Reason} ->
            erlang:error({cancellation_server_stopped, Reason})
    end.

server() ->
    case whereis(?SERVER) of
        undefined -> start_server();
        Pid -> Pid
    end.

start_server() ->
    Pid = spawn(fun() -> server_loop(#{}) end),
    try erlang:register(?SERVER, Pid) of
        true -> Pid
    catch
        error:badarg ->
            exit(Pid, kill),
            server()
    end.

server_loop(Entries) ->
    receive
        {call, From, Ref, {register, Token, Id, F, State}} ->
            case atomics:get(Token, 1) of
                1 ->
                    From ! {Ref, invoke},
                    server_loop(Entries);
                0 ->
                    Entry = maps:get(Token, Entries, new_entry()),
                    Listeners = maps:get(listeners, Entry),
                    Entry1 = Entry#{listeners := Listeners#{Id => {F, State}}},
                    From ! {Ref, registered},
                    server_loop(Entries#{Token => Entry1})
            end;
        {call, From, Ref, {unregister, Token, Id}} ->
            Entries1 = update_entry(Token, fun(Entry) ->
                Listeners = maps:get(listeners, Entry),
                Entry#{listeners := maps:remove(Id, Listeners)}
            end, Entries),
            From ! {Ref, ok},
            server_loop(Entries1);
        {call, From, Ref, {cancel, Token}} ->
            case atomics:compare_exchange(Token, 1, 0, 1) of
                ok ->
                    {Listeners, Entries1} = take_entry(Token, Entries),
                    From ! {Ref, {invoke, Listeners}},
                    server_loop(Entries1);
                1 ->
                    From ! {Ref, already_cancelled},
                    server_loop(Entries)
            end;
        {call, From, Ref, {cancel_after, Token, Ms}} ->
            Entries1 = schedule_cancel_after(Token, Ms, Entries),
            From ! {Ref, ok},
            server_loop(Entries1);
        {cancel_after, Token, Generation} ->
            case maps:find(Token, Entries) of
                {ok, #{timer := {_TimerRef, Generation}}} ->
                    case atomics:compare_exchange(Token, 1, 0, 1) of
                        ok ->
                            {Listeners, Entries1} = take_entry(Token, Entries),
                            %% CancelAfter has no calling process in which callbacks can run.
                            %% A short-lived worker keeps user code out of the state broker.
                            spawn(fun() -> invoke_listeners(Listeners) end),
                            server_loop(Entries1);
                        1 ->
                            {_Listeners, Entries1} = take_entry(Token, Entries),
                            server_loop(Entries1)
                    end;
                _ ->
                    server_loop(Entries)
            end
    end.

new_entry() ->
    #{listeners => #{}, timer => undefined}.

schedule_cancel_after(Token, Ms, Entries) ->
    case atomics:get(Token, 1) of
        1 -> Entries;
        0 ->
            Entry = maps:get(Token, Entries, new_entry()),
            cancel_timer(maps:get(timer, Entry)),
            Generation = make_ref(),
            TimerRef = erlang:send_after(Ms, self(), {cancel_after, Token, Generation}),
            Entries#{Token => Entry#{timer := {TimerRef, Generation}}}
    end.

update_entry(Token, F, Entries) ->
    case maps:find(Token, Entries) of
        error -> Entries;
        {ok, Entry} ->
            Entry1 = F(Entry),
            case entry_empty(Entry1) of
                true -> maps:remove(Token, Entries);
                false -> Entries#{Token := Entry1}
            end
    end.

take_entry(Token, Entries) ->
    case maps:take(Token, Entries) of
        error -> {[], Entries};
        {Entry, Entries1} ->
            cancel_timer(maps:get(timer, Entry)),
            {maps:values(maps:get(listeners, Entry)), Entries1}
    end.

entry_empty(#{listeners := Listeners, timer := Timer}) ->
    map_size(Listeners) =:= 0 andalso Timer =:= undefined.

cancel_timer(undefined) -> ok;
cancel_timer({TimerRef, _Generation}) ->
    erlang:cancel_timer(TimerRef),
    ok.
