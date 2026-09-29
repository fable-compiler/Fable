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
-define(CALLBACK_KEY, '$fable_cancellation_callback').

%% CancellationToken/CancellationTokenSource runtime.
%%
%% decision: stores the cancellation bit in atomics so token state is shared and garbage-collected
%% with the token instead of being tied to any BEAM process or ETS owner
%% decision: serializes registration, cancellation, and timers through one runtime-wide broker —
%% this closes the register-versus-cancel race without leaking one process per token
%% invariant: the broker never invokes a callback for direct cancellation — Cancel runs callbacks
%% synchronously in its caller, matching CancellationTokenSource.Cancel semantics
%% invariant: a running registration remains broker-owned until its callback completes — Dispose
%% can therefore wait for callbacks executing in another process
%% tradeoff: user callbacks cannot capture process-dictionary-backed Fable mutable values across
%% processes because Cancel runs them in its caller and CancelAfter uses a short-lived worker;
%% runtime-owned callbacks must marshal their work back to the registering process

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
        {invoke, Listeners} -> invoke_listeners(Token, Listeners);
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
    IsCurrentCallback = get(?CALLBACK_KEY) =:= {Token, Id},
    call({unregister, Token, Id, IsCurrentCallback}),
    ok.

%% Internal helpers

invoke_listeners(Token, Listeners) ->
    lists:foreach(fun(Listener) -> invoke_listener(Token, Listener) end, Listeners).

invoke_listener(Token, {Id, F, State}) ->
    PreviousCallback = put(?CALLBACK_KEY, {Token, Id}),
    invoke_listener({F, State}),
    case PreviousCallback of
        undefined -> erase(?CALLBACK_KEY);
        _ -> put(?CALLBACK_KEY, PreviousCallback)
    end,
    call({callback_complete, Token, Id}),
    ok.

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
        {call, From, Ref, {unregister, Token, Id, IsCurrentCallback}} ->
            {Entries1, Reply} = unregister_listener(
                Token, Id, From, Ref, IsCurrentCallback, Entries
            ),
            case Reply of
                now -> From ! {Ref, ok};
                after_callback -> ok
            end,
            server_loop(Entries1);
        {call, From, Ref, {cancel, Token}} ->
            case atomics:compare_exchange(Token, 1, 0, 1) of
                ok ->
                    {Listeners, Entries1} = begin_callbacks(Token, From, Entries),
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
                            %% CancelAfter has no calling process in which callbacks can run.
                            %% A short-lived worker keeps user code out of the state broker.
                            Worker = spawn(fun() ->
                                receive
                                    {invoke, Listeners} -> invoke_listeners(Token, Listeners)
                                end
                            end),
                            {Listeners, Entries1} = begin_callbacks(Token, Worker, Entries),
                            Worker ! {invoke, Listeners},
                            server_loop(Entries1);
                        1 ->
                            Entries1 = remove_timer(Token, Entries),
                            server_loop(Entries1)
                    end;
                _ ->
                    server_loop(Entries)
            end;
        {call, From, Ref, {callback_complete, Token, Id}} ->
            Entries1 = complete_callback(Token, Id, Entries),
            From ! {Ref, ok},
            server_loop(Entries1)
    end.

new_entry() ->
    #{listeners => #{}, running => #{}, waiters => #{}, timer => undefined}.

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

begin_callbacks(Token, Owner, Entries) ->
    case maps:find(Token, Entries) of
        error -> {[], Entries};
        {ok, Entry} ->
            cancel_timer(maps:get(timer, Entry)),
            Listeners = maps:get(listeners, Entry),
            Running = maps:from_list([{Id, Owner} || Id <- maps:keys(Listeners)]),
            Callbacks = [{Id, F, State} || {Id, {F, State}} <- maps:to_list(Listeners)],
            Entry1 = Entry#{listeners := #{}, running := Running, timer := undefined},
            {Callbacks, put_entry(Token, Entry1, Entries)}
    end.

unregister_listener(Token, Id, From, Ref, IsCurrentCallback, Entries) ->
    case maps:find(Token, Entries) of
        error -> {Entries, now};
        {ok, Entry} ->
            Listeners = maps:get(listeners, Entry),
            Running = maps:get(running, Entry),
            case maps:find(Id, Running) of
                {ok, From} when IsCurrentCallback ->
                    %% A callback may dispose its own registration; waiting here would deadlock.
                    {Entries, now};
                {ok, _Owner} ->
                    Waiters = maps:get(waiters, Entry),
                    RegistrationWaiters = maps:get(Id, Waiters, []),
                    Entry1 = Entry#{waiters := Waiters#{Id => [{From, Ref} | RegistrationWaiters]}},
                    {Entries#{Token := Entry1}, after_callback};
                error ->
                    Entry1 = Entry#{listeners := maps:remove(Id, Listeners)},
                    {put_entry(Token, Entry1, Entries), now}
            end
    end.

complete_callback(Token, Id, Entries) ->
    case maps:find(Token, Entries) of
        error -> Entries;
        {ok, Entry} ->
            Waiters = maps:get(waiters, Entry),
            lists:foreach(fun({Pid, Ref}) -> Pid ! {Ref, ok} end, maps:get(Id, Waiters, [])),
            Entry1 = Entry#{
                running := maps:remove(Id, maps:get(running, Entry)),
                waiters := maps:remove(Id, Waiters)
            },
            put_entry(Token, Entry1, Entries)
    end.

remove_timer(Token, Entries) ->
    update_entry(Token, fun(Entry) -> Entry#{timer := undefined} end, Entries).

put_entry(Token, Entry, Entries) ->
    case entry_empty(Entry) of
        true -> maps:remove(Token, Entries);
        false -> Entries#{Token => Entry}
    end.

entry_empty(#{listeners := Listeners, running := Running, waiters := Waiters, timer := Timer}) ->
    map_size(Listeners) =:= 0 andalso
        map_size(Running) =:= 0 andalso
        map_size(Waiters) =:= 0 andalso
        Timer =:= undefined.

cancel_timer(undefined) -> ok;
cancel_timer({TimerRef, _Generation}) ->
    erlang:cancel_timer(TimerRef),
    ok.
