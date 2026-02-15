-module(fable_cancellation).
-export([create/0, create/1, cancel/1, cancel_after/2,
         is_cancellation_requested/1, throw_if_cancellation_requested/1,
         register/2, register/3]).

%% CancellationToken/CancellationTokenSource runtime.
%% Token = make_ref(), stored in process dict as:
%%   #{cancelled => false, listeners => #{}, next_id => 1}
%%
%% Uses process dict pattern (same as Ref/Dictionary/ResizeArray).
%% Since Beam async runs same-process CPS, no cross-process communication needed.

create() -> create(undefined).

create(undefined) ->
    Token = make_ref(),
    put(Token, #{cancelled => false, listeners => #{}, next_id => 1}),
    Token;
create(true) ->
    Token = make_ref(),
    put(Token, #{cancelled => true, listeners => #{}, next_id => 1}),
    Token;
create(false) ->
    create(undefined);
create(Ms) when is_integer(Ms) ->
    Token = create(undefined),
    cancel_after(Token, Ms),
    Token.

cancel(undefined) -> ok;
cancel(Token) ->
    State = get(Token),
    put(Token, State#{cancelled := true}),
    invoke_listeners(maps:get(listeners, State)),
    ok.

cancel_after(undefined, _Ms) -> ok;
cancel_after(Token, Ms) ->
    Self = self(),
    timer:apply_after(Ms, erlang, send, [Self, {cancel_token, Token}]),
    %% Spawn a receiver that watches for the cancel message
    %% Since we're same-process CPS, we set up a process dict flag
    %% and check it. But for timer-based cancel we need a receive loop.
    %% Use a helper process that sends us a message we handle in sleep.
    ok.

is_cancellation_requested(undefined) -> false;
is_cancellation_requested(Token) ->
    drain_cancel_messages(),
    maps:get(cancelled, get(Token)).

throw_if_cancellation_requested(undefined) -> ok;
throw_if_cancellation_requested(Token) ->
    drain_cancel_messages(),
    case maps:get(cancelled, get(Token)) of
        true -> erlang:error(operation_cancelled);
        false -> ok
    end.

register(Token, F) -> register(Token, F, undefined).

register(undefined, _F, _State) -> undefined;
register(Token, F, _State) ->
    State = get(Token),
    Id = maps:get(next_id, State),
    Listeners = maps:get(listeners, State),
    put(Token, State#{listeners := Listeners#{Id => F}, next_id := Id + 1}),
    %% Return a disposable-like value (the registration id + token)
    {Token, Id}.

%% Internal helpers

invoke_listeners(Listeners) ->
    maps:foreach(fun(_Id, F) ->
        try F(ok)
        catch _:_ -> ok
        end
    end, Listeners).

%% Drain any pending {cancel_token, Token} messages from the mailbox
%% and apply cancellation. This is needed for timer-based cancellation
%% since timer:apply_after sends a message to self().
drain_cancel_messages() ->
    receive
        {cancel_token, Token} ->
            State = get(Token),
            case maps:get(cancelled, State) of
                true -> ok;
                false ->
                    put(Token, State#{cancelled := true}),
                    invoke_listeners(maps:get(listeners, State))
            end,
            drain_cancel_messages()
    after 0 ->
        ok
    end.
