-module(fable_mailbox).
-export([
    default/1, default/2,
    start/1, start/2,
    start_instance/1,
    receive_msg/1,
    post/2,
    post_and_async_reply/2
]).

-spec default(fun()) -> map().
-spec default(fun(), term()) -> map().
-spec start(fun()) -> map().
-spec start(fun(), term()) -> map().
-spec start_instance(map()) -> term().
-spec receive_msg(map()) -> fun().
-spec post(map(), term()) -> ok.
-spec post_and_async_reply(map(), fun()) -> fun().

%% Constructor: create agent with empty queue, not started.
%% State stored in process dict keyed by a unique Ref.
default(Body) -> default(Body, undefined).
default(Body, CancelToken) ->
    Ref = make_ref(),
    put(Ref, #{body => Body, messages => [], continuation => undefined, cancel_token => CancelToken}),
    #{ref => Ref}.

%% Static Start: create + start + return agent
start(Body) -> start(Body, undefined).
start(Body, CancelToken) ->
    Agent = default(Body, CancelToken),
    start_instance(Agent),
    Agent.

%% Instance Start: run body with agent as inbox
start_instance(Agent) ->
    Ref = maps:get(ref, Agent),
    State = get(Ref),
    Body = maps:get(body, State),
    fable_async:start_immediate(Body(Agent)).

%% Receive: return Async<Msg>
%% Named receive_msg because 'receive' is an Erlang reserved keyword.
receive_msg(Agent) ->
    fable_async:from_continuations(fun({OnSuccess, _OnError, _OnCancel}) ->
        Ref = maps:get(ref, Agent),
        State = get(Ref),
        case maps:get(messages, State) of
            [Msg | Rest] ->
                %% Message available, consume immediately
                put(Ref, State#{messages => Rest, continuation => undefined}),
                OnSuccess(Msg);
            [] ->
                %% No message, store continuation for later
                put(Ref, State#{continuation => OnSuccess})
        end
    end).

%% Post: add message to queue and process events
post(Agent, Msg) ->
    Ref = maps:get(ref, Agent),
    State = get(Ref),
    NewMessages = maps:get(messages, State) ++ [Msg],
    put(Ref, State#{messages => NewMessages}),
    process_events(Agent).

%% PostAndAsyncReply: create reply channel, post message, return Async<Reply>
%% Everything runs synchronously in the same process, so by the time
%% post returns, the inbox has processed the message and called Reply.
post_and_async_reply(Agent, BuildMessage) ->
    fable_async:from_continuations(fun({OnSuccess, _OnError, _OnCancel}) ->
        ReplyRef = make_ref(),
        ReplyChannel = #{
            reply => fun(Value) ->
                put({reply_result, ReplyRef}, Value)
            end
        },
        Msg = BuildMessage(ReplyChannel),
        post(Agent, Msg),
        Value = erase({reply_result, ReplyRef}),
        OnSuccess(Value)
    end).

%% Internal: if continuation AND message available, invoke continuation with message.
%% Check cancellation token before processing.
process_events(Agent) ->
    Ref = maps:get(ref, Agent),
    State = get(Ref),
    case maps:get(continuation, State) of
        undefined ->
            ok;
        Cont ->
            CancelToken = maps:get(cancel_token, State, undefined),
            case fable_cancellation:is_cancellation_requested(CancelToken) of
                true ->
                    put(Ref, State#{continuation => undefined}),
                    ok;
                false ->
                    case maps:get(messages, State) of
                        [] ->
                            ok;
                        [Msg | Rest] ->
                            put(Ref, State#{messages => Rest, continuation => undefined}),
                            Cont(Msg)
                    end
            end
    end.
