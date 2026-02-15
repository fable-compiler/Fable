-module(fable_queue).
-export([
    create_empty/0, create_from_list/1,
    enqueue/2, dequeue/1, try_dequeue/1, try_dequeue/2,
    peek/1, try_peek/1, try_peek/2,
    contains/2, get_count/1, clear/1,
    to_array/1, trim_excess/1,
    get_enumerator/1
]).

%% Queue is a mutable FIFO stored in the process dictionary.
%% Uses Erlang's queue module for amortized O(1) enqueue/dequeue.

create_empty() ->
    Ref = make_ref(),
    put(Ref, queue:new()),
    Ref.

create_from_list(Items) when is_list(Items) ->
    Ref = make_ref(),
    put(Ref, queue:from_list(Items)),
    Ref.

enqueue(QRef, Item) ->
    put(QRef, queue:in(Item, get(QRef))),
    ok.

dequeue(QRef) ->
    case queue:out(get(QRef)) of
        {{value, Item}, Q2} ->
            put(QRef, Q2),
            Item;
        {empty, _} ->
            erlang:error({badmatch, <<"Queue is empty">>})
    end.

try_dequeue(QRef) ->
    case queue:out(get(QRef)) of
        {{value, Item}, Q2} ->
            put(QRef, Q2),
            {true, Item};
        {empty, _} ->
            {false, undefined}
    end.

%% TryDequeue with out-ref: sets OutRef and returns just bool
try_dequeue(QRef, OutRef) ->
    case queue:out(get(QRef)) of
        {{value, Item}, Q2} ->
            put(QRef, Q2),
            put(OutRef, Item),
            true;
        {empty, _} ->
            false
    end.

peek(QRef) ->
    case queue:peek(get(QRef)) of
        {value, Item} -> Item;
        empty -> erlang:error({badmatch, <<"Queue is empty">>})
    end.

try_peek(QRef) ->
    case queue:peek(get(QRef)) of
        {value, Item} -> {true, Item};
        empty -> {false, undefined}
    end.

%% TryPeek with out-ref: sets OutRef and returns just bool
try_peek(QRef, OutRef) ->
    case queue:peek(get(QRef)) of
        {value, Item} ->
            put(OutRef, Item),
            true;
        empty ->
            false
    end.

contains(QRef, Item) ->
    queue:member(Item, get(QRef)).

get_count(QRef) ->
    queue:len(get(QRef)).

clear(QRef) ->
    put(QRef, queue:new()),
    ok.

to_array(QRef) ->
    queue:to_list(get(QRef)).

trim_excess(_QRef) ->
    %% No-op in Erlang (no pre-allocation)
    ok.

get_enumerator(QRef) ->
    fable_utils:get_enumerator(queue:to_list(get(QRef))).
