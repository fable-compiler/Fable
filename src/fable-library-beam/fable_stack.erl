-module(fable_stack).
-export([
    create_empty/0, create_from_list/1,
    push/2, pop/1, try_pop/1, try_pop/2,
    peek/1, try_peek/1, try_peek/2,
    contains/2, get_count/1, clear/1,
    to_array/1,
    get_enumerator/1
]).

%% Stack is a mutable LIFO stored in the process dictionary.
%% Uses a plain list (head = top of stack).

create_empty() ->
    Ref = make_ref(),
    put(Ref, []),
    Ref.

create_from_list(Items) when is_list(Items) ->
    %% Stack from IEnumerable: items are pushed in order,
    %% so first item ends up at bottom. Reverse so iteration
    %% matches .NET behavior (LIFO order).
    Ref = make_ref(),
    put(Ref, lists:reverse(Items)),
    Ref.

push(SRef, Item) ->
    put(SRef, [Item | get(SRef)]),
    ok.

pop(SRef) ->
    case get(SRef) of
        [H | T] ->
            put(SRef, T),
            H;
        [] ->
            erlang:error({badmatch, <<"Stack is empty">>})
    end.

try_pop(SRef) ->
    case get(SRef) of
        [H | T] ->
            put(SRef, T),
            {true, H};
        [] ->
            {false, undefined}
    end.

%% TryPop with out-ref: sets OutRef and returns just bool
try_pop(SRef, OutRef) ->
    case get(SRef) of
        [H | T] ->
            put(SRef, T),
            put(OutRef, H),
            true;
        [] ->
            false
    end.

peek(SRef) ->
    case get(SRef) of
        [H | _] -> H;
        [] -> erlang:error({badmatch, <<"Stack is empty">>})
    end.

try_peek(SRef) ->
    case get(SRef) of
        [H | _] -> {true, H};
        [] -> {false, undefined}
    end.

%% TryPeek with out-ref: sets OutRef and returns just bool
try_peek(SRef, OutRef) ->
    case get(SRef) of
        [H | _] ->
            put(OutRef, H),
            true;
        [] ->
            false
    end.

contains(SRef, Item) ->
    lists:member(Item, get(SRef)).

get_count(SRef) ->
    length(get(SRef)).

clear(SRef) ->
    put(SRef, []),
    ok.

to_array(SRef) ->
    %% Returns in stack order (top first = LIFO)
    get(SRef).

get_enumerator(SRef) ->
    fable_utils:get_enumerator(get(SRef)).
