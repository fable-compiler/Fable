-module(fable_utils).
-export([iface_get/2, apply_curried/2, new_ref/1, safe_dispose/1,
         get_enumerator/1, move_next/1, get_current/1,
         pos_infinity/0, neg_infinity/0, nan/0, is_infinity/1,
         new_lazy/1, new_lazy_from_value/1, force_lazy/1, is_value_created/1]).

%% Interface dispatch: works for both object expressions (maps) and class instances (refs).
iface_get(Name, Obj) when is_map(Obj) -> maps:get(Name, Obj);
iface_get(Name, Ref) -> maps:get(Name, get(Ref)).

%% Create a new process dictionary ref cell with the given initial value.
%% Encapsulates make_ref() + put() to avoid variable name collisions in nested constructs.
new_ref(Value) ->
    Ref = make_ref(),
    put(Ref, Value),
    Ref.

%% Safely call Dispose on an object that may or may not implement IDisposable.
%% Handles both ref-based class instances and map-based object expressions.
safe_dispose(Obj) when is_reference(Obj) ->
    safe_dispose(get(Obj));
safe_dispose(Obj) when is_map(Obj) ->
    case maps:is_key(dispose, Obj) of
        true ->
            Fn = maps:get(dispose, Obj),
            case erlang:fun_info(Fn, arity) of
                {arity, 0} -> Fn();
                {arity, _} -> Fn(ok)
            end;
        false -> ok
    end;
safe_dispose(_) -> ok.

%% Apply a list of args one at a time to a curried function.
%% Used by CurriedApply when the target is a qualified call returning a curried function.
apply_curried(Fun, []) -> Fun;
apply_curried(Fun, [Arg | Rest]) -> apply_curried(Fun(Arg), Rest).

%% Enumerator support for for-in loops over lists.
%% Enumerator is a process dict ref pointing to #{items => List, current => undefined}.
get_enumerator(List) when is_list(List) ->
    Ref = make_ref(),
    put(Ref, #{items => List, current => undefined}),
    Ref;
get_enumerator(Ref) when is_reference(Ref) ->
    %% Process dict ref (ResizeArray, HashSet, etc.): unwrap and recurse
    get_enumerator(get(Ref));
get_enumerator(Map) when is_map(Map) ->
    %% HashSet internal map: iterate over keys
    get_enumerator(maps:keys(Map));
get_enumerator(Other) ->
    %% Fallback: treat as list
    get_enumerator(lists:flatten([Other])).

move_next(EnumRef) ->
    State = get(EnumRef),
    case maps:get(items, State) of
        [H | T] ->
            put(EnumRef, State#{items := T, current := H}),
            true;
        [] ->
            false
    end.

get_current(EnumRef) ->
    State = get(EnumRef),
    maps:get(current, State).

%% IEEE 754 special float values.
%% Erlang BEAM VM doesn't support infinity/NaN as float values.
%% Use max finite float as stand-in for infinity, atom nan for NaN.
pos_infinity() -> 1.7976931348623157e308.
neg_infinity() -> -1.7976931348623157e308.
nan() -> nan.
is_infinity(X) when is_float(X) -> X >= 1.7976931348623157e308 orelse X =< -1.7976931348623157e308;
is_infinity(_) -> false.

%% Lazy<T> with memoization via process dictionary.
new_lazy(Factory) ->
    Ref = make_ref(),
    put(Ref, #{factory => Factory, is_value_created => false}),
    Ref.

%% Create an already-evaluated lazy value.
new_lazy_from_value(Value) ->
    Ref = make_ref(),
    put(Ref, #{is_value_created => true, value => Value}),
    Ref.

force_lazy(LazyRef) ->
    State = get(LazyRef),
    case maps:get(is_value_created, State) of
        true -> maps:get(value, State);
        false ->
            Factory = maps:get(factory, State),
            Value = Factory(ok),
            put(LazyRef, State#{is_value_created := true, value => Value}),
            Value
    end.

is_value_created(LazyRef) ->
    maps:get(is_value_created, get(LazyRef)).
