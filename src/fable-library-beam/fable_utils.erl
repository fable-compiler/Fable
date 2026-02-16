-module(fable_utils).
-export([iface_get/2, apply_curried/2, new_ref/1, safe_dispose/1,
         get_enumerator/1, move_next/1, get_current/1,
         pos_infinity/0, neg_infinity/0, nan/0, is_infinity/1,
         new_lazy/1, new_lazy_from_value/1, force_lazy/1, is_value_created/1,
         using/2, to_list/1,
         new_byte_array/1, new_byte_array_zeroed/1, byte_array_to_list/1, is_byte_array/1,
         byte_array_get/2, byte_array_set/3, byte_array_length/1]).

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

%% F# `use` / `using` — execute action with resource, dispose after.
using(Resource, Action) ->
    try Action(Resource)
    after safe_dispose(Resource)
    end.

%% Convert any value to a list — derefs array refs and atomics byte arrays.
to_list(V) when is_reference(V) ->
    case is_byte_array(V) of
        true -> byte_array_to_list(V);
        false -> get(V)
    end;
to_list(V) -> V.

%% Atomics-backed byte arrays for O(1) read/write.
new_byte_array({byte_array, _, _} = BA) ->
    %% Already a byte array — copy it
    new_byte_array(byte_array_to_list(BA));
new_byte_array(Bin) when is_binary(Bin) ->
    %% Binary to byte array
    new_byte_array(binary_to_list(Bin));
new_byte_array(Values) when is_list(Values) ->
    Len = erlang:length(Values),
    case Len of
        0 ->
            {byte_array, 0, atomics:new(1, [{signed, false}])};
        _ ->
            Ref = atomics:new(Len, [{signed, false}]),
            populate_byte_array(Ref, 1, Values),
            {byte_array, Len, Ref}
    end.

%% Create a zero-initialized byte array of given size (atomics are zero by default).
new_byte_array_zeroed(0) ->
    {byte_array, 0, atomics:new(1, [{signed, false}])};
new_byte_array_zeroed(Len) ->
    {byte_array, Len, atomics:new(Len, [{signed, false}])}.

populate_byte_array(_, _, []) -> ok;
populate_byte_array(Ref, Idx, [V|Rest]) ->
    atomics:put(Ref, Idx, V),
    populate_byte_array(Ref, Idx + 1, Rest).

byte_array_to_list({byte_array, 0, _}) -> [];
byte_array_to_list({byte_array, Size, Ref}) ->
    [atomics:get(Ref, I) || I <- lists:seq(1, Size)].

is_byte_array({byte_array, _, _}) -> true;
is_byte_array(_) -> false.

%% O(1) indexed get on byte array (0-based F# index).
byte_array_get({byte_array, _, Ref}, Idx) ->
    atomics:get(Ref, Idx + 1);
byte_array_get(PdRef, Idx) when is_reference(PdRef) ->
    byte_array_get(get(PdRef), Idx).

%% O(1) indexed set on byte array (0-based F# index).
byte_array_set({byte_array, _, Ref}, Idx, Value) ->
    atomics:put(Ref, Idx + 1, Value);
byte_array_set(PdRef, Idx, Value) when is_reference(PdRef) ->
    byte_array_set(get(PdRef), Idx, Value).

%% Length of byte array.
byte_array_length({byte_array, Size, _}) -> Size;
byte_array_length(PdRef) when is_reference(PdRef) ->
    byte_array_length(get(PdRef)).
