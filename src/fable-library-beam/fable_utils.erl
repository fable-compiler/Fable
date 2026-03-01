-module(fable_utils).
-export([
    iface_get/2,
    apply_curried/2,
    new_ref/1,
    safe_dispose/1,
    get_enumerator/1,
    move_next/1,
    get_current/1,
    pos_infinity/0,
    neg_infinity/0,
    nan/0,
    is_infinity/1,
    new_lazy/1,
    new_lazy_from_value/1,
    force_lazy/1,
    is_value_created/1,
    using/2,
    to_list/1,
    enumerate_to_list/1,
    new_byte_array/1,
    new_byte_array_zeroed/1,
    new_byte_array_filled/2,
    byte_array_to_list/1,
    is_byte_array/1,
    byte_array_get/2,
    byte_array_set/3,
    byte_array_length/1,
    div_rem/3
]).

-spec iface_get(atom(), map() | reference()) -> term().
-spec apply_curried(fun(), list()) -> term().
-spec new_ref(term()) -> reference().
-spec safe_dispose(term()) -> ok.
-spec get_enumerator(list() | reference() | map() | term()) -> reference().
-spec move_next(reference()) -> boolean().
-spec get_current(reference()) -> term().
-spec pos_infinity() -> float().
-spec neg_infinity() -> float().
-spec nan() -> nan.
-spec is_infinity(term()) -> boolean().
-spec new_lazy(fun()) -> reference().
-spec new_lazy_from_value(term()) -> reference().
-spec force_lazy(reference()) -> term().
-spec is_value_created(reference()) -> boolean().
-spec using(term(), fun()) -> term().
-spec to_list(term()) -> list().
-spec enumerate_to_list(term()) -> list().
-spec new_byte_array(list() | binary() | tuple()) -> tuple().
-spec new_byte_array_zeroed(non_neg_integer()) -> tuple().
-spec new_byte_array_filled(non_neg_integer(), non_neg_integer()) -> tuple().
-spec byte_array_to_list(tuple()) -> list().
-spec is_byte_array(term()) -> boolean().
-spec byte_array_get(tuple() | reference(), non_neg_integer()) -> non_neg_integer().
-spec byte_array_set(tuple() | reference(), non_neg_integer(), non_neg_integer()) -> ok.
-spec byte_array_length(tuple() | reference()) -> non_neg_integer().
-spec div_rem(integer(), integer(), reference()) -> integer().

%% Interface dispatch: works for both object expressions (maps) and class instances (refs).
%% Class interface property getters are stored as {getter, Fun} tagged thunks — call Fun().
%% ObjectExpr property getters are stored as plain values — return directly.
iface_get(Name, Obj) when is_map(Obj) -> iface_unwrap(maps:get(Name, Obj));
iface_get(Name, Ref) -> iface_unwrap(maps:get(Name, get(Ref))).

iface_unwrap({getter, Fun}) -> Fun();
iface_unwrap(Val) -> Val.

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
        false ->
            ok
    end;
safe_dispose(_) ->
    ok.

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
    Stored = get(Ref),
    case is_map(Stored) of
        true ->
            %% Could be a lazy seq object (has get_enumerator key) or a class instance
            case maps:is_key(get_enumerator, Stored) of
                true ->
                    %% Lazy seq object: call its get_enumerator function
                    (maps:get(get_enumerator, Stored))();
                false ->
                    %% Other map-based ref: try keys (e.g., HashSet-like)
                    get_enumerator(maps:keys(Stored))
            end;
        false ->
            case is_list(Stored) of
                true ->
                    %% Array ref: enumerate the list
                    get_enumerator(Stored);
                false ->
                    %% Fallback
                    get_enumerator(lists:flatten([Stored]))
            end
    end;
get_enumerator(Map) when is_map(Map) ->
    case maps:is_key(get_enumerator, Map) of
        true ->
            %% Lazy seq object (plain map, not ref-wrapped): call its get_enumerator
            (maps:get(get_enumerator, Map))();
        false ->
            %% HashSet internal map: iterate over keys
            get_enumerator(maps:keys(Map))
    end;
get_enumerator({group_collection, Items, _Names}) ->
    %% Regex GroupCollection: iterate over the group items list
    get_enumerator(Items);
get_enumerator(Other) ->
    %% Fallback: treat as list
    get_enumerator(lists:flatten([Other])).

move_next(EnumRef) ->
    State = get(EnumRef),
    case maps:is_key(items, State) of
        true ->
            %% Simple list-based enumerator
            case maps:get(items, State) of
                [H | T] ->
                    put(EnumRef, State#{items := T, current := H}),
                    true;
                [] ->
                    false
            end;
        false ->
            %% Compiled seq.erl enumerator (has move_next function)
            (maps:get(system_collections_i_enumerator_move_next, State))()
    end.

get_current(EnumRef) ->
    State = get(EnumRef),
    case maps:is_key(current, State) of
        true ->
            %% Simple list-based enumerator
            maps:get(current, State);
        false ->
            %% Compiled seq.erl enumerator: call field_current fun
            (maps:get(field_current, State))(ok)
    end.

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
        true ->
            maps:get(value, State);
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
    try
        Action(Resource)
    after
        safe_dispose(Resource)
    end.

%% Convert any value to a list — derefs array refs, atomics byte arrays, and lazy seq objects.
to_list(V) when is_list(V) -> V;
to_list(V) when is_reference(V) ->
    case is_byte_array(V) of
        true ->
            byte_array_to_list(V);
        false ->
            Stored = get(V),
            case is_list(Stored) of
                % array ref
                true ->
                    Stored;
                false ->
                    %% Lazy seq object (ref to map with get_enumerator) — enumerate to list
                    enumerate_to_list(V)
            end
    end;
to_list(V) when is_map(V) ->
    case maps:is_key(get_enumerator, V) of
        true ->
            %% Lazy seq object (plain map, not ref-wrapped)
            enumerate_to_list(V);
        false ->
            %% HashSet (map) — return keys as list
            maps:keys(V)
    end;
to_list(V) when is_binary(V) ->
    %% String binary → list of Unicode codepoints
    unicode:characters_to_list(V);
to_list(V) ->
    V.

%% Enumerate a lazy seq object to a plain list using get_enumerator/move_next/get_current.
enumerate_to_list(Seq) ->
    E = get_enumerator(Seq),
    try
        enumerate_to_list_loop(E, [])
    after
        safe_dispose(E)
    end.

enumerate_to_list_loop(E, Acc) ->
    case move_next(E) of
        true ->
            enumerate_to_list_loop(E, [get_current(E) | Acc]);
        false ->
            lists:reverse(Acc)
    end.

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

%% Create a byte array of given size filled with a constant value.
%% For value 0, delegates to new_byte_array_zeroed (atomics are zero by default).
new_byte_array_filled(Len, 0) ->
    new_byte_array_zeroed(Len);
new_byte_array_filled(0, _Value) ->
    {byte_array, 0, atomics:new(1, [{signed, false}])};
new_byte_array_filled(Len, Value) ->
    Ref = atomics:new(Len, [{signed, false}]),
    fill_byte_array(Ref, 1, Len, Value),
    {byte_array, Len, Ref}.

fill_byte_array(_Ref, Idx, Len, _Value) when Idx > Len -> ok;
fill_byte_array(Ref, Idx, Len, Value) ->
    atomics:put(Ref, Idx, Value),
    fill_byte_array(Ref, Idx + 1, Len, Value).

populate_byte_array(_, _, []) ->
    ok;
populate_byte_array(Ref, Idx, [V | Rest]) ->
    atomics:put(Ref, Idx, V),
    populate_byte_array(Ref, Idx + 1, Rest).

byte_array_to_list({byte_array, 0, _}) -> [];
byte_array_to_list({byte_array, Size, Ref}) -> [atomics:get(Ref, I) || I <- lists:seq(1, Size)].

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
byte_array_length({byte_array, Size, _}) ->
    Size;
byte_array_length(PdRef) when is_reference(PdRef) ->
    byte_array_length(get(PdRef)).

%% Math.DivRem with out-ref: computes quotient and sets remainder via process dictionary.
div_rem(X, Y, RemRef) ->
    put(RemRef, X rem Y),
    X div Y.
