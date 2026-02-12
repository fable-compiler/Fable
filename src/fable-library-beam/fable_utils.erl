-module(fable_utils).
-export([iface_get/2, apply_curried/2, new_ref/1, safe_dispose/1,
         get_enumerator/1, move_next/1, get_current/1,
         pos_infinity/0, neg_infinity/0, nan/0]).

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
        true -> (maps:get(dispose, Obj))(ok);
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
    %% ResizeArray: unwrap the process dict list, then make enumerator
    get_enumerator(get(Ref));
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

%% IEEE 754 special float values via binary construction.
%% Erlang has no literals for infinity/NaN, but the BEAM VM supports them internally.
pos_infinity() -> <<F/float>> = <<0:1, 2047:11, 0:52>>, F.
neg_infinity() -> <<F/float>> = <<1:1, 2047:11, 0:52>>, F.
nan() -> <<F/float>> = <<0:1, 2047:11, 1:52>>, F.
