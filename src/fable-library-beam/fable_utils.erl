-module(fable_utils).
-export([iface_get/2, get_enumerator/1, move_next/1, get_current/1]).

%% Interface dispatch: works for both object expressions (maps) and class instances (refs).
iface_get(Name, Obj) when is_map(Obj) -> maps:get(Name, Obj);
iface_get(Name, Ref) -> maps:get(Name, get(Ref)).

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
