-module(fable_utils).
-export([iface_get/2]).

%% Interface dispatch: works for both object expressions (maps) and class instances (refs).
iface_get(Name, Obj) when is_map(Obj) -> maps:get(Name, Obj);
iface_get(Name, Ref) -> maps:get(Name, get(Ref)).
