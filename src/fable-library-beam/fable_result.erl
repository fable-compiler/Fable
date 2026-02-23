-module(fable_result).
-export([
    map/2,
    map_error/2,
    bind/2,
    is_ok/1,
    is_error/1,
    contains/2,
    count/1,
    default_value/2,
    default_with/2,
    exists/2,
    fold/3,
    fold_back/3,
    forall/2,
    iter/2,
    to_array/1,
    to_list/1,
    to_option/1
]).

-type result() :: {ok, term()} | {error, term()}.

-spec map(fun(), result()) -> result().
-spec map_error(fun(), result()) -> result().
-spec bind(fun(), result()) -> result().
-spec is_ok(result()) -> boolean().
-spec is_error(result()) -> boolean().
-spec contains(term(), result()) -> boolean().
-spec count(result()) -> 0 | 1.
-spec default_value(term(), result()) -> term().
-spec default_with(fun(), result()) -> term().
-spec exists(fun(), result()) -> boolean().
-spec fold(fun(), term(), result()) -> term().
-spec fold_back(fun(), result(), term()) -> term().
-spec forall(fun(), result()) -> boolean().
-spec iter(fun(), result()) -> ok.
-spec to_array(result()) -> list().
-spec to_list(result()) -> list().
-spec to_option(result()) -> term() | undefined.

%% Ok = {ok, Value}, Error = {error, ErrValue}

map(Fn, {ok, V}) -> {ok, Fn(V)};
map(_Fn, Err) -> Err.

map_error(_Fn, {ok, _V} = Ok) -> Ok;
map_error(Fn, {error, E}) -> {error, Fn(E)}.

bind(Fn, {ok, V}) -> Fn(V);
bind(_Fn, Err) -> Err.

is_ok({ok, _V}) -> true;
is_ok(_) -> false.

is_error({error, _E}) -> true;
is_error(_) -> false.

contains(Value, {ok, V}) -> V =:= Value;
contains(_Value, _) -> false.

count({ok, _V}) -> 1;
count(_) -> 0.

default_value(_DefVal, {ok, V}) -> V;
default_value(DefVal, _) -> DefVal.

default_with(_DefFn, {ok, V}) -> V;
default_with(DefFn, {error, E}) -> DefFn(E).

exists(Fn, {ok, V}) -> Fn(V);
exists(_Fn, _) -> false.

%% fold: folder state result -> state
%% Curried: fun s x -> ... => fun(S) -> fun(X) -> ... end end
fold(Fn, State, {ok, V}) -> (Fn(State))(V);
fold(_Fn, State, _) -> State.

%% foldBack: folder result state -> state
%% Curried: fun x s -> ... => fun(X) -> fun(S) -> ... end end
fold_back(Fn, {ok, V}, State) -> (Fn(V))(State);
fold_back(_Fn, _, State) -> State.

forall(Fn, {ok, V}) -> Fn(V);
forall(_Fn, _) -> true.

iter(Fn, {ok, V}) ->
    Fn(V),
    ok;
iter(_Fn, _) ->
    ok.

to_array({ok, V}) -> [V];
to_array(_) -> [].

to_list({ok, V}) -> [V];
to_list(_) -> [].

%% None = undefined, Some(x) = x
to_option({ok, V}) -> V;
to_option(_) -> undefined.
