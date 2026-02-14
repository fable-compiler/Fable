-module(native_code).
-export([add5/1, add7/1, greet/1, multiply/2]).

add5(X) ->
    X + 5.

add7(X) ->
    X + 7.

greet(Name) ->
    iolist_to_binary([<<"Hello, ">>, Name, <<"!">>]).

multiply(X, Y) ->
    X * Y.
