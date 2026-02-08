-module(fable_convert).
-export([to_float/1]).

%% Robust string-to-float conversion that handles edge cases
%% Erlang's binary_to_float/1 is strict and rejects formats like "1." or "1"
%% .NET accepts these, so we normalize before converting.
to_float(Bin) when is_binary(Bin) ->
    case binary_to_list(Bin) of
        [] -> erlang:error(badarg);
        Str ->
            case string:to_float(Str) of
                {Float, []} -> Float;
                {_Float, _Rest} ->
                    %% Partial parse - try as integer
                    try_as_integer(Str);
                {error, no_float} ->
                    %% No float found - try as integer
                    try_as_integer(Str)
            end
    end;
to_float(N) when is_integer(N) -> float(N);
to_float(F) when is_float(F) -> F.

try_as_integer(Str) ->
    case string:to_integer(Str) of
        {Int, []} -> float(Int);
        {Int, "."} -> float(Int);  %% Handle trailing dot like "1."
        _ -> erlang:error(badarg)
    end.
