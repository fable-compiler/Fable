-module(fable_environment).
-export([
    get_environment_variable/1,
    get_current_directory/0
]).

-spec get_environment_variable(binary()) -> binary() | undefined.
-spec get_current_directory() -> binary().

get_environment_variable(Name) ->
    case os:getenv(unicode:characters_to_list(Name)) of
        false -> undefined;
        Value -> unicode:characters_to_binary(Value)
    end.

get_current_directory() ->
    {ok, Dir} = file:get_cwd(),
    unicode:characters_to_binary(Dir).
