-module(fable_reflection).
-export([full_name/1, namespace/1, is_generic_type/1, is_array/1,
         get_element_type/1, get_generics/1, make_tuple_type/1]).

full_name(TypeInfo) ->
    maps:get(fullname, TypeInfo).

namespace(TypeInfo) ->
    FullName = maps:get(fullname, TypeInfo),
    case binary:match(FullName, <<".">>) of
        nomatch -> <<>>;
        _ ->
            Parts = binary:split(FullName, <<".">>, [global]),
            iolist_to_binary(lists:join(<<".">>, lists:droplast(Parts)))
    end.

is_generic_type(TypeInfo) ->
    maps:get(generics, TypeInfo) =/= [].

is_array(TypeInfo) ->
    FullName = maps:get(fullname, TypeInfo),
    case binary:match(FullName, <<"[]">>) of
        nomatch -> false;
        _ -> true
    end.

get_element_type(TypeInfo) ->
    case maps:get(generics, TypeInfo, []) of
        [Gen | _] -> Gen;
        _ -> undefined
    end.

get_generics(TypeInfo) ->
    maps:get(generics, TypeInfo, []).

%% Create a TypeInfo for a tuple from a list of element TypeInfos.
make_tuple_type(TypeInfos) when is_list(TypeInfos) ->
    N = length(TypeInfos),
    FullName = iolist_to_binary([<<"System.Tuple`">>, integer_to_binary(N)]),
    #{fullname => FullName, generics => TypeInfos}.
