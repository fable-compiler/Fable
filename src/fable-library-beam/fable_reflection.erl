-module(fable_reflection).
-export([full_name/1, namespace/1, name/1, is_generic_type/1, is_array/1,
         get_element_type/1, get_generics/1, make_tuple_type/1,
         get_generic_type_definition/1,
         get_record_elements/1, is_record/1, is_union/1,
         get_union_cases/1,
         is_tuple_type/1, get_tuple_elements/1,
         is_function_type/1, get_function_elements/1,
         get_record_fields_value/2, get_record_field_value/2,
         make_record_from_values/2,
         get_tuple_fields_value/1, get_tuple_field_value/2,
         get_union_case_fields/1,
         get_union_fields_value/2, make_union_value/2,
         get_value/2]).

-spec full_name(map()) -> binary().
-spec namespace(map()) -> binary().
-spec name(map()) -> binary().
-spec is_generic_type(map()) -> boolean().
-spec is_array(map()) -> boolean().
-spec get_element_type(map()) -> map() | undefined.
-spec get_generics(map()) -> list().
-spec get_generic_type_definition(map()) -> map().
-spec make_tuple_type(list()) -> map().
-spec get_record_elements(map()) -> list().
-spec is_record(term()) -> boolean().
-spec is_union(term()) -> boolean().
-spec get_union_cases(map()) -> list().
-spec is_tuple_type(map()) -> boolean().
-spec get_tuple_elements(map()) -> list().
-spec is_function_type(map()) -> boolean().
-spec get_function_elements(map()) -> {map(), map()}.
-spec get_record_fields_value(map(), map()) -> list().
-spec get_record_field_value(map(), map()) -> term().
-spec make_record_from_values(map(), list() | reference()) -> map().
-spec get_tuple_fields_value(tuple()) -> list().
-spec get_tuple_field_value(tuple(), non_neg_integer()) -> term().
-spec get_union_case_fields(map()) -> list().
-spec get_union_fields_value(term(), map()) -> {map(), list()}.
-spec make_union_value(map(), list() | reference()) -> term().
-spec get_value(map(), map()) -> term().

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

%% Type.Name — extract last part of FullName (after last dot)
name(TypeInfo) ->
    FullName = maps:get(fullname, TypeInfo),
    Parts = binary:split(FullName, <<".">>, [global]),
    lists:last(Parts).

is_generic_type(TypeInfo) ->
    maps:get(generics, TypeInfo) =/= [].

is_array(TypeInfo) ->
    FullName = maps:get(fullname, TypeInfo),
    case binary:match(FullName, <<"[]">>) of
        nomatch -> false;
        _ -> true
    end.

get_element_type(TypeInfo) ->
    case is_array(TypeInfo) of
        true ->
            case maps:get(generics, TypeInfo, []) of
                [Gen | _] -> Gen;
                _ -> undefined
            end;
        false -> undefined
    end.

get_generics(TypeInfo) ->
    maps:get(generics, TypeInfo, []).

%% Type.GetGenericTypeDefinition — replace each generic arg with System.Object
get_generic_type_definition(TypeInfo) ->
    Generics = maps:get(generics, TypeInfo, []),
    ObjType = #{fullname => <<"System.Object">>, generics => []},
    TypeInfo#{generics => [ObjType || _ <- Generics]}.

%% Create a TypeInfo for a tuple from a list of element TypeInfos.
make_tuple_type(TypeInfos) when is_list(TypeInfos) ->
    N = length(TypeInfos),
    FullName = iolist_to_binary([<<"System.Tuple`">>, integer_to_binary(N)]),
    #{fullname => FullName, generics => TypeInfos}.

%% FSharpType.GetRecordFields — returns list of PropertyInfo maps.
get_record_elements(#{fields := Fields}) -> Fields;
get_record_elements(TypeInfo) ->
    erlang:error({not_record_type, maps:get(fullname, TypeInfo, <<"unknown">>)}).

%% FSharpType.IsRecord
is_record(TypeInfo) when is_map(TypeInfo) ->
    maps:is_key(fields, TypeInfo);
is_record(_) -> false.

%% FSharpType.IsUnion
is_union(TypeInfo) when is_map(TypeInfo) ->
    maps:is_key(cases, TypeInfo);
is_union(_) -> false.

%% FSharpType.GetUnionCases — returns list of CaseInfo maps.
get_union_cases(#{cases := Cases}) -> Cases;
get_union_cases(TypeInfo) ->
    erlang:error({not_union_type, maps:get(fullname, TypeInfo, <<"unknown">>)}).

%% FSharpType.IsTuple
is_tuple_type(TypeInfo) ->
    FullName = maps:get(fullname, TypeInfo),
    (starts_with(FullName, <<"System.Tuple">>) orelse
     starts_with(FullName, <<"System.ValueTuple">>))
    andalso not is_array(TypeInfo).

%% FSharpType.GetTupleElements — returns list of element TypeInfos.
get_tuple_elements(TypeInfo) ->
    case is_tuple_type(TypeInfo) of
        true -> maps:get(generics, TypeInfo, []);
        false -> erlang:error({not_tuple_type, maps:get(fullname, TypeInfo)})
    end.

%% FSharpType.IsFunction
is_function_type(TypeInfo) ->
    maps:get(fullname, TypeInfo) =:= <<"Microsoft.FSharp.Core.FSharpFunc`2">>.

%% FSharpType.GetFunctionElements — returns {Domain, Range} tuple.
get_function_elements(TypeInfo) ->
    case is_function_type(TypeInfo) of
        true ->
            [Domain, Range | _] = maps:get(generics, TypeInfo),
            {Domain, Range};
        false ->
            erlang:error({not_function_type, maps:get(fullname, TypeInfo)})
    end.

%% --- FSharpValue functions ---

%% FSharpValue.GetRecordFields(record) — extract values in field order.
%% TypeInfo is injected by the compiler since Erlang maps don't preserve order.
get_record_fields_value(Record, TypeInfo) ->
    Fields = maps:get(fields, TypeInfo),
    [maps:get(binary_to_atom(maps:get(name, F)), Record) || F <- Fields].

%% FSharpValue.GetRecordField(record, propertyInfo) — get single field value.
get_record_field_value(Record, PropInfo) ->
    Name = maps:get(name, PropInfo),
    maps:get(binary_to_atom(Name), Record).

%% FSharpValue.MakeRecord(typeInfo, values) — create record from type info and values.
make_record_from_values(TypeInfo, Values) ->
    Fields = maps:get(fields, TypeInfo),
    FieldNames = [binary_to_atom(maps:get(name, F)) || F <- Fields],
    ValList = case is_reference(Values) of
        true -> erlang:get(Values);
        false -> Values
    end,
    maps:from_list(lists:zip(FieldNames, ValList)).

%% FSharpValue.GetTupleFields(tuple)
get_tuple_fields_value(Tuple) ->
    erlang:tuple_to_list(Tuple).

%% FSharpValue.GetTupleField(tuple, index)
get_tuple_field_value(Tuple, Index) ->
    erlang:element(Index + 1, Tuple).

%% UnionCaseInfo.GetFields(caseInfo) — returns the case's field list.
get_union_case_fields(CaseInfo) ->
    maps:get(fields, CaseInfo, []).

%% FSharpValue.GetUnionFields(value, typeInfo) — returns {CaseInfo, FieldValues}.
get_union_fields_value(Value, TypeInfo) ->
    Cases = maps:get(cases, TypeInfo),
    %% Determine the Erlang tag from the value
    %% Determine the integer tag from the value
    Tag = if
        erlang:is_integer(Value) -> Value;
        erlang:is_tuple(Value) -> erlang:element(1, Value);
        true -> erlang:error({not_union_value, Value})
    end,
    %% Find matching case by integer tag
    CaseInfo = find_case_by_tag(Tag, Cases),
    %% Extract field values
    Fields = if
        erlang:is_integer(Value) -> [];
        erlang:is_tuple(Value) ->
            Elems = erlang:tuple_to_list(Value),
            lists:nthtail(1, Elems);  % skip the tag
        true -> []
    end,
    {CaseInfo, Fields}.

%% FSharpValue.MakeUnion(caseInfo, values) — create union value from case info.
make_union_value(CaseInfo, Values) ->
    Tag = maps:get(tag, CaseInfo),
    ValList = case is_reference(Values) of
        true -> erlang:get(Values);
        false when is_list(Values) -> Values;
        false -> Values
    end,
    case ValList of
        [] -> Tag;
        _ -> erlang:list_to_tuple([Tag | ValList])
    end.

%% PropertyInfo.GetValue(propInfo, obj)
get_value(PropInfo, Obj) ->
    Name = maps:get(name, PropInfo),
    maps:get(binary_to_atom(Name), Obj).

%% --- Internal helpers ---

starts_with(Bin, Prefix) ->
    PLen = byte_size(Prefix),
    case Bin of
        <<Prefix:PLen/binary, _/binary>> -> true;
        _ -> false
    end.

find_case_by_tag(_Tag, []) ->
    erlang:error(case_not_found);
find_case_by_tag(Tag, [Case | Rest]) ->
    case maps:get(tag, Case) of
        Tag -> Case;
        _ -> find_case_by_tag(Tag, Rest)
    end.
