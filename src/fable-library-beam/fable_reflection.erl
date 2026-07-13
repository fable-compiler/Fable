-module(fable_reflection).
-export([
    full_name/1,
    namespace/1,
    name/1,
    is_generic_type/1,
    is_array/1,
    get_element_type/1,
    get_generics/1,
    make_tuple_type/1,
    get_generic_type_definition/1,
    get_record_elements/1,
    is_record/1,
    is_union/1,
    get_union_cases/1,
    is_tuple_type/1,
    get_tuple_elements/1,
    is_function_type/1,
    get_function_elements/1,
    get_record_fields_value/2,
    get_record_field_value/2,
    make_record_from_values/2,
    get_tuple_fields_value/1,
    get_tuple_field_value/2,
    get_union_case_fields/1,
    get_union_fields_value/2,
    make_union_value/2,
    get_value/2,
    is_enum/1,
    get_enum_underlying_type/1,
    get_enum_values/1,
    get_enum_names/1,
    get_enum_name/2,
    is_enum_defined/2,
    parse_enum/2,
    try_parse_enum/3,
    make_generic_type/2,
    create_instance/2
]).

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
-spec is_enum(term()) -> boolean().
-spec get_enum_underlying_type(map()) -> map().
-spec get_enum_values(map()) -> list().
-spec get_enum_names(map()) -> list().
-spec get_enum_name(map(), integer()) -> binary() | undefined.
-spec is_enum_defined(map(), integer() | binary()) -> boolean().
-spec parse_enum(map(), binary()) -> integer().
-spec try_parse_enum(map(), binary(), reference()) -> boolean().
-spec make_generic_type(map(), list() | reference()) -> map().
-spec create_instance(map(), list() | reference()) -> term().

full_name(TypeInfo) ->
    maps:get(fullname, TypeInfo).

namespace(TypeInfo) ->
    FullName = maps:get(fullname, TypeInfo),
    case binary:match(FullName, <<".">>) of
        nomatch ->
            <<>>;
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
        false ->
            undefined
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
get_record_elements(#{fields := Fields}) ->
    force(Fields);
get_record_elements(TypeInfo) ->
    erlang:error({not_record_type, maps:get(fullname, TypeInfo, <<"unknown">>)}).

%% FSharpType.IsRecord
is_record(TypeInfo) when is_map(TypeInfo) ->
    maps:is_key(fields, TypeInfo);
is_record(_) ->
    false.

%% FSharpType.IsUnion
is_union(TypeInfo) when is_map(TypeInfo) ->
    maps:is_key(cases, TypeInfo);
is_union(_) ->
    false.

%% FSharpType.GetUnionCases — returns list of CaseInfo maps.
get_union_cases(#{cases := Cases}) ->
    force(Cases);
get_union_cases(TypeInfo) ->
    erlang:error({not_union_type, maps:get(fullname, TypeInfo, <<"unknown">>)}).

%% FSharpType.IsTuple
is_tuple_type(TypeInfo) ->
    FullName = maps:get(fullname, TypeInfo),
    (starts_with(FullName, <<"System.Tuple">>) orelse
        starts_with(FullName, <<"System.ValueTuple">>)) andalso
        not is_array(TypeInfo).

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
%% Fields are keyed in the record map by `erl_name`, the sanitized name; `name` holds the
%% F# name that PropertyInfo.Name reports and is not a valid key.
get_record_fields_value(Record, TypeInfo) ->
    Fields = force(maps:get(fields, TypeInfo)),
    [maps:get(maps:get(erl_name, F), Record) || F <- Fields].

%% FSharpValue.GetRecordField(record, propertyInfo) — get single field value.
get_record_field_value(Record, PropInfo) ->
    maps:get(maps:get(erl_name, PropInfo), Record).

%% FSharpValue.MakeRecord(typeInfo, values) — create record from type info and values.
make_record_from_values(TypeInfo, Values) ->
    Fields = force(maps:get(fields, TypeInfo)),
    FieldNames = [maps:get(erl_name, F) || F <- Fields],
    ValList =
        case is_reference(Values) of
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
    force(maps:get(fields, CaseInfo, [])).

%% FSharpValue.GetUnionFields(value, typeInfo) — returns {CaseInfo, FieldValues}.
%% FieldValues is typed `obj[]` by the compiler, so it must be an array ref. The result is a
%% tuple, so the compiler cannot wrap it at the call site the way it does for GetRecordFields.
get_union_fields_value(Value, TypeInfo) ->
    Cases = force(maps:get(cases, TypeInfo)),
    %% Determine the atom tag from the value
    Tag =
        if
            % bare atom (fieldless case)
            erlang:is_atom(Value) -> Value;
            % atom tag in first position
            erlang:is_tuple(Value) -> erlang:element(1, Value);
            true -> erlang:error({not_union_value, Value})
        end,
    %% Find matching case by atom tag (erl_tag field)
    CaseInfo = find_case_by_tag(Tag, Cases),
    %% Extract field values
    Fields =
        if
            erlang:is_atom(Value) ->
                [];
            erlang:is_tuple(Value) ->
                Elems = erlang:tuple_to_list(Value),
                % skip the tag
                lists:nthtail(1, Elems);
            true ->
                []
        end,
    {CaseInfo, fable_utils:new_ref(Fields)}.

%% FSharpValue.MakeUnion(caseInfo, values) — create union value from case info.
make_union_value(CaseInfo, Values) ->
    Tag = maps:get(erl_tag, CaseInfo),
    ValList =
        case is_reference(Values) of
            true -> erlang:get(Values);
            false when is_list(Values) -> Values;
            false -> Values
        end,
    case ValList of
        % bare atom for fieldless cases
        [] -> Tag;
        _ -> erlang:list_to_tuple([Tag | ValList])
    end.

%% PropertyInfo.GetValue(propInfo, obj)
get_value(PropInfo, Obj) ->
    maps:get(maps:get(erl_name, PropInfo), Obj).

%% --- Enums ---
%% An enum type info carries `enum_cases => [{Name, Value}]` and its underlying
%% numeric type as the single entry in `generics`.

%% Type.IsEnum — keyed on the presence of `enum_cases`, not on it being non-empty,
%% since an enum with no cases is still an enum.
is_enum(TypeInfo) when is_map(TypeInfo) ->
    maps:is_key(enum_cases, TypeInfo);
is_enum(_) ->
    false.

%% Type.GetEnumUnderlyingType / Enum.GetUnderlyingType
get_enum_underlying_type(TypeInfo) ->
    case maps:get(generics, TypeInfo, []) of
        [Underlying | _] -> Underlying;
        [] -> erlang:error({not_enum_type, maps:get(fullname, TypeInfo, <<"unknown">>)})
    end.

%% Enum.GetValues
get_enum_values(TypeInfo) ->
    [Value || {_Name, Value} <- enum_cases(TypeInfo)].

%% Enum.GetNames
get_enum_names(TypeInfo) ->
    [Name || {Name, _Value} <- enum_cases(TypeInfo)].

%% Enum.GetName — .NET returns undefined when no case matches.
get_enum_name(TypeInfo, Value) ->
    case lists:keyfind(Value, 2, enum_cases(TypeInfo)) of
        {Name, _} -> Name;
        false -> undefined
    end.

%% Enum.IsDefined — accepts either a case name or a value.
is_enum_defined(TypeInfo, NameOrValue) ->
    Cases = enum_cases(TypeInfo),

    case is_binary(NameOrValue) of
        true -> lists:keyfind(NameOrValue, 1, Cases) =/= false;
        false -> lists:keyfind(NameOrValue, 2, Cases) =/= false
    end.

%% Enum.Parse
parse_enum(TypeInfo, Name) ->
    case lists:keyfind(Name, 1, enum_cases(TypeInfo)) of
        {_, Value} ->
            Value;
        false ->
            erlang:error(
                {enum_case_not_found, maps:get(fullname, TypeInfo, <<"unknown">>), Name}
            )
    end.

%% Enum.TryParse — F# out-parameter pattern: sets the out-ref and returns a boolean.
try_parse_enum(TypeInfo, Name, OutRef) ->
    case lists:keyfind(Name, 1, enum_cases(TypeInfo)) of
        {_, Value} ->
            erlang:put(OutRef, Value),
            true;
        false ->
            erlang:put(OutRef, 0),
            false
    end.

%% --- Type construction ---

%% Type.MakeGenericType — substitute the generic arguments, keeping the rest of the type
%% info (including the lazy fields/cases) intact. Mirrors makeGenericType in fable-library-ts.
make_generic_type(TypeInfo, Generics) ->
    maps:put(generics, deref(Generics), TypeInfo).

%% Activator.CreateInstance. Beam has no runtime constructors attached to type infos, so this
%% covers records (built from their field values) and the primitives that have a zero value.
create_instance(TypeInfo, Args) ->
    case is_record(TypeInfo) of
        true ->
            make_record_from_values(TypeInfo, deref(Args));
        false ->
            case maps:get(fullname, TypeInfo, <<>>) of
                <<"System.String">> -> <<>>;
                <<"System.Boolean">> -> false;
                <<"System.Char">> -> 0;
                <<"System.Double">> -> 0.0;
                <<"System.Single">> -> 0.0;
                <<"System.SByte">> -> 0;
                <<"System.Byte">> -> 0;
                <<"System.Int16">> -> 0;
                <<"System.UInt16">> -> 0;
                <<"System.Int32">> -> 0;
                <<"System.UInt32">> -> 0;
                <<"System.Int64">> -> 0;
                <<"System.UInt64">> -> 0;
                <<"System.Decimal">> -> 0;
                FullName -> erlang:error({cannot_create_instance, FullName})
            end
    end.

%% --- Internal helpers ---

%% Fable passes arrays as refs into the process dictionary.
deref(Values) when is_reference(Values) ->
    erlang:get(Values);
deref(Values) when is_list(Values) ->
    Values.

enum_cases(TypeInfo) ->
    case maps:find(enum_cases, TypeInfo) of
        {ok, Cases} -> Cases;
        error -> erlang:error({not_enum_type, maps:get(fullname, TypeInfo, <<"unknown">>)})
    end.

%% Record fields and union cases are emitted as zero-arity funs so that a recursive type
%% (whose fields mention the type itself) terminates: forcing a thunk yields a type info
%% map whose own fields/cases are still unforced. Plain lists are accepted too, for the
%% type infos that are still inlined (types with no generated Erlang module).
force(Thunk) when is_function(Thunk, 0) ->
    Thunk();
force(List) when is_list(List) ->
    List.

starts_with(Bin, Prefix) ->
    PLen = byte_size(Prefix),
    case Bin of
        <<Prefix:PLen/binary, _/binary>> -> true;
        _ -> false
    end.

find_case_by_tag(_Tag, []) ->
    erlang:error(case_not_found);
find_case_by_tag(Tag, [Case | Rest]) ->
    case maps:get(erl_tag, Case) of
        Tag -> Case;
        _ -> find_case_by_tag(Tag, Rest)
    end.
