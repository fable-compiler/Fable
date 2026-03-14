-module(fable_quotation).
-export([
    mk_var/3, mk_var_expr/1, mk_value/2, mk_lambda/2,
    mk_app/2, mk_let/3, mk_if_then_else/3, mk_call/3,
    mk_sequential/2, mk_new_tuple/1,
    mk_new_union/3, mk_new_record/2, mk_new_list/2,
    mk_tuple_get/2, mk_union_tag/1, mk_union_field/2,
    mk_field_get/2, mk_field_set/3, mk_var_set/2,
    var_get_name/1, var_get_type/1, var_get_is_mutable/1,
    get_type/1,
    is_value/1, is_var/1, is_lambda/1, is_application/1,
    is_let/1, is_if_then_else/1, is_call/1, is_sequential/1,
    is_new_tuple/1, is_new_union/1, is_new_record/1,
    is_tuple_get/1, is_field_get/1,
    evaluate/1
]).

%% ===================================================================
%% Var constructor: {var, Name, Type, IsMutable}
%% ===================================================================

mk_var(Name, Type, IsMutable) -> {var, Name, Type, IsMutable}.

%% Var accessors
var_get_name({var, Name, _, _}) -> Name.
var_get_type({var, _, Type, _}) -> Type.
var_get_is_mutable({var, _, _, IsMutable}) -> IsMutable.

%% ===================================================================
%% Expr node constructors: {expr, Tag, ...fields}
%% ===================================================================

mk_var_expr(Var) -> {expr, var_expr, Var}.
mk_value(Value, Type) -> {expr, value, Value, Type}.
mk_lambda(Var, Body) -> {expr, lambda, Var, Body}.
mk_app(Func, Arg) -> {expr, application, Func, Arg}.
mk_let(Var, Value, Body) -> {expr, 'let', Var, Value, Body}.
mk_if_then_else(Guard, Then, Else) -> {expr, if_then_else, Guard, Then, Else}.
mk_call(Instance, Method, Args) -> {expr, call, Instance, Method, Args}.
mk_sequential(First, Second) -> {expr, sequential, First, Second}.
mk_new_tuple(Elements) -> {expr, new_tuple, Elements}.
mk_new_union(TypeName, Tag, Fields) -> {expr, new_union, TypeName, Tag, Fields}.
mk_new_record(FieldNames, Values) -> {expr, new_record, FieldNames, Values}.
mk_new_list(Head, Tail) -> {expr, new_list, Head, Tail}.
mk_tuple_get(Expr, Index) -> {expr, tuple_get, Expr, Index}.
mk_union_tag(Expr) -> {expr, union_tag, Expr}.
mk_union_field(Expr, FieldIndex) -> {expr, union_field, Expr, FieldIndex}.
mk_field_get(Expr, FieldName) -> {expr, field_get, Expr, FieldName}.
mk_field_set(Expr, FieldName, Value) -> {expr, field_set, Expr, FieldName, Value}.
mk_var_set(Target, Value) -> {expr, var_set, Target, Value}.

%% ===================================================================
%% Expr accessor
%% ===================================================================

get_type({expr, value, _, T}) -> T;
get_type({expr, lambda, {var, _, T, _}, _}) -> T;
get_type(_) -> <<"obj">>.

%% ===================================================================
%% Pattern match helpers (for Quotations.Patterns active patterns)
%% Returns {some, Result} | undefined (matching Fable's option convention)
%% ===================================================================

%% Value pattern: returns {Value, Type}
is_value({expr, value, V, T}) -> {V, T};
is_value(_) -> undefined.

%% Var pattern: returns the Var
is_var({expr, var_expr, Var}) -> Var;
is_var(_) -> undefined.

%% Lambda pattern: returns {Var, Body}
is_lambda({expr, lambda, V, B}) -> {V, B};
is_lambda(_) -> undefined.

%% Application pattern: returns {Func, Arg}
is_application({expr, application, F, A}) -> {F, A};
is_application(_) -> undefined.

%% Let pattern: returns {Var, Value, Body}
is_let({expr, 'let', V, Val, B}) -> {V, Val, B};
is_let(_) -> undefined.

%% IfThenElse pattern: returns {Guard, Then, Else}
is_if_then_else({expr, if_then_else, G, T, E}) -> {G, T, E};
is_if_then_else(_) -> undefined.

%% Call pattern: returns {Instance, Method, Args} (dereference Args)
is_call({expr, call, I, M, A}) -> {I, M, deref(A)};
is_call(_) -> undefined.

%% Sequential pattern: returns {First, Second}
is_sequential({expr, sequential, F, S}) -> {F, S};
is_sequential(_) -> undefined.

%% NewTuple pattern: returns Elements list (dereference if stored as Ref)
is_new_tuple({expr, new_tuple, E}) -> deref(E);
is_new_tuple(_) -> undefined.

%% NewUnionCase pattern: returns {TypeName, Tag, Fields} (dereference Fields)
is_new_union({expr, new_union, N, T, F}) -> {N, T, deref(F)};
is_new_union(_) -> undefined.

%% NewRecord pattern: returns {FieldNames, Values} (dereference both)
is_new_record({expr, new_record, N, V}) -> {deref(N), deref(V)};
is_new_record(_) -> undefined.

%% TupleGet pattern: returns {Expr, Index}
is_tuple_get({expr, tuple_get, E, I}) -> {E, I};
is_tuple_get(_) -> undefined.

%% FieldGet/PropertyGet pattern: returns {Expr, FieldName}
is_field_get({expr, field_get, E, N}) -> {E, N};
is_field_get(_) -> undefined.

%% ===================================================================
%% Evaluation
%% ===================================================================

evaluate(Expr) -> evaluate(Expr, #{}).

%% Dereference a Fable array (stored as a Ref in the process dictionary) to a plain list.
deref(Ref) when is_reference(Ref) -> get(Ref);
deref(List) when is_list(List) -> List.

evaluate({expr, value, V, _T}, _Env) -> V;
evaluate({expr, var_expr, {var, Name, _, _}}, Env) ->
    maps:get(Name, Env);
evaluate({expr, lambda, {var, Name, _, _}, Body}, Env) ->
    CapturedEnv = Env,
    fun(Arg) -> evaluate(Body, CapturedEnv#{Name => Arg}) end;
evaluate({expr, application, Func, Arg}, Env) ->
    F = evaluate(Func, Env),
    A = evaluate(Arg, Env),
    F(A);
evaluate({expr, 'let', {var, Name, _, _}, Value, Body}, Env) ->
    V = evaluate(Value, Env),
    evaluate(Body, Env#{Name => V});
evaluate({expr, if_then_else, Guard, Then, Else}, Env) ->
    case evaluate(Guard, Env) of
        true -> evaluate(Then, Env);
        _ -> evaluate(Else, Env)
    end;
evaluate({expr, sequential, First, Second}, Env) ->
    evaluate(First, Env),
    evaluate(Second, Env);
evaluate({expr, new_tuple, Elements}, Env) ->
    list_to_tuple([evaluate(E, Env) || E <- deref(Elements)]);
evaluate({expr, tuple_get, Inner, Index}, Env) ->
    element(Index + 1, evaluate(Inner, Env));
evaluate({expr, call, _Instance, Method, Args}, Env) ->
    EvaluatedArgs = [evaluate(A, Env) || A <- deref(Args)],
    apply_operator(Method, EvaluatedArgs).

apply_operator(<<"op_Addition">>, [A, B]) -> A + B;
apply_operator(<<"op_Subtraction">>, [A, B]) -> A - B;
apply_operator(<<"op_Multiply">>, [A, B]) -> A * B;
apply_operator(<<"op_Division">>, [A, B]) -> A div B;
apply_operator(<<"op_Modulus">>, [A, B]) -> A rem B;
apply_operator(<<"op_UnaryNegation">>, [A]) -> -A;
apply_operator(<<"op_Equality">>, [A, B]) -> A =:= B;
apply_operator(<<"op_Inequality">>, [A, B]) -> A =/= B;
apply_operator(<<"op_LessThan">>, [A, B]) -> A < B;
apply_operator(<<"op_LessThanOrEqual">>, [A, B]) -> A =< B;
apply_operator(<<"op_GreaterThan">>, [A, B]) -> A > B;
apply_operator(<<"op_GreaterThanOrEqual">>, [A, B]) -> A >= B;
apply_operator(<<"op_BooleanAnd">>, [A, B]) -> A andalso B;
apply_operator(<<"op_BooleanOr">>, [A, B]) -> A orelse B;
apply_operator(<<"op_LogicalNot">>, [A]) -> not A;
apply_operator(<<"op_BitwiseOr">>, [A, B]) -> A bor B;
apply_operator(<<"op_BitwiseAnd">>, [A, B]) -> A band B;
apply_operator(<<"op_ExclusiveOr">>, [A, B]) -> A bxor B;
apply_operator(<<"op_LeftShift">>, [A, B]) -> A bsl B;
apply_operator(<<"op_RightShift">>, [A, B]) -> A bsr B.
