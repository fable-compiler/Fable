"""F# Quotation runtime support for Fable Python target.

Provides dataclass-based representations of F# quotation AST nodes
and pattern matching helpers compatible with Fable's option convention
(None = no match, tuple = match).
"""

from __future__ import annotations

import operator
from dataclasses import dataclass
from typing import Any

from .array_ import Array
from .list import FSharpList, of_array


# ===================================================================
# Var: represents an F# quotation variable
# ===================================================================


@dataclass
class Var:
    name: str
    type: str
    is_mutable: bool


def mk_var(name: str, type: str, is_mutable: bool = False) -> Var:
    return Var(name, type, is_mutable)


def var_get_name(var: Var) -> str:
    return var.name


def var_get_type(var: Var) -> str:
    return var.type


def var_get_is_mutable(var: Var) -> bool:
    return var.is_mutable


# ===================================================================
# Expr nodes: tagged dataclasses for each quotation expression kind
# ===================================================================


@dataclass
class ExprValue:
    value: Any
    type: str


@dataclass
class ExprVarExpr:
    var: Var


@dataclass
class ExprLambda:
    var: Var
    body: Any  # Expr node


@dataclass
class ExprApplication:
    func: Any  # Expr node
    arg: Any  # Expr node


@dataclass
class ExprLet:
    var: Var
    value: Any  # Expr node
    body: Any  # Expr node


@dataclass
class ExprIfThenElse:
    guard: Any  # Expr node
    then_expr: Any  # Expr node
    else_expr: Any  # Expr node


@dataclass
class ExprCall:
    instance: Any
    method: str
    args: Array[Any]


@dataclass
class ExprSequential:
    first: Any  # Expr node
    second: Any  # Expr node


@dataclass
class ExprNewTuple:
    elements: Array[Any]


@dataclass
class ExprNewUnion:
    type_name: str
    tag: int
    fields: Array[Any]


@dataclass
class ExprNewRecord:
    field_names: Array[str]
    values: Array[Any]


@dataclass
class ExprNewList:
    head: Any  # Expr node
    tail: Any  # Expr node


@dataclass
class ExprTupleGet:
    expr: Any  # Expr node
    index: int


@dataclass
class ExprUnionTag:
    expr: Any  # Expr node


@dataclass
class ExprUnionField:
    expr: Any  # Expr node
    field_index: int


@dataclass
class ExprFieldGet:
    expr: Any  # Expr node
    field_name: str


@dataclass
class ExprFieldSet:
    expr: Any  # Expr node
    field_name: str
    value: Any  # Expr node


@dataclass
class ExprVarSet:
    target: Any  # Expr node
    value: Any  # Expr node


# Expr is the union of all expression node types
type Expr = (
    ExprValue
    | ExprVarExpr
    | ExprLambda
    | ExprApplication
    | ExprLet
    | ExprIfThenElse
    | ExprCall
    | ExprSequential
    | ExprNewTuple
    | ExprNewUnion
    | ExprNewRecord
    | ExprNewList
    | ExprTupleGet
    | ExprUnionTag
    | ExprUnionField
    | ExprFieldGet
    | ExprFieldSet
    | ExprVarSet
)


# ===================================================================
# Constructors
# ===================================================================


def mk_value(value: Any, type: str) -> ExprValue:
    return ExprValue(value, type)


def mk_var_expr(var: Var) -> ExprVarExpr:
    return ExprVarExpr(var)


def mk_lambda(var: Var, body: Expr) -> ExprLambda:
    return ExprLambda(var, body)


def mk_app(func: Expr, arg: Expr) -> ExprApplication:
    return ExprApplication(func, arg)


def mk_let(var: Var, value: Expr, body: Expr) -> ExprLet:
    return ExprLet(var, value, body)


def mk_if_then_else(guard: Expr, then_expr: Expr, else_expr: Expr) -> ExprIfThenElse:
    return ExprIfThenElse(guard, then_expr, else_expr)


def mk_call(instance: Any, method: str, args: Array[Any]) -> ExprCall:
    return ExprCall(instance, method, args)


def mk_sequential(first: Expr, second: Expr) -> ExprSequential:
    return ExprSequential(first, second)


def mk_new_tuple(elements: Array[Any]) -> ExprNewTuple:
    return ExprNewTuple(elements)


def mk_new_union(type_name: str, tag: int, fields: Array[Any]) -> ExprNewUnion:
    return ExprNewUnion(type_name, tag, fields)


def mk_new_record(field_names: Array[str], values: Array[Any]) -> ExprNewRecord:
    return ExprNewRecord(field_names, values)


def mk_new_list(head: Expr, tail: Expr) -> ExprNewList:
    return ExprNewList(head, tail)


def mk_tuple_get(expr: Expr, index: int) -> ExprTupleGet:
    return ExprTupleGet(expr, index)


def mk_union_tag(expr: Expr) -> ExprUnionTag:
    return ExprUnionTag(expr)


def mk_union_field(expr: Expr, field_index: int) -> ExprUnionField:
    return ExprUnionField(expr, field_index)


def mk_field_get(expr: Expr, field_name: str) -> ExprFieldGet:
    return ExprFieldGet(expr, field_name)


def mk_field_set(expr: Expr, field_name: str, value: Expr) -> ExprFieldSet:
    return ExprFieldSet(expr, field_name, value)


def mk_var_set(target: Expr, value: Expr) -> ExprVarSet:
    return ExprVarSet(target, value)


# ===================================================================
# Accessor
# ===================================================================


def get_type(expr: Expr) -> str:
    if isinstance(expr, ExprValue):
        return expr.type
    if isinstance(expr, ExprLambda):
        return expr.var.type
    return "obj"


# ===================================================================
# Pattern match helpers
# Returns None (no match) or a tuple (match), following Fable's
# option convention for active patterns.
# ===================================================================


def is_value(expr: Expr) -> tuple[Any, str] | None:
    if isinstance(expr, ExprValue):
        return (expr.value, expr.type)
    return None


def is_var(expr: Expr) -> Var | None:
    if isinstance(expr, ExprVarExpr):
        return expr.var
    return None


def is_lambda(expr: Expr) -> tuple[Var, Expr] | None:
    if isinstance(expr, ExprLambda):
        return (expr.var, expr.body)
    return None


def is_application(expr: Expr) -> tuple[Expr, Expr] | None:
    if isinstance(expr, ExprApplication):
        return (expr.func, expr.arg)
    return None


def is_let(expr: Expr) -> tuple[Var, Expr, Expr] | None:
    if isinstance(expr, ExprLet):
        return (expr.var, expr.value, expr.body)
    return None


def is_if_then_else(expr: Expr) -> tuple[Expr, Expr, Expr] | None:
    if isinstance(expr, ExprIfThenElse):
        return (expr.guard, expr.then_expr, expr.else_expr)
    return None


def is_call(expr: Expr) -> tuple[Any, str, Array[Any]] | None:
    if isinstance(expr, ExprCall):
        return (expr.instance, expr.method, expr.args)
    return None


def is_sequential(expr: Expr) -> tuple[Expr, Expr] | None:
    if isinstance(expr, ExprSequential):
        return (expr.first, expr.second)
    return None


def is_new_tuple(expr: Expr) -> FSharpList[Any] | None:
    if isinstance(expr, ExprNewTuple):
        return of_array(expr.elements)
    return None


def is_new_union(expr: Expr) -> tuple[str, int, Array[Any]] | None:
    if isinstance(expr, ExprNewUnion):
        return (expr.type_name, expr.tag, expr.fields)
    return None


def is_new_record(expr: Expr) -> tuple[Array[str], Array[Any]] | None:
    if isinstance(expr, ExprNewRecord):
        return (expr.field_names, expr.values)
    return None


def is_tuple_get(expr: Expr) -> tuple[Expr, int] | None:
    if isinstance(expr, ExprTupleGet):
        return (expr.expr, expr.index)
    return None


def is_field_get(expr: Expr) -> tuple[Expr, str] | None:
    if isinstance(expr, ExprFieldGet):
        return (expr.expr, expr.field_name)
    return None


# ===================================================================
# Evaluation
# ===================================================================

_OPERATORS: dict[str, Any] = {
    "op_Addition": operator.add,
    "op_Subtraction": operator.sub,
    "op_Multiply": operator.mul,
    "op_Division": operator.floordiv,
    "op_Modulus": operator.mod,
    "op_Exponentiation": operator.pow,
    "op_UnaryNegation": operator.neg,
    "op_UnaryPlus": operator.pos,
    "op_LogicalNot": operator.not_,
    "op_BitwiseOr": operator.or_,
    "op_BitwiseAnd": operator.and_,
    "op_ExclusiveOr": operator.xor,
    "op_LeftShift": operator.lshift,
    "op_RightShift": operator.rshift,
    "op_Equality": operator.eq,
    "op_Inequality": operator.ne,
    "op_LessThan": operator.lt,
    "op_LessThanOrEqual": operator.le,
    "op_GreaterThan": operator.gt,
    "op_GreaterThanOrEqual": operator.ge,
    "op_BooleanAnd": lambda a, b: a and b,
    "op_BooleanOr": lambda a, b: a or b,
}


def evaluate(expr: Expr, env: dict[str, Any] | None = None) -> Any:
    """Evaluate a quotation AST node and return the resulting value."""
    if env is None:
        env = {}

    match expr:
        case ExprValue(value=value):
            return value

        case ExprVarExpr(var=var):
            if var.name in env:
                return env[var.name]
            raise ValueError(f"Unbound variable: {var.name}")

        case ExprLambda(var=var, body=body):
            captured_env = env.copy()

            def closure(arg: Any) -> Any:
                new_env = captured_env.copy()
                new_env[var.name] = arg
                return evaluate(body, new_env)

            return closure

        case ExprApplication(func=func, arg=arg):
            return evaluate(func, env)(evaluate(arg, env))

        case ExprLet(var=var, value=value, body=body):
            new_env = env | {var.name: evaluate(value, env)}
            return evaluate(body, new_env)

        case ExprIfThenElse(guard=guard, then_expr=then_expr, else_expr=else_expr):
            if evaluate(guard, env):
                return evaluate(then_expr, env)
            return evaluate(else_expr, env)

        case ExprSequential(first=first, second=second):
            evaluate(first, env)
            return evaluate(second, env)

        case ExprNewTuple(elements=elements):
            return tuple(evaluate(e, env) for e in elements)

        case ExprCall(method=method, args=args):
            evaluated_args = [evaluate(a, env) for a in args]
            if method in _OPERATORS:
                return _OPERATORS[method](*evaluated_args)
            raise ValueError(f"Unknown method: {method}")

        case ExprTupleGet(expr=inner, index=index):
            return evaluate(inner, env)[index]

        case ExprNewUnion(tag=tag, fields=fields):
            return (tag, *[evaluate(f, env) for f in fields])

        case ExprNewRecord(field_names=names, values=values):
            return {n: evaluate(v, env) for n, v in zip(names, values)}

        case ExprNewList(head=head, tail=tail):
            return [evaluate(head, env), *evaluate(tail, env)]

        case ExprVarSet(target=target, value=value):
            match target:
                case ExprVarExpr(var=var):
                    env[var.name] = evaluate(value, env)
                    return None
                case _:
                    raise ValueError("VarSet target must be a variable")

        case ExprFieldGet(expr=inner, field_name=name):
            obj = evaluate(inner, env)
            if isinstance(obj, dict):
                return obj[name]
            return getattr(obj, name)

        case _:
            raise ValueError(f"Cannot evaluate expression: {type(expr).__name__}")
