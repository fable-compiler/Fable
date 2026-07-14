// F# Quotation runtime support for the Fable Dart target.
//
// Mirrors fable-library-py/quotation.py and fable-library-ts/quotation.ts:
// the QuotationEmitter lowers a quoted body into calls to the mk* constructors
// below (the LibCall "quotation" module maps to this quotation.dart file).
// Dart is dynamically typed, so nodes are tagged classes and options follow
// Fable's convention (null = None, value/tuple = Some).
import 'List.dart' as list;
import 'Types.dart' as types;

// ===================================================================
// Var: represents an F# quotation variable
// ===================================================================

class Var {
  final String name;
  final String type;
  final bool isMutable;
  const Var(this.name, this.type, this.isMutable);
}

Var mkQuotVar(String name, String type, [bool isMutable = false]) =>
    Var(name, type, isMutable);

String varGetName(Var v) => v.name;
String varGetType(Var v) => v.type;
bool varGetIsMutable(Var v) => v.isMutable;

// ===================================================================
// Expr nodes: tagged classes for each quotation expression kind
// ===================================================================

abstract class Expr {
  final String tag;
  const Expr(this.tag);
}

class ExprValue extends Expr {
  final dynamic value;
  final String type;
  const ExprValue(this.value, this.type) : super('Value');
}

class ExprVarExpr extends Expr {
  final Var variable;
  const ExprVarExpr(this.variable) : super('Var');
}

class ExprLambda extends Expr {
  final Var variable;
  final Expr body;
  const ExprLambda(this.variable, this.body) : super('Lambda');
}

class ExprApplication extends Expr {
  final Expr func;
  final Expr arg;
  const ExprApplication(this.func, this.arg) : super('Application');
}

class ExprLet extends Expr {
  final Var variable;
  final Expr value;
  final Expr body;
  const ExprLet(this.variable, this.value, this.body) : super('Let');
}

class ExprIfThenElse extends Expr {
  final Expr guard;
  final Expr thenExpr;
  final Expr elseExpr;
  const ExprIfThenElse(this.guard, this.thenExpr, this.elseExpr)
      : super('IfThenElse');
}

class ExprCall extends Expr {
  final Expr? instance;
  final String method;
  final List<dynamic> args;
  final String declaringType;
  const ExprCall(this.instance, this.method, this.args,
      [this.declaringType = ''])
      : super('Call');
}

class ExprSequential extends Expr {
  final Expr first;
  final Expr second;
  const ExprSequential(this.first, this.second) : super('Sequential');
}

class ExprNewTuple extends Expr {
  final List<dynamic> elements;
  const ExprNewTuple(this.elements) : super('NewTuple');
}

class ExprNewUnion extends Expr {
  final String typeName;
  final int unionTag;
  final List<dynamic> fields;
  const ExprNewUnion(this.typeName, this.unionTag, this.fields)
      : super('NewUnion');
}

class ExprNewRecord extends Expr {
  final List<dynamic> fieldNames;
  final List<dynamic> values;
  const ExprNewRecord(this.fieldNames, this.values) : super('NewRecord');
}

class ExprNewList extends Expr {
  final Expr head;
  final Expr tail;
  const ExprNewList(this.head, this.tail) : super('NewList');
}

class ExprTupleGet extends Expr {
  final Expr expr;
  final int index;
  const ExprTupleGet(this.expr, this.index) : super('TupleGet');
}

class ExprUnionTag extends Expr {
  final Expr expr;
  const ExprUnionTag(this.expr) : super('UnionTag');
}

class ExprUnionField extends Expr {
  final Expr expr;
  final int fieldIndex;
  const ExprUnionField(this.expr, this.fieldIndex) : super('UnionField');
}

class ExprFieldGet extends Expr {
  final Expr expr;
  final String fieldName;
  const ExprFieldGet(this.expr, this.fieldName) : super('FieldGet');
}

class ExprFieldSet extends Expr {
  final Expr expr;
  final String fieldName;
  final Expr value;
  const ExprFieldSet(this.expr, this.fieldName, this.value) : super('FieldSet');
}

class ExprVarSet extends Expr {
  final Expr target;
  final Expr value;
  const ExprVarSet(this.target, this.value) : super('VarSet');
}

// ===================================================================
// Constructors (called by QuotationEmitter.fs)
// ===================================================================

ExprValue mkValue(dynamic value, String type) => ExprValue(value, type);

// A null-value node (no instance / null literal / empty option or list).
ExprValue mkNull(String type) => ExprValue(null, type);

ExprVarExpr mkVar(Var v) => ExprVarExpr(v);
ExprLambda mkLambda(Var v, Expr body) => ExprLambda(v, body);
ExprApplication mkApplication(Expr func, Expr arg) => ExprApplication(func, arg);
ExprLet mkLet(Var v, Expr value, Expr body) => ExprLet(v, value, body);

ExprIfThenElse mkIfThenElse(Expr guard, Expr thenExpr, Expr elseExpr) =>
    ExprIfThenElse(guard, thenExpr, elseExpr);

ExprCall mkCall(Expr? instance, String method, List<dynamic> args,
        [String declaringType = '']) =>
    ExprCall(instance, method, args, declaringType);

ExprSequential mkSequential(Expr first, Expr second) =>
    ExprSequential(first, second);

ExprNewTuple mkNewTuple(List<dynamic> elements) => ExprNewTuple(elements);

ExprNewUnion mkNewUnion(String typeName, int tag, List<dynamic> fields) =>
    ExprNewUnion(typeName, tag, fields);

ExprNewRecord mkNewRecord(List<dynamic> fieldNames, List<dynamic> values) =>
    ExprNewRecord(fieldNames, values);

ExprNewList mkNewList(Expr head, Expr tail) => ExprNewList(head, tail);
ExprTupleGet mkTupleGet(Expr expr, int index) => ExprTupleGet(expr, index);
ExprUnionTag mkUnionTag(Expr expr) => ExprUnionTag(expr);

ExprUnionField mkUnionField(Expr expr, int fieldIndex) =>
    ExprUnionField(expr, fieldIndex);

ExprFieldGet mkFieldGet(Expr expr, String fieldName) =>
    ExprFieldGet(expr, fieldName);

ExprFieldSet mkFieldSet(Expr expr, String fieldName, Expr value) =>
    ExprFieldSet(expr, fieldName, value);

ExprVarSet mkVarSet(Expr target, Expr value) => ExprVarSet(target, value);

// Correctly-typed empty argument array (used by the emitter for empty arg lists).
List<dynamic> emptyExprArray() => <dynamic>[];

// ===================================================================
// Accessor
// ===================================================================

String getType(Expr expr) {
  if (expr is ExprValue) return expr.type;
  if (expr is ExprLambda) return expr.variable.type;
  return 'obj';
}

// ===================================================================
// Pattern match helpers
// Returns null (no match) or a Some-wrapped value/tuple (match).
//
// Fable's Dart backend wraps `'a option` as a reified `Some<'a>?` (None = null)
// and unwraps it with `Types.value`. The F# quotation types (FSharpExpr /
// FSharpVar) are mapped to `dynamic` by the Dart backend, so the wrappers here
// are built with `dynamic` type arguments to match the generated annotations
// (e.g. `Some<Tuple2<dynamic, dynamic>>?`). `Value` keeps `Type` for its second
// element because F#'s `(|Value|_|)` returns `(obj * System.Type)`.
// ===================================================================

types.Some<types.Tuple2<dynamic, Type>>? isValue(Expr expr) => expr is ExprValue
    ? types.Some<types.Tuple2<dynamic, Type>>(
        types.Tuple2<dynamic, Type>(expr.value, expr.value.runtimeType))
    : null;

types.Some<dynamic>? isVar(Expr expr) =>
    expr is ExprVarExpr ? types.Some<dynamic>(expr.variable) : null;

types.Some<types.Tuple2<dynamic, dynamic>>? isLambda(Expr expr) =>
    expr is ExprLambda
        ? types.Some<types.Tuple2<dynamic, dynamic>>(
            types.Tuple2<dynamic, dynamic>(expr.variable, expr.body))
        : null;

types.Some<types.Tuple2<dynamic, dynamic>>? isApplication(Expr expr) =>
    expr is ExprApplication
        ? types.Some<types.Tuple2<dynamic, dynamic>>(
            types.Tuple2<dynamic, dynamic>(expr.func, expr.arg))
        : null;

types.Some<types.Tuple3<dynamic, dynamic, dynamic>>? isLet(Expr expr) =>
    expr is ExprLet
        ? types.Some<types.Tuple3<dynamic, dynamic, dynamic>>(
            types.Tuple3<dynamic, dynamic, dynamic>(
                expr.variable, expr.value, expr.body))
        : null;

types.Some<types.Tuple3<dynamic, dynamic, dynamic>>? isIfThenElse(Expr expr) =>
    expr is ExprIfThenElse
        ? types.Some<types.Tuple3<dynamic, dynamic, dynamic>>(
            types.Tuple3<dynamic, dynamic, dynamic>(
                expr.guard, expr.thenExpr, expr.elseExpr))
        : null;

types.Some<types.Tuple3<dynamic, dynamic, dynamic>>? isCall(Expr expr) {
  if (expr is ExprCall) {
    // A static/operator call carries the "novalue" no-instance sentinel node as
    // its instance (see QuotationEmitter). "novalue" is distinct from a genuine
    // quoted null value ("null"). Expose it as null so Patterns.Call matches F#.
    final inst = expr.instance;
    final normalized =
        (inst is ExprValue && inst.type == 'novalue') ? null : inst;
    return types.Some<types.Tuple3<dynamic, dynamic, dynamic>>(
        types.Tuple3<dynamic, dynamic, dynamic>(
            normalized, expr.method, list.ofArray<dynamic>(expr.args)));
  }
  return null;
}

String callDeclaringType(Expr expr) =>
    expr is ExprCall ? expr.declaringType : '';

types.Some<types.Tuple2<dynamic, dynamic>>? isSequential(Expr expr) =>
    expr is ExprSequential
        ? types.Some<types.Tuple2<dynamic, dynamic>>(
            types.Tuple2<dynamic, dynamic>(expr.first, expr.second))
        : null;

types.Some<list.FSharpList<dynamic>>? isNewTuple(Expr expr) =>
    expr is ExprNewTuple
        ? types.Some<list.FSharpList<dynamic>>(
            list.ofArray<dynamic>(expr.elements))
        : null;

types.Some<types.Tuple2<dynamic, list.FSharpList<dynamic>>>? isNewUnionCase(
        Expr expr) =>
    expr is ExprNewUnion
        ? types.Some<types.Tuple2<dynamic, list.FSharpList<dynamic>>>(
            types.Tuple2<dynamic, list.FSharpList<dynamic>>(
                expr.typeName, list.ofArray<dynamic>(expr.fields)))
        : null;

types.Some<types.Tuple2<dynamic, dynamic>>? isNewRecord(Expr expr) =>
    expr is ExprNewRecord
        ? types.Some<types.Tuple2<dynamic, dynamic>>(
            types.Tuple2<dynamic, dynamic>(list.ofArray<dynamic>(expr.fieldNames),
                list.ofArray<dynamic>(expr.values)))
        : null;

types.Some<types.Tuple2<dynamic, dynamic>>? isTupleGet(Expr expr) =>
    expr is ExprTupleGet
        ? types.Some<types.Tuple2<dynamic, dynamic>>(
            types.Tuple2<dynamic, dynamic>(expr.expr, expr.index))
        : null;

// Patterns.PropertyGet is `PropertyGet of Expr option * PropertyInfo * Expr list`,
// so the three slots must line up with Fable's Dart encoding of those F# types:
// `Expr option` -> `Some<dynamic>?` (null = None), `PropertyInfo` -> dynamic (the
// runtime exposes it as a plain field-name string) and `Expr list` -> FSharpList.
// Dart generics are invariant, so this return type has to be spelled out exactly.
types.Some<
        types.Tuple3<types.Some<dynamic>?, dynamic, list.FSharpList<dynamic>>>?
    isFieldGet(Expr expr) {
  if (expr is ExprFieldGet) {
    // A static property/field get carries the "novalue" no-instance sentinel
    // node as its target (see QuotationEmitter). "novalue" is distinct from a
    // genuine quoted null value ("null"). Expose it as None so
    // Patterns.PropertyGet/FieldGet match F#.
    final target = expr.expr;
    final normalized = (target is ExprValue && target.type == 'novalue')
        ? null
        : types.Some<dynamic>(target);
    return types.Some<
            types.Tuple3<types.Some<dynamic>?, dynamic,
                list.FSharpList<dynamic>>>(
        types.Tuple3<types.Some<dynamic>?, dynamic, list.FSharpList<dynamic>>(
            normalized, expr.fieldName, list.empty<dynamic>()));
  }
  return null;
}

// ===================================================================
// Evaluation
// ===================================================================

final Map<String, Function> _operators = {
  'op_Addition': (a, b) => a + b,
  'op_Subtraction': (a, b) => a - b,
  'op_Multiply': (a, b) => a * b,
  'op_Division': (a, b) => a is int && b is int ? a ~/ b : a / b,
  'op_Modulus': (a, b) => a % b,
  'op_Exponentiation': (a, b) => _pow(a, b),
  'op_UnaryNegation': (a) => -a,
  'op_UnaryPlus': (a) => a,
  'op_LogicalNot': (a) => !(a as bool),
  'op_BitwiseOr': (a, b) => a | b,
  'op_BitwiseAnd': (a, b) => a & b,
  'op_ExclusiveOr': (a, b) => a ^ b,
  'op_LeftShift': (a, b) => a << b,
  'op_RightShift': (a, b) => a >> b,
  'op_Equality': (a, b) => a == b,
  'op_Inequality': (a, b) => a != b,
  'op_LessThan': (a, b) => a < b,
  'op_LessThanOrEqual': (a, b) => a <= b,
  'op_GreaterThan': (a, b) => a > b,
  'op_GreaterThanOrEqual': (a, b) => a >= b,
  'op_BooleanAnd': (a, b) => (a as bool) && (b as bool),
  'op_BooleanOr': (a, b) => (a as bool) || (b as bool),
};

num _pow(num a, num b) {
  num result = 1;
  for (var i = 0; i < b; i++) {
    result *= a;
  }
  return result;
}

dynamic _evaluate(Expr expr, Map<String, dynamic> env) {
  if (expr is ExprValue) return expr.value;

  if (expr is ExprVarExpr) {
    if (env.containsKey(expr.variable.name)) return env[expr.variable.name];
    throw Exception('Unbound variable: ${expr.variable.name}');
  }

  if (expr is ExprLambda) {
    final capturedEnv = Map<String, dynamic>.from(env);
    return (dynamic arg) {
      final newEnv = Map<String, dynamic>.from(capturedEnv);
      newEnv[expr.variable.name] = arg;
      return _evaluate(expr.body, newEnv);
    };
  }

  if (expr is ExprApplication) {
    final f = _evaluate(expr.func, env);
    return f(_evaluate(expr.arg, env));
  }

  if (expr is ExprLet) {
    final newEnv = Map<String, dynamic>.from(env);
    newEnv[expr.variable.name] = _evaluate(expr.value, env);
    return _evaluate(expr.body, newEnv);
  }

  if (expr is ExprIfThenElse) {
    return _evaluate(expr.guard, env) as bool
        ? _evaluate(expr.thenExpr, env)
        : _evaluate(expr.elseExpr, env);
  }

  if (expr is ExprSequential) {
    _evaluate(expr.first, env);
    return _evaluate(expr.second, env);
  }

  if (expr is ExprNewTuple) {
    // Return the evaluated elements as a plain list. Dart reifies generics, so a
    // typed tuple (Tuple2<int,int>) can't be built from dynamic values; callers
    // treat an evaluated tuple as an obj[] (mirrors the TS/JS runtime).
    return expr.elements.map((e) => _evaluate(e, env)).toList();
  }

  if (expr is ExprCall) {
    final evaluatedArgs = expr.args.map((a) => _evaluate(a, env)).toList();
    final op = _operators[expr.method];
    if (op != null) return Function.apply(op, evaluatedArgs);
    throw Exception('Unknown method: ${expr.method}');
  }

  if (expr is ExprTupleGet) {
    final t = _evaluate(expr.expr, env);
    return t[expr.index];
  }

  if (expr is ExprNewUnion) {
    return [expr.unionTag, ...expr.fields.map((f) => _evaluate(f, env))];
  }

  if (expr is ExprNewRecord) {
    final result = <String, dynamic>{};
    for (var i = 0; i < expr.fieldNames.length; i++) {
      result[expr.fieldNames[i] as String] = _evaluate(expr.values[i], env);
    }
    return result;
  }

  if (expr is ExprNewList) {
    return [_evaluate(expr.head, env), ..._evaluate(expr.tail, env)];
  }

  if (expr is ExprVarSet) {
    final target = expr.target;
    if (target is ExprVarExpr) {
      env[target.variable.name] = _evaluate(expr.value, env);
      return null;
    }
    throw Exception('VarSet target must be a variable');
  }

  if (expr is ExprFieldGet) {
    final obj = _evaluate(expr.expr, env);
    if (obj is Map) return obj[expr.fieldName];
    throw Exception('Cannot get field ${expr.fieldName}');
  }

  throw Exception('Cannot evaluate expression: ${expr.tag}');
}

dynamic evaluate(Expr expr) => _evaluate(expr, <String, dynamic>{});

// ===================================================================
// FSharpExpr instance methods
// ===================================================================

const Map<String, String> _opSymbols = {
  'op_Addition': '+',
  'op_Subtraction': '-',
  'op_Multiply': '*',
  'op_Division': '/',
  'op_Modulus': '%',
  'op_Exponentiation': '**',
  'op_Equality': '=',
  'op_Inequality': '<>',
  'op_LessThan': '<',
  'op_LessThanOrEqual': '<=',
  'op_GreaterThan': '>',
  'op_GreaterThanOrEqual': '>=',
  'op_BooleanAnd': '&&',
  'op_BooleanOr': '||',
  'op_UnaryNegation': '-',
  'op_LogicalNot': 'not',
};

String exprToString(Expr expr) {
  if (expr is ExprValue) {
    if (expr.type == 'string') return '"${expr.value}"';
    if (expr.type == 'unit') return '()';
    if (expr.type == 'bool') return expr.value == true ? 'true' : 'false';
    return '${expr.value}';
  }
  if (expr is ExprVarExpr) return expr.variable.name;
  if (expr is ExprLambda) {
    return 'fun ${expr.variable.name} -> ${exprToString(expr.body)}';
  }
  if (expr is ExprApplication) {
    return '${exprToString(expr.func)} ${exprToString(expr.arg)}';
  }
  if (expr is ExprLet) {
    return 'let ${expr.variable.name} = ${exprToString(expr.value)} in ${exprToString(expr.body)}';
  }
  if (expr is ExprIfThenElse) {
    return 'if ${exprToString(expr.guard)} then ${exprToString(expr.thenExpr)} else ${exprToString(expr.elseExpr)}';
  }
  if (expr is ExprCall) {
    final sym = _opSymbols[expr.method];
    if (sym != null && expr.args.length == 2) {
      return '(${exprToString(expr.args[0] as Expr)} $sym ${exprToString(expr.args[1] as Expr)})';
    }
    if (sym != null && expr.args.length == 1) {
      return '$sym${exprToString(expr.args[0] as Expr)}';
    }
    final argStrs =
        expr.args.map((a) => exprToString(a as Expr)).join(', ');
    return '${expr.method}($argStrs)';
  }
  if (expr is ExprSequential) {
    return '${exprToString(expr.first)}; ${exprToString(expr.second)}';
  }
  if (expr is ExprNewTuple) {
    return '(${expr.elements.map((e) => exprToString(e as Expr)).join(', ')})';
  }
  if (expr is ExprTupleGet) {
    return 'Item${expr.index + 1}(${exprToString(expr.expr)})';
  }
  if (expr is ExprFieldGet) {
    return '${exprToString(expr.expr)}.${expr.fieldName}';
  }
  return '<${expr.tag}>';
}

// Returns the free variables as a (Fable) array (seq) of Var.
List<dynamic> getFreeVars(Expr expr) {
  final free = <dynamic>[];
  final seen = <String>{};

  void walk(Expr e, Set<String> bound) {
    if (e is ExprVarExpr) {
      if (!bound.contains(e.variable.name) && !seen.contains(e.variable.name)) {
        free.add(e.variable);
        seen.add(e.variable.name);
      }
    } else if (e is ExprLambda) {
      walk(e.body, {...bound, e.variable.name});
    } else if (e is ExprLet) {
      walk(e.value, bound);
      walk(e.body, {...bound, e.variable.name});
    } else if (e is ExprApplication) {
      walk(e.func, bound);
      walk(e.arg, bound);
    } else if (e is ExprIfThenElse) {
      walk(e.guard, bound);
      walk(e.thenExpr, bound);
      walk(e.elseExpr, bound);
    } else if (e is ExprCall) {
      for (final a in e.args) {
        walk(a as Expr, bound);
      }
    } else if (e is ExprSequential) {
      walk(e.first, bound);
      walk(e.second, bound);
    } else if (e is ExprNewTuple) {
      for (final el in e.elements) {
        walk(el as Expr, bound);
      }
    } else if (e is ExprNewUnion) {
      for (final f in e.fields) {
        walk(f as Expr, bound);
      }
    } else if (e is ExprNewRecord) {
      for (final v in e.values) {
        walk(v as Expr, bound);
      }
    } else if (e is ExprNewList) {
      walk(e.head, bound);
      walk(e.tail, bound);
    } else if (e is ExprTupleGet) {
      walk(e.expr, bound);
    } else if (e is ExprUnionTag) {
      walk(e.expr, bound);
    } else if (e is ExprUnionField) {
      walk(e.expr, bound);
    } else if (e is ExprFieldGet) {
      walk(e.expr, bound);
    } else if (e is ExprFieldSet) {
      walk(e.expr, bound);
      walk(e.value, bound);
    } else if (e is ExprVarSet) {
      walk(e.target, bound);
      walk(e.value, bound);
    }
  }

  walk(expr, <String>{});
  return free;
}

// Substitute variables using fn: Var -> Expr option (null = keep).
Expr substitute(Expr expr, dynamic fn) {
  Expr sub(Expr e) {
    if (e is ExprVarExpr) {
      // fn: Var -> Expr option; Fable wraps Some in a Some<...> (None = null).
      final result = fn(e.variable);
      if (result == null) return e;
      if (result is types.Some) return result.value as Expr;
      return result as Expr;
    }
    if (e is ExprLambda) return ExprLambda(e.variable, sub(e.body));
    if (e is ExprLet) return ExprLet(e.variable, sub(e.value), sub(e.body));
    if (e is ExprApplication) return ExprApplication(sub(e.func), sub(e.arg));
    if (e is ExprIfThenElse) {
      return ExprIfThenElse(sub(e.guard), sub(e.thenExpr), sub(e.elseExpr));
    }
    if (e is ExprCall) {
      final newInst = e.instance != null ? sub(e.instance!) : e.instance;
      return ExprCall(newInst, e.method,
          e.args.map((a) => sub(a as Expr)).toList(), e.declaringType);
    }
    if (e is ExprSequential) return ExprSequential(sub(e.first), sub(e.second));
    if (e is ExprNewTuple) {
      return ExprNewTuple(e.elements.map((a) => sub(a as Expr)).toList());
    }
    if (e is ExprTupleGet) return ExprTupleGet(sub(e.expr), e.index);
    if (e is ExprNewUnion) {
      return ExprNewUnion(
          e.typeName, e.unionTag, e.fields.map((a) => sub(a as Expr)).toList());
    }
    if (e is ExprNewRecord) {
      return ExprNewRecord(
          e.fieldNames, e.values.map((a) => sub(a as Expr)).toList());
    }
    if (e is ExprNewList) return ExprNewList(sub(e.head), sub(e.tail));
    if (e is ExprUnionTag) return ExprUnionTag(sub(e.expr));
    if (e is ExprUnionField) return ExprUnionField(sub(e.expr), e.fieldIndex);
    if (e is ExprFieldGet) return ExprFieldGet(sub(e.expr), e.fieldName);
    if (e is ExprFieldSet) {
      return ExprFieldSet(sub(e.expr), e.fieldName, sub(e.value));
    }
    if (e is ExprVarSet) return ExprVarSet(sub(e.target), sub(e.value));
    return e;
  }

  return sub(expr);
}
