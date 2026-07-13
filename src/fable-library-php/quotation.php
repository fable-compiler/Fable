<?php
// F# quotation runtime for the Fable PHP target.
// Mirrors fable-library-py/quotation.py: the QuotationEmitter lowers a quoted body
// into calls to the mk* constructors below (namespace + camelCase names match the
// LibCall "quotation" module). PHP is dynamically typed, so nodes are plain arrays
// tagged by 'tag' and options follow Fable's convention (null = None, value = Some).
namespace quotation;

// --- Var constructor + accessors ---

function mkQuotVar($name, $type, $isMutable = false) {
    return ['name' => $name, 'type' => $type, 'isMutable' => $isMutable];
}

function varGetName($v) { return $v['name']; }
function varGetType($v) { return $v['type']; }
function varGetIsMutable($v) { return $v['isMutable']; }

// --- Expr constructors ---

function mkValue($value, $type) { return ['tag' => 'Value', 'value' => $value, 'type' => $type]; }

// A null-value node (no instance / null literal / empty option or list).
function mkNull($type) { return ['tag' => 'Value', 'value' => null, 'type' => $type]; }

function mkVar($var) { return ['tag' => 'Var', 'var' => $var]; }
function mkLambda($var, $body) { return ['tag' => 'Lambda', 'var' => $var, 'body' => $body]; }
function mkApplication($func, $arg) { return ['tag' => 'Application', 'func' => $func, 'arg' => $arg]; }
function mkLet($var, $value, $body) { return ['tag' => 'Let', 'var' => $var, 'value' => $value, 'body' => $body]; }

function mkIfThenElse($guard, $thenExpr, $elseExpr) {
    return ['tag' => 'IfThenElse', 'guard' => $guard, 'thenExpr' => $thenExpr, 'elseExpr' => $elseExpr];
}

function mkCall($instance, $method, $args, $declaringType = '') {
    return ['tag' => 'Call', 'instance' => $instance, 'method' => $method, 'args' => $args, 'declaringType' => $declaringType];
}

function mkSequential($first, $second) { return ['tag' => 'Sequential', 'first' => $first, 'second' => $second]; }
function mkNewTuple($elements) { return ['tag' => 'NewTuple', 'elements' => $elements]; }

function mkNewUnion($typeName, $tag, $fields) {
    return ['tag' => 'NewUnion', 'typeName' => $typeName, 'unionTag' => $tag, 'fields' => $fields];
}

function mkNewRecord($fieldNames, $values) {
    return ['tag' => 'NewRecord', 'fieldNames' => $fieldNames, 'values' => $values];
}

function mkNewList($head, $tail) { return ['tag' => 'NewList', 'head' => $head, 'tail' => $tail]; }
function mkTupleGet($expr, $index) { return ['tag' => 'TupleGet', 'expr' => $expr, 'index' => $index]; }
function mkUnionTag($expr) { return ['tag' => 'UnionTag', 'expr' => $expr]; }
function mkUnionField($expr, $fieldIndex) { return ['tag' => 'UnionField', 'expr' => $expr, 'fieldIndex' => $fieldIndex]; }
function mkFieldGet($expr, $fieldName) { return ['tag' => 'FieldGet', 'expr' => $expr, 'fieldName' => $fieldName]; }
function mkFieldSet($expr, $fieldName, $value) { return ['tag' => 'FieldSet', 'expr' => $expr, 'fieldName' => $fieldName, 'value' => $value]; }
function mkVarSet($target, $value) { return ['tag' => 'VarSet', 'target' => $target, 'value' => $value]; }

// --- Type accessor ---

function getType($e) {
    switch ($e['tag']) {
        case 'Value': return $e['type'];
        case 'Lambda': return $e['var']['type'];
        default: return 'obj';
    }
}

// --- Pattern helpers (null = no match, value = match) ---

function isValue($e) { return $e['tag'] === 'Value' ? [$e['value'], $e['type']] : null; }
function isVar($e) { return $e['tag'] === 'Var' ? $e['var'] : null; }
function isLambda($e) { return $e['tag'] === 'Lambda' ? [$e['var'], $e['body']] : null; }
function isApplication($e) { return $e['tag'] === 'Application' ? [$e['func'], $e['arg']] : null; }
function isLet($e) { return $e['tag'] === 'Let' ? [$e['var'], $e['value'], $e['body']] : null; }
function isIfThenElse($e) { return $e['tag'] === 'IfThenElse' ? [$e['guard'], $e['thenExpr'], $e['elseExpr']] : null; }

function isCall($e) {
    if ($e['tag'] !== 'Call') return null;
    // A static/operator call carries the null-value node as its instance;
    // expose it as null so Patterns.Call matches F#.
    $inst = $e['instance'];
    if (is_array($inst) && ($inst['tag'] ?? null) === 'Value' && ($inst['type'] ?? null) === 'null') {
        $inst = null;
    }
    return [$inst, $e['method'], $e['args']];
}

function isSequential($e) { return $e['tag'] === 'Sequential' ? [$e['first'], $e['second']] : null; }
function isNewTuple($e) { return $e['tag'] === 'NewTuple' ? $e['elements'] : null; }
function isNewUnionCase($e) { return $e['tag'] === 'NewUnion' ? [$e['typeName'], $e['unionTag'], $e['fields']] : null; }
function isNewRecord($e) { return $e['tag'] === 'NewRecord' ? [$e['fieldNames'], $e['values']] : null; }
function isTupleGet($e) { return $e['tag'] === 'TupleGet' ? [$e['expr'], $e['index']] : null; }
function isFieldGet($e) { return $e['tag'] === 'FieldGet' ? [$e['expr'], $e['fieldName']] : null; }

// --- Free variables ---

function getFreeVars($e) {
    $free = [];
    $seen = [];
    $walk = function ($e, $bound) use (&$walk, &$free, &$seen) {
        switch ($e['tag']) {
            case 'Var':
                $name = $e['var']['name'];
                if (!in_array($name, $bound, true) && !in_array($name, $seen, true)) {
                    $free[] = $e['var'];
                    $seen[] = $name;
                }
                break;
            case 'Lambda':
                $walk($e['body'], array_merge($bound, [$e['var']['name']]));
                break;
            case 'Let':
                $walk($e['value'], $bound);
                $walk($e['body'], array_merge($bound, [$e['var']['name']]));
                break;
            case 'Application':
                $walk($e['func'], $bound);
                $walk($e['arg'], $bound);
                break;
            case 'IfThenElse':
                $walk($e['guard'], $bound);
                $walk($e['thenExpr'], $bound);
                $walk($e['elseExpr'], $bound);
                break;
            case 'Call':
                foreach ($e['args'] as $a) $walk($a, $bound);
                break;
            case 'Sequential':
                $walk($e['first'], $bound);
                $walk($e['second'], $bound);
                break;
            case 'NewTuple':
                foreach ($e['elements'] as $el) $walk($el, $bound);
                break;
            case 'TupleGet':
                $walk($e['expr'], $bound);
                break;
            case 'FieldGet':
                $walk($e['expr'], $bound);
                break;
        }
    };
    $walk($e, []);
    return $free;
}

// --- Substitution ---

function substitute($e, $fn) {
    $sub = function ($e) use (&$sub, $fn) {
        switch ($e['tag']) {
            case 'Var':
                $r = $fn($e['var']);
                return $r !== null ? $r : $e;
            case 'Lambda': return mkLambda($e['var'], $sub($e['body']));
            case 'Let': return mkLet($e['var'], $sub($e['value']), $sub($e['body']));
            case 'Application': return mkApplication($sub($e['func']), $sub($e['arg']));
            case 'IfThenElse': return mkIfThenElse($sub($e['guard']), $sub($e['thenExpr']), $sub($e['elseExpr']));
            case 'Call':
                return mkCall($e['instance'], $e['method'], array_map($sub, $e['args']), $e['declaringType']);
            case 'Sequential': return mkSequential($sub($e['first']), $sub($e['second']));
            case 'NewTuple': return mkNewTuple(array_map($sub, $e['elements']));
            default: return $e;
        }
    };
    return $sub($e);
}

// --- Evaluation (structural cases + common operators) ---

const OPERATORS = [
    'op_Addition', 'op_Subtraction', 'op_Multiply', 'op_Division', 'op_Modulus',
    'op_UnaryNegation', 'op_Equality', 'op_Inequality', 'op_LessThan', 'op_LessThanOrEqual',
    'op_GreaterThan', 'op_GreaterThanOrEqual', 'op_BooleanAnd', 'op_BooleanOr', 'op_LogicalNot',
];

function applyOperator($method, $args) {
    switch ($method) {
        case 'op_Addition': return $args[0] + $args[1];
        case 'op_Subtraction': return $args[0] - $args[1];
        case 'op_Multiply': return $args[0] * $args[1];
        case 'op_Division': return intdiv($args[0], $args[1]);
        case 'op_Modulus': return $args[0] % $args[1];
        case 'op_UnaryNegation': return -$args[0];
        case 'op_Equality': return $args[0] === $args[1];
        case 'op_Inequality': return $args[0] !== $args[1];
        case 'op_LessThan': return $args[0] < $args[1];
        case 'op_LessThanOrEqual': return $args[0] <= $args[1];
        case 'op_GreaterThan': return $args[0] > $args[1];
        case 'op_GreaterThanOrEqual': return $args[0] >= $args[1];
        case 'op_BooleanAnd': return $args[0] && $args[1];
        case 'op_BooleanOr': return $args[0] || $args[1];
        case 'op_LogicalNot': return !$args[0];
        default: throw new \Exception("Cannot evaluate method: $method");
    }
}

function evaluate($e) {
    $eval = function ($e, $env) use (&$eval) {
        switch ($e['tag']) {
            case 'Value': return $e['value'];
            case 'Var': return $env[$e['var']['name']];
            case 'Lambda':
                $var = $e['var']; $body = $e['body'];
                return function ($arg) use (&$eval, $var, $body, $env) {
                    $newEnv = $env; $newEnv[$var['name']] = $arg;
                    return $eval($body, $newEnv);
                };
            case 'Application':
                $f = $eval($e['func'], $env);
                return $f($eval($e['arg'], $env));
            case 'Let':
                $newEnv = $env; $newEnv[$e['var']['name']] = $eval($e['value'], $env);
                return $eval($e['body'], $newEnv);
            case 'IfThenElse':
                return $eval($e['guard'], $env) ? $eval($e['thenExpr'], $env) : $eval($e['elseExpr'], $env);
            case 'Sequential':
                $eval($e['first'], $env);
                return $eval($e['second'], $env);
            case 'NewTuple':
                return array_map(function ($el) use (&$eval, $env) { return $eval($el, $env); }, $e['elements']);
            case 'Call':
                $evalArgs = array_map(function ($a) use (&$eval, $env) { return $eval($a, $env); }, $e['args']);
                return applyOperator($e['method'], $evalArgs);
            case 'TupleGet':
                return $eval($e['expr'], $env)[$e['index']];
            default:
                throw new \Exception("Cannot evaluate expression: " . $e['tag']);
        }
    };
    return $eval($e, []);
}
