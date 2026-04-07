/**
 * F# Quotation runtime support for Fable JS/TS target.
 *
 * Provides class-based representations of F# quotation AST nodes
 * and pattern matching helpers compatible with Fable's option convention
 * (undefined = no match, value/tuple = match).
 */

// ===================================================================
// Var: represents an F# quotation variable
// ===================================================================

export class Var {
    readonly Name: string;
    readonly Type: string;
    readonly IsMutable: boolean;

    constructor(name: string, type_: string, isMutable: boolean) {
        this.Name = name;
        this.Type = type_;
        this.IsMutable = isMutable;
    }
}

export function mkQuotVar(name: string, type_: string, isMutable: boolean = false): Var {
    return new Var(name, type_, isMutable);
}

export function varGetName(v: Var): string { return v.Name; }
export function varGetType(v: Var): string { return v.Type; }
export function varGetIsMutable(v: Var): boolean { return v.IsMutable; }

// ===================================================================
// Expr nodes: tagged classes for each quotation expression kind
// ===================================================================

export class ExprValue {
    readonly tag = "Value";
    value: any;
    type: string;
    constructor(value: any, type: string) { this.value = value; this.type = type; }
    toJSON() { return ["Value", this.value, this.type]; }
}

export class ExprVarExpr {
    readonly tag = "Var";
    var_: Var;
    constructor(var_: Var) { this.var_ = var_; }
    toJSON() { return ["Var", this.var_]; }
}

export class ExprLambda {
    readonly tag = "Lambda";
    var_: Var;
    body: Expr;
    constructor(var_: Var, body: Expr) { this.var_ = var_; this.body = body; }
    toJSON() { return ["Lambda", this.var_, this.body]; }
}

export class ExprApplication {
    readonly tag = "Application";
    func: Expr;
    arg: Expr;
    constructor(func: Expr, arg: Expr) { this.func = func; this.arg = arg; }
    toJSON() { return ["Application", this.func, this.arg]; }
}

export class ExprLet {
    readonly tag = "Let";
    var_: Var;
    value: Expr;
    body: Expr;
    constructor(var_: Var, value: Expr, body: Expr) { this.var_ = var_; this.value = value; this.body = body; }
    toJSON() { return ["Let", this.var_, this.value, this.body]; }
}

export class ExprIfThenElse {
    readonly tag = "IfThenElse";
    guard: Expr;
    thenExpr: Expr;
    elseExpr: Expr;
    constructor(guard: Expr, thenExpr: Expr, elseExpr: Expr) { this.guard = guard; this.thenExpr = thenExpr; this.elseExpr = elseExpr; }
    toJSON() { return ["IfThenElse", this.guard, this.thenExpr, this.elseExpr]; }
}

export class ExprCall {
    readonly tag = "Call";
    instance: any;
    method: string;
    args: any[];
    constructor(instance: any, method: string, args: any[]) { this.instance = instance; this.method = method; this.args = args; }
    toJSON() { return ["Call", this.instance, this.method, this.args]; }
}

export class ExprSequential {
    readonly tag = "Sequential";
    first: Expr;
    second: Expr;
    constructor(first: Expr, second: Expr) { this.first = first; this.second = second; }
    toJSON() { return ["Sequential", this.first, this.second]; }
}

export class ExprNewTuple {
    readonly tag = "NewTuple";
    elements: any[];
    constructor(elements: any[]) { this.elements = elements; }
    toJSON() { return ["NewTuple", this.elements]; }
}

export class ExprNewUnion {
    readonly tag = "NewUnion";
    typeName: string;
    unionTag: number;
    fields: any[];
    constructor(typeName: string, unionTag: number, fields: any[]) { this.typeName = typeName; this.unionTag = unionTag; this.fields = fields; }
    toJSON() { return ["NewUnion", this.typeName, this.unionTag, this.fields]; }
}

export class ExprNewRecord {
    readonly tag = "NewRecord";
    fieldNames: any[];
    values: any[];
    constructor(fieldNames: any[], values: any[]) { this.fieldNames = fieldNames; this.values = values; }
    toJSON() { return ["NewRecord", this.fieldNames, this.values]; }
}

export class ExprNewList {
    readonly tag = "NewList";
    head: Expr;
    tail: Expr;
    constructor(head: Expr, tail: Expr) { this.head = head; this.tail = tail; }
    toJSON() { return ["NewList", this.head, this.tail]; }
}

export class ExprTupleGet {
    readonly tag = "TupleGet";
    expr: Expr;
    index: number;
    constructor(expr: Expr, index: number) { this.expr = expr; this.index = index; }
    toJSON() { return ["TupleGet", this.expr, this.index]; }
}

export class ExprUnionTag {
    readonly tag = "UnionTag";
    expr: Expr;
    constructor(expr: Expr) { this.expr = expr; }
    toJSON() { return ["UnionTag", this.expr]; }
}

export class ExprUnionField {
    readonly tag = "UnionField";
    expr: Expr;
    fieldIndex: number;
    constructor(expr: Expr, fieldIndex: number) { this.expr = expr; this.fieldIndex = fieldIndex; }
    toJSON() { return ["UnionField", this.expr, this.fieldIndex]; }
}

export class ExprFieldGet {
    readonly tag = "FieldGet";
    expr: Expr;
    fieldName: string;
    constructor(expr: Expr, fieldName: string) { this.expr = expr; this.fieldName = fieldName; }
    toJSON() { return ["FieldGet", this.expr, this.fieldName]; }
}

export class ExprFieldSet {
    readonly tag = "FieldSet";
    expr: Expr;
    fieldName: string;
    value: Expr;
    constructor(expr: Expr, fieldName: string, value: Expr) { this.expr = expr; this.fieldName = fieldName; this.value = value; }
    toJSON() { return ["FieldSet", this.expr, this.fieldName, this.value]; }
}

export class ExprVarSet {
    readonly tag = "VarSet";
    target: Expr;
    value: Expr;
    constructor(target: Expr, value: Expr) { this.target = target; this.value = value; }
    toJSON() { return ["VarSet", this.target, this.value]; }
}

export type Expr =
    | ExprValue
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
    | ExprVarSet;

// ===================================================================
// Constructors (called by QuotationEmitter.fs)
// ===================================================================

export function mkValue(value: any, type: string): ExprValue {
    return new ExprValue(value, type);
}

export function mkVar(v: Var): ExprVarExpr {
    return new ExprVarExpr(v);
}

export function mkLambda(v: Var, body: Expr): ExprLambda {
    return new ExprLambda(v, body);
}

export function mkApplication(func: Expr, arg: Expr): ExprApplication {
    return new ExprApplication(func, arg);
}

export function mkLet(v: Var, value: Expr, body: Expr): ExprLet {
    return new ExprLet(v, value, body);
}

export function mkIfThenElse(guard: Expr, thenExpr: Expr, elseExpr: Expr): ExprIfThenElse {
    return new ExprIfThenElse(guard, thenExpr, elseExpr);
}

export function mkCall(instance: any, method: string, args: any[]): ExprCall {
    return new ExprCall(instance, method, args);
}

export function mkSequential(first: Expr, second: Expr): ExprSequential {
    return new ExprSequential(first, second);
}

export function mkNewTuple(elements: any[]): ExprNewTuple {
    return new ExprNewTuple(elements);
}

export function mkTupleGet(expr: Expr, index: number): ExprTupleGet {
    return new ExprTupleGet(expr, index);
}

export function mkUnionTag(expr: Expr): ExprUnionTag {
    return new ExprUnionTag(expr);
}

export function mkUnionField(expr: Expr, fieldIndex: number): ExprUnionField {
    return new ExprUnionField(expr, fieldIndex);
}

export function mkFieldGet(expr: Expr, fieldName: string): ExprFieldGet {
    return new ExprFieldGet(expr, fieldName);
}

export function mkFieldSet(expr: Expr, fieldName: string, value: Expr): ExprFieldSet {
    return new ExprFieldSet(expr, fieldName, value);
}

export function mkVarSet(target: Expr, value: Expr): ExprVarSet {
    return new ExprVarSet(target, value);
}

export function mkNewUnion(typeName: string, tag: number, fields: any[]): ExprNewUnion {
    return new ExprNewUnion(typeName, tag, fields);
}

export function mkNewRecord(fieldNames: any[], values: any[]): ExprNewRecord {
    return new ExprNewRecord(fieldNames, values);
}

export function mkNewList(head: Expr, tail: Expr): ExprNewList {
    return new ExprNewList(head, tail);
}

// ===================================================================
// Accessors
// ===================================================================

export function getType(expr: Expr): string {
    if (expr instanceof ExprValue) return expr.type;
    if (expr instanceof ExprLambda) return expr.var_.Type;
    return "obj";
}

// ===================================================================
// Pattern match helpers
// Returns undefined (no match) or a value/tuple (match), following
// Fable's option convention for active patterns.
// ===================================================================

export function isValue(expr: Expr): [any, string] | undefined {
    if (expr instanceof ExprValue) return [expr.value, expr.type];
    return undefined;
}

export function isVar(expr: Expr): Var | undefined {
    if (expr instanceof ExprVarExpr) return expr.var_;
    return undefined;
}

export function isLambda(expr: Expr): [Var, Expr] | undefined {
    if (expr instanceof ExprLambda) return [expr.var_, expr.body];
    return undefined;
}

export function isApplication(expr: Expr): [Expr, Expr] | undefined {
    if (expr instanceof ExprApplication) return [expr.func, expr.arg];
    return undefined;
}

export function isLet(expr: Expr): [Var, Expr, Expr] | undefined {
    if (expr instanceof ExprLet) return [expr.var_, expr.value, expr.body];
    return undefined;
}

export function isIfThenElse(expr: Expr): [Expr, Expr, Expr] | undefined {
    if (expr instanceof ExprIfThenElse) return [expr.guard, expr.thenExpr, expr.elseExpr];
    return undefined;
}

export function isCall(expr: Expr): [any, string, any[]] | undefined {
    if (expr instanceof ExprCall) return [expr.instance, expr.method, expr.args];
    return undefined;
}

export function isSequential(expr: Expr): [Expr, Expr] | undefined {
    if (expr instanceof ExprSequential) return [expr.first, expr.second];
    return undefined;
}

export function isNewTuple(expr: Expr): any[] | undefined {
    if (expr instanceof ExprNewTuple) return expr.elements;
    return undefined;
}

export function isNewUnionCase(expr: Expr): [string, any[]] | undefined {
    if (expr instanceof ExprNewUnion) return [expr.typeName, expr.fields];
    return undefined;
}

export function isNewRecord(expr: Expr): [any[], any[]] | undefined {
    if (expr instanceof ExprNewRecord) return [expr.fieldNames, expr.values];
    return undefined;
}

export function isTupleGet(expr: Expr): [Expr, number] | undefined {
    if (expr instanceof ExprTupleGet) return [expr.expr, expr.index];
    return undefined;
}

export function isFieldGet(expr: Expr): [Expr, string] | undefined {
    if (expr instanceof ExprFieldGet) return [expr.expr, expr.fieldName];
    return undefined;
}

// ===================================================================
// Evaluation
// ===================================================================

const OPERATORS: Record<string, (...args: any[]) => any> = {
    "op_Addition": (a: any, b: any) => a + b,
    "op_Subtraction": (a: any, b: any) => a - b,
    "op_Multiply": (a: any, b: any) => a * b,
    "op_Division": (a: any, b: any) => (a / b) | 0,
    "op_Modulus": (a: any, b: any) => a % b,
    "op_Exponentiation": (a: any, b: any) => a ** b,
    "op_UnaryNegation": (a: any) => -a,
    "op_UnaryPlus": (a: any) => +a,
    "op_LogicalNot": (a: any) => !a,
    "op_BitwiseOr": (a: any, b: any) => a | b,
    "op_BitwiseAnd": (a: any, b: any) => a & b,
    "op_ExclusiveOr": (a: any, b: any) => a ^ b,
    "op_LeftShift": (a: any, b: any) => a << b,
    "op_RightShift": (a: any, b: any) => a >> b,
    "op_Equality": (a: any, b: any) => a === b,
    "op_Inequality": (a: any, b: any) => a !== b,
    "op_LessThan": (a: any, b: any) => a < b,
    "op_LessThanOrEqual": (a: any, b: any) => a <= b,
    "op_GreaterThan": (a: any, b: any) => a > b,
    "op_GreaterThanOrEqual": (a: any, b: any) => a >= b,
    "op_BooleanAnd": (a: any, b: any) => a && b,
    "op_BooleanOr": (a: any, b: any) => a || b,
};

export function evaluate(expr: Expr, env?: Map<string, any>): any {
    if (env == null) env = new Map();

    if (expr instanceof ExprValue) return expr.value;

    if (expr instanceof ExprVarExpr) {
        if (env.has(expr.var_.Name)) return env.get(expr.var_.Name);
        throw new Error(`Unbound variable: ${expr.var_.Name}`);
    }

    if (expr instanceof ExprLambda) {
        const capturedEnv = new Map(env);
        return (arg: any) => {
            const newEnv = new Map(capturedEnv);
            newEnv.set(expr.var_.Name, arg);
            return evaluate(expr.body, newEnv);
        };
    }

    if (expr instanceof ExprApplication) {
        return evaluate(expr.func, env)(evaluate(expr.arg, env));
    }

    if (expr instanceof ExprLet) {
        const newEnv = new Map(env);
        newEnv.set(expr.var_.Name, evaluate(expr.value, env));
        return evaluate(expr.body, newEnv);
    }

    if (expr instanceof ExprIfThenElse) {
        return evaluate(expr.guard, env) ? evaluate(expr.thenExpr, env) : evaluate(expr.elseExpr, env);
    }

    if (expr instanceof ExprSequential) {
        evaluate(expr.first, env);
        return evaluate(expr.second, env);
    }

    if (expr instanceof ExprNewTuple) {
        return expr.elements.map(e => evaluate(e, env));
    }

    if (expr instanceof ExprCall) {
        const evaluatedArgs = expr.args.map((a: any) => evaluate(a, env));
        if (expr.method in OPERATORS) return OPERATORS[expr.method](...evaluatedArgs);
        throw new Error(`Unknown method: ${expr.method}`);
    }

    if (expr instanceof ExprTupleGet) {
        return evaluate(expr.expr, env)[expr.index];
    }

    if (expr instanceof ExprNewUnion) {
        return [expr.unionTag, ...expr.fields.map((f: any) => evaluate(f, env))];
    }

    if (expr instanceof ExprNewRecord) {
        const result: Record<string, any> = {};
        for (let i = 0; i < expr.fieldNames.length; i++) {
            result[expr.fieldNames[i]] = evaluate(expr.values[i], env);
        }
        return result;
    }

    if (expr instanceof ExprNewList) {
        return [evaluate(expr.head, env), ...evaluate(expr.tail, env)];
    }

    if (expr instanceof ExprVarSet) {
        if (expr.target instanceof ExprVarExpr) {
            env.set(expr.target.var_.Name, evaluate(expr.value, env));
            return undefined;
        }
        throw new Error("VarSet target must be a variable");
    }

    if (expr instanceof ExprFieldGet) {
        const obj = evaluate(expr.expr, env);
        return obj[expr.fieldName];
    }

    throw new Error(`Cannot evaluate expression: ${(expr as any).tag}`);
}

// ===================================================================
// FSharpExpr instance methods
// ===================================================================

const OP_SYMBOLS: Record<string, string> = {
    "op_Addition": "+",
    "op_Subtraction": "-",
    "op_Multiply": "*",
    "op_Division": "/",
    "op_Modulus": "%",
    "op_Exponentiation": "**",
    "op_Equality": "=",
    "op_Inequality": "<>",
    "op_LessThan": "<",
    "op_LessThanOrEqual": "<=",
    "op_GreaterThan": ">",
    "op_GreaterThanOrEqual": ">=",
    "op_BooleanAnd": "&&",
    "op_BooleanOr": "||",
    "op_UnaryNegation": "-",
    "op_LogicalNot": "not",
};

export function exprToString(expr: Expr): string {
    if (expr instanceof ExprValue) {
        if (expr.type === "string") return `"${expr.value}"`;
        if (expr.type === "unit") return "()";
        if (expr.type === "bool") return expr.value ? "true" : "false";
        return String(expr.value);
    }
    if (expr instanceof ExprVarExpr) return expr.var_.Name;
    if (expr instanceof ExprLambda) return `fun ${expr.var_.Name} -> ${exprToString(expr.body)}`;
    if (expr instanceof ExprApplication) return `${exprToString(expr.func)} ${exprToString(expr.arg)}`;
    if (expr instanceof ExprLet) return `let ${expr.var_.Name} = ${exprToString(expr.value)} in ${exprToString(expr.body)}`;
    if (expr instanceof ExprIfThenElse) return `if ${exprToString(expr.guard)} then ${exprToString(expr.thenExpr)} else ${exprToString(expr.elseExpr)}`;
    if (expr instanceof ExprCall) {
        if (expr.method in OP_SYMBOLS && expr.args.length === 2) {
            return `(${exprToString(expr.args[0])} ${OP_SYMBOLS[expr.method]} ${exprToString(expr.args[1])})`;
        }
        if (expr.method in OP_SYMBOLS && expr.args.length === 1) {
            return `${OP_SYMBOLS[expr.method]}${exprToString(expr.args[0])}`;
        }
        return `${expr.method}(${expr.args.map(exprToString).join(", ")})`;
    }
    if (expr instanceof ExprSequential) return `${exprToString(expr.first)}; ${exprToString(expr.second)}`;
    if (expr instanceof ExprNewTuple) return `(${expr.elements.map(exprToString).join(", ")})`;
    if (expr instanceof ExprTupleGet) return `Item${expr.index + 1}(${exprToString(expr.expr)})`;
    if (expr instanceof ExprFieldGet) return `${exprToString(expr.expr)}.${expr.fieldName}`;
    return `<${(expr as any).tag}>`;
}

export function getFreeVars(expr: Expr): Var[] {
    const free: Var[] = [];
    const seen = new Set<string>();

    function walk(e: Expr, bound: Set<string>): void {
        if (e instanceof ExprVarExpr) {
            if (!bound.has(e.var_.Name) && !seen.has(e.var_.Name)) {
                free.push(e.var_);
                seen.add(e.var_.Name);
            }
        } else if (e instanceof ExprLambda) {
            walk(e.body, new Set([...bound, e.var_.Name]));
        } else if (e instanceof ExprLet) {
            walk(e.value, bound);
            walk(e.body, new Set([...bound, e.var_.Name]));
        } else if (e instanceof ExprApplication) {
            walk(e.func, bound);
            walk(e.arg, bound);
        } else if (e instanceof ExprIfThenElse) {
            walk(e.guard, bound);
            walk(e.thenExpr, bound);
            walk(e.elseExpr, bound);
        } else if (e instanceof ExprCall) {
            for (const a of e.args) walk(a, bound);
        } else if (e instanceof ExprSequential) {
            walk(e.first, bound);
            walk(e.second, bound);
        } else if (e instanceof ExprNewTuple) {
            for (const el of e.elements) walk(el, bound);
        } else if (e instanceof ExprTupleGet) {
            walk(e.expr, bound);
        } else if (e instanceof ExprFieldGet) {
            walk(e.expr, bound);
        }
    }

    walk(expr, new Set());
    return free;
}

export function substitute(expr: Expr, fn: (v: Var) => Expr | undefined): Expr {
    function sub(e: Expr): Expr {
        if (e instanceof ExprVarExpr) {
            const result = fn(e.var_);
            return result !== undefined ? result : e;
        }
        if (e instanceof ExprLambda) return new ExprLambda(e.var_, sub(e.body));
        if (e instanceof ExprLet) return new ExprLet(e.var_, sub(e.value), sub(e.body));
        if (e instanceof ExprApplication) return new ExprApplication(sub(e.func), sub(e.arg));
        if (e instanceof ExprIfThenElse) return new ExprIfThenElse(sub(e.guard), sub(e.thenExpr), sub(e.elseExpr));
        if (e instanceof ExprCall) {
            const newInst = e.instance instanceof ExprVarExpr || e.instance instanceof ExprValue
                || e.instance instanceof ExprLambda || e.instance instanceof ExprApplication
                || e.instance instanceof ExprLet || e.instance instanceof ExprIfThenElse
                || e.instance instanceof ExprCall || e.instance instanceof ExprSequential
                || e.instance instanceof ExprNewTuple
                ? sub(e.instance) : e.instance;
            return new ExprCall(newInst, e.method, e.args.map(sub));
        }
        if (e instanceof ExprSequential) return new ExprSequential(sub(e.first), sub(e.second));
        if (e instanceof ExprNewTuple) return new ExprNewTuple(e.elements.map(sub));
        if (e instanceof ExprTupleGet) return new ExprTupleGet(sub(e.expr), e.index);
        if (e instanceof ExprFieldGet) return new ExprFieldGet(sub(e.expr), e.fieldName);
        return e;
    }
    return sub(expr);
}

// ===================================================================
// JSON deserialization
// Reconstructs Expr/Var from the toJSON() array format.
// ===================================================================

function varFromJSON(json: any): Var {
    return new Var(json.Name, json.Type, json.IsMutable);
}

export function exprFromJSON(json: any): Expr {
    if (!Array.isArray(json)) return new ExprValue(json, typeof json);
    const [tag, ...fields] = json;
    switch (tag) {
        case "Value": return new ExprValue(fields[0], fields[1]);
        case "Var": return new ExprVarExpr(varFromJSON(fields[0]));
        case "Lambda": return new ExprLambda(varFromJSON(fields[0]), exprFromJSON(fields[1]));
        case "Application": return new ExprApplication(exprFromJSON(fields[0]), exprFromJSON(fields[1]));
        case "Let": return new ExprLet(varFromJSON(fields[0]), exprFromJSON(fields[1]), exprFromJSON(fields[2]));
        case "IfThenElse": return new ExprIfThenElse(exprFromJSON(fields[0]), exprFromJSON(fields[1]), exprFromJSON(fields[2]));
        case "Call": return new ExprCall(fields[0] != null ? exprFromJSON(fields[0]) : null, fields[1], (fields[2] as any[]).map(exprFromJSON));
        case "Sequential": return new ExprSequential(exprFromJSON(fields[0]), exprFromJSON(fields[1]));
        case "NewTuple": return new ExprNewTuple((fields[0] as any[]).map(exprFromJSON));
        case "TupleGet": return new ExprTupleGet(exprFromJSON(fields[0]), fields[1]);
        case "NewUnion": return new ExprNewUnion(fields[0], fields[1], (fields[2] as any[]).map(exprFromJSON));
        case "UnionTag": return new ExprUnionTag(exprFromJSON(fields[0]));
        case "UnionField": return new ExprUnionField(exprFromJSON(fields[0]), fields[1]);
        case "NewRecord": return new ExprNewRecord(fields[0], (fields[1] as any[]).map(exprFromJSON));
        case "FieldGet": return new ExprFieldGet(exprFromJSON(fields[0]), fields[1]);
        case "FieldSet": return new ExprFieldSet(exprFromJSON(fields[0]), fields[1], exprFromJSON(fields[2]));
        case "VarSet": return new ExprVarSet(exprFromJSON(fields[0]), exprFromJSON(fields[1]));
        case "NewList": return new ExprNewList(exprFromJSON(fields[0]), exprFromJSON(fields[1]));
        default: return new ExprValue(json, "unknown");
    }
}
