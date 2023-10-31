import { Record, Union } from "./Types.js";
import { combineHashCodes, equalArraysWith, stringHash } from "./Util.js";
import Decimal from "./Decimal.js";
export class CaseInfo {
    constructor(declaringType, tag, name, fields) {
        this.declaringType = declaringType;
        this.tag = tag;
        this.name = name;
        this.fields = fields;
    }
}
export class MethodInfo {
    constructor(name, parameters, returnType) {
        this.name = name;
        this.parameters = parameters;
        this.returnType = returnType;
    }
}
export class TypeInfo {
    constructor(fullname, generics, construct, parent, fields, cases, enumCases) {
        this.fullname = fullname;
        this.generics = generics;
        this.construct = construct;
        this.parent = parent;
        this.fields = fields;
        this.cases = cases;
        this.enumCases = enumCases;
    }
    toString() {
        return fullName(this);
    }
    GetHashCode() {
        return getHashCode(this);
    }
    Equals(other) {
        return equals(this, other);
    }
}
export class GenericParameter extends TypeInfo {
    constructor(name) {
        super(name);
    }
}
export function getGenerics(t) {
    return t.generics != null ? t.generics : [];
}
export function getHashCode(t) {
    const fullnameHash = stringHash(t.fullname);
    const genHashes = getGenerics(t).map(getHashCode);
    return combineHashCodes([fullnameHash, ...genHashes]);
}
export function equals(t1, t2) {
    if (t1.fullname === "") { // Anonymous records
        return t2.fullname === ""
            && equalArraysWith(getRecordElements(t1), getRecordElements(t2), ([k1, v1], [k2, v2]) => k1 === k2 && equals(v1, v2));
    }
    else {
        return t1.fullname === t2.fullname
            && equalArraysWith(getGenerics(t1), getGenerics(t2), equals);
    }
}
export function class_type(fullname, generics, construct, parent) {
    return new TypeInfo(fullname, generics, construct, parent);
}
export function record_type(fullname, generics, construct, fields) {
    return new TypeInfo(fullname, generics, construct, undefined, fields);
}
export function anonRecord_type(...fields) {
    return new TypeInfo("", undefined, undefined, undefined, () => fields);
}
export function union_type(fullname, generics, construct, cases) {
    const t = new TypeInfo(fullname, generics, construct, undefined, undefined, () => {
        const caseNames = construct.prototype.cases();
        return cases().map((fields, i) => new CaseInfo(t, i, caseNames[i], fields));
    });
    return t;
}
export function tuple_type(...generics) {
    return new TypeInfo("System.Tuple`" + generics.length, generics);
}
export function delegate_type(...generics) {
    return new TypeInfo("System.Func`" + generics.length, generics);
}
export function lambda_type(argType, returnType) {
    return new TypeInfo("Microsoft.FSharp.Core.FSharpFunc`2", [argType, returnType]);
}
export function option_type(generic) {
    return new TypeInfo("Microsoft.FSharp.Core.FSharpOption`1", [generic]);
}
export function list_type(generic) {
    return new TypeInfo("Microsoft.FSharp.Collections.FSharpList`1", [generic]);
}
export function array_type(generic) {
    return new TypeInfo("[]", [generic]);
}
export function enum_type(fullname, underlyingType, enumCases) {
    return new TypeInfo(fullname, [underlyingType], undefined, undefined, undefined, undefined, enumCases);
}
export function measure_type(fullname) {
    return new TypeInfo(fullname);
}
export function generic_type(name) {
    return new GenericParameter(name);
}
export const obj_type = new TypeInfo("System.Object");
export const unit_type = new TypeInfo("Microsoft.FSharp.Core.Unit");
export const char_type = new TypeInfo("System.Char");
export const string_type = new TypeInfo("System.String");
export const bool_type = new TypeInfo("System.Boolean");
export const int8_type = new TypeInfo("System.SByte");
export const uint8_type = new TypeInfo("System.Byte");
export const int16_type = new TypeInfo("System.Int16");
export const uint16_type = new TypeInfo("System.UInt16");
export const int32_type = new TypeInfo("System.Int32");
export const uint32_type = new TypeInfo("System.UInt32");
export const int64_type = new TypeInfo("System.Int64");
export const uint64_type = new TypeInfo("System.UInt64");
export const int128_type = new TypeInfo("System.Int128");
export const uint128_type = new TypeInfo("System.UInt128");
export const nativeint_type = new TypeInfo("System.IntPtr");
export const unativeint_type = new TypeInfo("System.UIntPtr");
export const float16_type = new TypeInfo("System.Half");
export const float32_type = new TypeInfo("System.Single");
export const float64_type = new TypeInfo("System.Double");
export const decimal_type = new TypeInfo("System.Decimal");
export const bigint_type = new TypeInfo("System.Numerics.BigInteger");
export function name(info) {
    if (Array.isArray(info)) {
        return info[0];
    }
    else if (info instanceof TypeInfo) {
        const elemType = getElementType(info);
        if (elemType != null) {
            return name(elemType) + "[]";
        }
        else {
            const i = info.fullname.lastIndexOf(".");
            return i === -1 ? info.fullname : info.fullname.substr(i + 1);
        }
    }
    else {
        return info.name;
    }
}
export function fullName(t) {
    const elemType = getElementType(t);
    if (elemType != null) {
        return fullName(elemType) + "[]";
    }
    else if (t.generics == null || t.generics.length === 0) {
        return t.fullname;
    }
    else {
        return t.fullname + "[" + t.generics.map((x) => fullName(x)).join(",") + "]";
    }
}
export function namespace(t) {
    const elemType = getElementType(t);
    if (elemType != null) {
        return namespace(elemType);
    }
    else {
        const i = t.fullname.lastIndexOf(".");
        return i === -1 ? "" : t.fullname.substr(0, i);
    }
}
export function isArray(t) {
    return getElementType(t) != null;
}
export function getElementType(t) {
    return t.fullname === "[]" && t.generics?.length === 1 ? t.generics[0] : undefined;
}
export function isGenericType(t) {
    return t.generics != null && t.generics.length > 0;
}
export function isGenericParameter(t) {
    return t instanceof GenericParameter;
}
export function isEnum(t) {
    return t.enumCases != null && t.enumCases.length > 0;
}
export function isSubclassOf(t1, t2) {
    return (t2.fullname === obj_type.fullname) || (t1.parent != null && (t1.parent.Equals(t2) || isSubclassOf(t1.parent, t2)));
}
function isErasedToNumber(t) {
    return isEnum(t) || [
        int8_type.fullname,
        uint8_type.fullname,
        int16_type.fullname,
        uint16_type.fullname,
        int32_type.fullname,
        uint32_type.fullname,
        float16_type.fullname,
        float32_type.fullname,
        float64_type.fullname,
    ].includes(t.fullname);
}
function isErasedToBigInt(t) {
    return isEnum(t) || [
        int64_type.fullname,
        uint64_type.fullname,
        int128_type.fullname,
        uint128_type.fullname,
        nativeint_type.fullname,
        unativeint_type.fullname,
        bigint_type.fullname,
    ].includes(t.fullname);
}
export function isInstanceOfType(t, o) {
    if (t.fullname === obj_type.fullname)
        return true;
    switch (typeof o) {
        case "boolean":
            return t.fullname === bool_type.fullname;
        case "string":
            return t.fullname === string_type.fullname;
        case "function":
            return isFunction(t);
        case "number":
            return isErasedToNumber(t);
        case "bigint":
            return isErasedToBigInt(t);
        default:
            return t.construct != null && o instanceof t.construct;
    }
}
/**
 * This doesn't replace types for fields (records) or cases (unions)
 * but it should be enough for type comparison purposes
 */
export function getGenericTypeDefinition(t) {
    return t.generics == null ? t : new TypeInfo(t.fullname, t.generics.map(() => obj_type));
}
export function getEnumUnderlyingType(t) {
    return t.generics?.[0];
}
export function getEnumValues(t) {
    if (isEnum(t) && t.enumCases != null) {
        return t.enumCases.map((kv) => kv[1]);
    }
    else {
        throw new Error(`${t.fullname} is not an enum type`);
    }
}
export function getEnumNames(t) {
    if (isEnum(t) && t.enumCases != null) {
        return t.enumCases.map((kv) => kv[0]);
    }
    else {
        throw new Error(`${t.fullname} is not an enum type`);
    }
}
function getEnumCase(t, v) {
    if (t.enumCases != null) {
        if (typeof v === "string") {
            for (const kv of t.enumCases) {
                if (kv[0] === v) {
                    return kv;
                }
            }
            throw new Error(`'${v}' was not found in ${t.fullname}`);
        }
        else {
            for (const kv of t.enumCases) {
                if (kv[1] === v) {
                    return kv;
                }
            }
            // .NET returns the number even if it doesn't match any of the cases
            return ["", v];
        }
    }
    else {
        throw new Error(`${t.fullname} is not an enum type`);
    }
}
export function parseEnum(t, str) {
    // TODO: better int parsing here, parseInt ceils floats: "4.8" -> 4
    const value = parseInt(str, 10);
    return getEnumCase(t, isNaN(value) ? str : value)[1];
}
export function tryParseEnum(t, str, defValue) {
    try {
        defValue.contents = parseEnum(t, str);
        return true;
    }
    catch {
        return false;
    }
}
export function getEnumName(t, v) {
    return getEnumCase(t, v)[0];
}
export function isEnumDefined(t, v) {
    try {
        const kv = getEnumCase(t, v);
        return kv[0] != null && kv[0] !== "";
    }
    catch {
        // supress error
    }
    return false;
}
// FSharpType
export function getUnionCases(t) {
    if (t.cases != null) {
        return t.cases();
    }
    else {
        throw new Error(`${t.fullname} is not an F# union type`);
    }
}
export function getRecordElements(t) {
    if (t.fields != null) {
        return t.fields();
    }
    else {
        throw new Error(`${t.fullname} is not an F# record type`);
    }
}
export function getTupleElements(t) {
    if (isTuple(t) && t.generics != null) {
        return t.generics;
    }
    else {
        throw new Error(`${t.fullname} is not a tuple type`);
    }
}
export function getFunctionElements(t) {
    if (isFunction(t) && t.generics != null) {
        const gen = t.generics;
        return [gen[0], gen[1]];
    }
    else {
        throw new Error(`${t.fullname} is not an F# function type`);
    }
}
export function isUnion(t) {
    return t instanceof TypeInfo ? t.cases != null : t instanceof Union;
}
export function isRecord(t) {
    return t instanceof TypeInfo ? t.fields != null : t instanceof Record;
}
export function isTuple(t) {
    return t.fullname.startsWith("System.Tuple");
}
// In .NET this is false for delegates
export function isFunction(t) {
    return t.fullname === "Microsoft.FSharp.Core.FSharpFunc`2";
}
// FSharpValue
export function getUnionFields(v, t) {
    const cases = getUnionCases(t);
    const case_ = cases[v.tag];
    if (case_ == null) {
        throw new Error(`Cannot find case ${v.name} in union type`);
    }
    return [case_, v.fields];
}
export function getUnionCaseFields(uci) {
    return uci.fields == null ? [] : uci.fields;
}
// This is used as replacement of `FSharpValue.GetRecordFields`
// For `FSharpTypes.GetRecordFields` see `getRecordElements`
// Object.keys returns keys in the order they were added to the object
export function getRecordFields(v) {
    return Object.keys(v).map((k) => v[k]);
}
export function getRecordField(v, field) {
    return v[field[0]];
}
export function getTupleFields(v) {
    return v;
}
export function getTupleField(v, i) {
    return v[i];
}
export function makeUnion(uci, values) {
    const expectedLength = (uci.fields || []).length;
    if (values.length !== expectedLength) {
        throw new Error(`Expected an array of length ${expectedLength} but got ${values.length}`);
    }
    const construct = uci.declaringType.construct;
    if (construct == null) {
        return {};
    }
    const isSingleCase = uci.declaringType.cases ? uci.declaringType.cases().length == 1 : false;
    if (isSingleCase) {
        return new construct(...values);
    }
    else {
        return new construct(uci.tag, values);
    }
}
export function makeRecord(t, values) {
    const fields = getRecordElements(t);
    if (fields.length !== values.length) {
        throw new Error(`Expected an array of length ${fields.length} but got ${values.length}`);
    }
    return t.construct != null
        ? new t.construct(...values)
        : fields.reduce((obj, [key, _t], i) => {
            obj[key] = values[i];
            return obj;
        }, {});
}
export function makeTuple(values, _t) {
    return values;
}
export function makeGenericType(t, generics) {
    return new TypeInfo(t.fullname, generics, t.construct, t.parent, t.fields, t.cases);
}
export function createInstance(t, consArgs) {
    // TODO: Check if consArgs length is same as t.construct?
    // (Arg types can still be different)
    if (typeof t.construct === "function") {
        return new t.construct(...(consArgs ?? []));
    }
    else if (isErasedToNumber(t)) {
        return 0;
    }
    else if (isErasedToBigInt(t)) {
        return 0n;
    }
    else {
        switch (t.fullname) {
            case obj_type.fullname:
                return {};
            case bool_type.fullname:
                return false;
            case decimal_type.fullname:
                return new Decimal(0);
            case char_type.fullname:
                // Even though char is a value type, it's erased to string, and Unchecked.defaultof<char> is null
                return null;
            default:
                throw new Error(`Cannot access constructor of ${t.fullname}`);
        }
    }
}
export function getValue(propertyInfo, v) {
    return v[propertyInfo[0]];
}
// Fable.Core.Reflection
function assertUnion(x) {
    if (!(x instanceof Union)) {
        throw new Error(`Value is not an F# union type`);
    }
}
export function getCaseTag(x) {
    assertUnion(x);
    return x.tag;
}
export function getCaseName(x) {
    assertUnion(x);
    return x.cases()[x.tag];
}
export function getCaseFields(x) {
    assertUnion(x);
    return x.fields;
}
