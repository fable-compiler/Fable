import { FSharpRef } from "./Types.js";
import { IEquatable } from "./Util.js";
export type FieldInfo = [string, TypeInfo];
export type PropertyInfo = FieldInfo;
export type ParameterInfo = FieldInfo;
export type Constructor = new (...args: any[]) => any;
export declare class CaseInfo {
    declaringType: TypeInfo;
    tag: number;
    name: string;
    fields?: FieldInfo[] | undefined;
    constructor(declaringType: TypeInfo, tag: number, name: string, fields?: FieldInfo[] | undefined);
}
export type EnumCase = [string, number];
export declare class MethodInfo {
    name: string;
    parameters: ParameterInfo[];
    returnType: TypeInfo;
    constructor(name: string, parameters: ParameterInfo[], returnType: TypeInfo);
}
export declare class TypeInfo implements IEquatable<TypeInfo> {
    fullname: string;
    generics?: TypeInfo[] | undefined;
    construct?: Constructor | undefined;
    parent?: TypeInfo | undefined;
    fields?: (() => FieldInfo[]) | undefined;
    cases?: (() => CaseInfo[]) | undefined;
    enumCases?: EnumCase[] | undefined;
    constructor(fullname: string, generics?: TypeInfo[] | undefined, construct?: Constructor | undefined, parent?: TypeInfo | undefined, fields?: (() => FieldInfo[]) | undefined, cases?: (() => CaseInfo[]) | undefined, enumCases?: EnumCase[] | undefined);
    toString(): string;
    GetHashCode(): number;
    Equals(other: TypeInfo): boolean;
}
export declare class GenericParameter extends TypeInfo {
    constructor(name: string);
}
export declare function getGenerics(t: TypeInfo): TypeInfo[];
export declare function getHashCode(t: TypeInfo): number;
export declare function equals(t1: TypeInfo, t2: TypeInfo): boolean;
export declare function class_type(fullname: string, generics?: TypeInfo[], construct?: Constructor, parent?: TypeInfo): TypeInfo;
export declare function record_type(fullname: string, generics: TypeInfo[], construct: Constructor, fields: () => FieldInfo[]): TypeInfo;
export declare function anonRecord_type(...fields: FieldInfo[]): TypeInfo;
export declare function union_type(fullname: string, generics: TypeInfo[], construct: Constructor, cases: () => FieldInfo[][]): TypeInfo;
export declare function tuple_type(...generics: TypeInfo[]): TypeInfo;
export declare function delegate_type(...generics: TypeInfo[]): TypeInfo;
export declare function lambda_type(argType: TypeInfo, returnType: TypeInfo): TypeInfo;
export declare function option_type(generic: TypeInfo): TypeInfo;
export declare function list_type(generic: TypeInfo): TypeInfo;
export declare function array_type(generic: TypeInfo): TypeInfo;
export declare function enum_type(fullname: string, underlyingType: TypeInfo, enumCases: EnumCase[]): TypeInfo;
export declare function measure_type(fullname: string): TypeInfo;
export declare function generic_type(name: string): TypeInfo;
export declare const obj_type: TypeInfo;
export declare const unit_type: TypeInfo;
export declare const char_type: TypeInfo;
export declare const string_type: TypeInfo;
export declare const bool_type: TypeInfo;
export declare const int8_type: TypeInfo;
export declare const uint8_type: TypeInfo;
export declare const int16_type: TypeInfo;
export declare const uint16_type: TypeInfo;
export declare const int32_type: TypeInfo;
export declare const uint32_type: TypeInfo;
export declare const int64_type: TypeInfo;
export declare const uint64_type: TypeInfo;
export declare const int128_type: TypeInfo;
export declare const uint128_type: TypeInfo;
export declare const nativeint_type: TypeInfo;
export declare const unativeint_type: TypeInfo;
export declare const float16_type: TypeInfo;
export declare const float32_type: TypeInfo;
export declare const float64_type: TypeInfo;
export declare const decimal_type: TypeInfo;
export declare const bigint_type: TypeInfo;
export declare function name(info: FieldInfo | TypeInfo | CaseInfo | MethodInfo): string;
export declare function fullName(t: TypeInfo): string;
export declare function namespace(t: TypeInfo): string;
export declare function isArray(t: TypeInfo): boolean;
export declare function getElementType(t: TypeInfo): TypeInfo | undefined;
export declare function isGenericType(t: TypeInfo): boolean;
export declare function isGenericParameter(t: TypeInfo): boolean;
export declare function isEnum(t: TypeInfo): boolean;
export declare function isSubclassOf(t1: TypeInfo, t2: TypeInfo): boolean;
export declare function isInstanceOfType(t: TypeInfo, o: any): boolean;
/**
 * This doesn't replace types for fields (records) or cases (unions)
 * but it should be enough for type comparison purposes
 */
export declare function getGenericTypeDefinition(t: TypeInfo): TypeInfo;
export declare function getEnumUnderlyingType(t: TypeInfo): TypeInfo | undefined;
export declare function getEnumValues(t: TypeInfo): number[];
export declare function getEnumNames(t: TypeInfo): string[];
export declare function parseEnum(t: TypeInfo, str: string): number;
export declare function tryParseEnum(t: TypeInfo, str: string, defValue: FSharpRef<number>): boolean;
export declare function getEnumName(t: TypeInfo, v: number): string;
export declare function isEnumDefined(t: TypeInfo, v: string | number): boolean;
export declare function getUnionCases(t: TypeInfo): CaseInfo[];
export declare function getRecordElements(t: TypeInfo): FieldInfo[];
export declare function getTupleElements(t: TypeInfo): TypeInfo[];
export declare function getFunctionElements(t: TypeInfo): [TypeInfo, TypeInfo];
export declare function isUnion(t: any): boolean;
export declare function isRecord(t: any): boolean;
export declare function isTuple(t: TypeInfo): boolean;
export declare function isFunction(t: TypeInfo): boolean;
export declare function getUnionFields(v: any, t: TypeInfo): [CaseInfo, any[]];
export declare function getUnionCaseFields(uci: CaseInfo): FieldInfo[];
export declare function getRecordFields(v: any): any[];
export declare function getRecordField(v: any, field: FieldInfo): any;
export declare function getTupleFields(v: any): any[];
export declare function getTupleField(v: any, i: number): any;
export declare function makeUnion(uci: CaseInfo, values: any[]): any;
export declare function makeRecord(t: TypeInfo, values: any[]): any;
export declare function makeTuple(values: any[], _t: TypeInfo): any;
export declare function makeGenericType(t: TypeInfo, generics: TypeInfo[]): TypeInfo;
export declare function createInstance(t: TypeInfo, consArgs?: any[]): any;
export declare function getValue(propertyInfo: PropertyInfo, v: any): any;
export declare function getCaseTag(x: any): number;
export declare function getCaseName(x: any): string;
export declare function getCaseFields(x: any): any[];
