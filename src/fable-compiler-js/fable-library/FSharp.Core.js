import { disposeSafe, defaultOf, structuralHash, equals } from "./Util.js";
import { HashIdentity_Structural, ComparisonIdentity_Structural } from "./FSharp.Collections.js";
import { StringBuilder__Append_Z721C83C5 } from "./System.Text.js";
export const LanguagePrimitives_GenericEqualityComparer = {
    "System.Collections.IEqualityComparer.Equals541DA560"(x, y) {
        return equals(x, y);
    },
    "System.Collections.IEqualityComparer.GetHashCode4E60E31B"(x_1) {
        return structuralHash(x_1);
    },
};
export const LanguagePrimitives_GenericEqualityERComparer = {
    "System.Collections.IEqualityComparer.Equals541DA560"(x, y) {
        return equals(x, y);
    },
    "System.Collections.IEqualityComparer.GetHashCode4E60E31B"(x_1) {
        return structuralHash(x_1);
    },
};
export function LanguagePrimitives_FastGenericComparer() {
    return ComparisonIdentity_Structural();
}
export function LanguagePrimitives_FastGenericComparerFromTable() {
    return ComparisonIdentity_Structural();
}
export function LanguagePrimitives_FastGenericEqualityComparer() {
    return HashIdentity_Structural();
}
export function LanguagePrimitives_FastGenericEqualityComparerFromTable() {
    return HashIdentity_Structural();
}
export function Operators_Failure(message) {
    return new Error(message);
}
export function Operators_FailurePattern(exn) {
    return exn.message;
}
export function Operators_NullArg(x) {
    throw new Error(x);
}
export function Operators_Using(resource, action) {
    try {
        return action(resource);
    }
    finally {
        if (equals(resource, defaultOf())) {
        }
        else {
            let copyOfStruct = resource;
            disposeSafe(copyOfStruct);
        }
    }
}
export function Operators_Lock(_lockObj, action) {
    return action();
}
export function ExtraTopLevelOperators_LazyPattern(input) {
    return input.Value;
}
export function PrintfModule_PrintFormatToStringBuilderThen(continuation, builder, format) {
    return format.cont((s) => {
        StringBuilder__Append_Z721C83C5(builder, s);
        return continuation();
    });
}
export function PrintfModule_PrintFormatToStringBuilder(builder, format) {
    return PrintfModule_PrintFormatToStringBuilderThen(() => {
    }, builder, format);
}
