import { compare, physicalHash, equals, structuralHash } from "./Util.js";
export function HashIdentity_FromFunctions(hash, eq) {
    return {
        Equals(x, y) {
            return eq(x, y);
        },
        GetHashCode(x_1) {
            return hash(x_1);
        },
    };
}
export function HashIdentity_Structural() {
    return HashIdentity_FromFunctions(structuralHash, equals);
}
export function HashIdentity_Reference() {
    return HashIdentity_FromFunctions(physicalHash, (e, e_1) => (e === e_1));
}
export function ComparisonIdentity_FromFunction(comparer) {
    return {
        Compare(x, y) {
            return comparer(x, y);
        },
    };
}
export function ComparisonIdentity_Structural() {
    return ComparisonIdentity_FromFunction(compare);
}
