import { int32 } from "./Int32.js";
import { IComparer, IEqualityComparer } from "./Util.js";
export declare function HashIdentity_FromFunctions<T>(hash: ((arg0: T) => int32), eq: ((arg0: T, arg1: T) => boolean)): IEqualityComparer<T>;
export declare function HashIdentity_Structural<T>(): IEqualityComparer<T>;
export declare function HashIdentity_Reference<T>(): IEqualityComparer<T>;
export declare function ComparisonIdentity_FromFunction<T>(comparer: ((arg0: T, arg1: T) => int32)): IComparer<T>;
export declare function ComparisonIdentity_Structural<T>(): IComparer<T>;
