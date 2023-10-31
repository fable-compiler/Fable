import { IMap, ISet } from "./Util.js";
import { FSharpRef } from "./Types.js";
export declare function keyValueList(fields: Iterable<any>, caseRule?: number): {
    [k: string]: any;
};
export declare function containsValue<K, V>(v: V, map: IMap<K, V>): boolean;
export declare function tryGetValue<K, V>(map: IMap<K, V>, key: K, defaultValue: FSharpRef<V>): boolean;
export declare function addToSet<T>(v: T, set: ISet<T>): boolean;
export declare function addToDict<K, V>(dict: IMap<K, V>, k: K, v: V): void;
export declare function getItemFromDict<K, V>(map: IMap<K, V>, key: K): V;
