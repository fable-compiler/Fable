import { IEqualityComparer } from "./Util";

declare module "./Map" {
  export function createMutable<K, V>(
    source: ReadonlyArray<readonly [K, V]> | null,
    comparer: IEqualityComparer<K>): Map<K, V>;
}
