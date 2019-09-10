import { IEqualityComparer } from "./Util";

declare module "./Set" {
  export function createMutable<T>(
    source: ReadonlyArray<T> | null,
    comparer: IEqualityComparer<T>): Set<T>;
}
