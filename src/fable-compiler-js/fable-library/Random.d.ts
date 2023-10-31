import { uint8, int32, float64 } from "./Int32.js";
import { TypeInfo } from "./Reflection.js";
export interface IRandom {
    Next0(): int32;
    Next1(maxValue: int32): int32;
    Next2(minValue: int32, maxValue: int32): int32;
    NextBytes(buffer: uint8[]): void;
    NextDouble(): float64;
}
export declare class NonSeeded implements IRandom {
    constructor();
    Next0(): int32;
    Next1(maxValue: int32): int32;
    Next2(minValue: int32, maxValue: int32): int32;
    NextDouble(): float64;
    NextBytes(buffer: uint8[]): void;
}
export declare function NonSeeded_$reflection(): TypeInfo;
export declare function NonSeeded_$ctor(): NonSeeded;
export declare class Seeded implements IRandom {
    readonly MBIG: int32;
    inext: int32;
    inextp: int32;
    seedArray: int32[];
    constructor(seed: int32);
    Next0(): int32;
    Next1(maxValue: int32): int32;
    Next2(minValue: int32, maxValue: int32): int32;
    NextDouble(): float64;
    NextBytes(buffer: uint8[]): void;
}
export declare function Seeded_$reflection(): TypeInfo;
export declare function Seeded_$ctor_Z524259A4(seed: int32): Seeded;
export declare function Seeded__Sample(this$: Seeded): float64;
export declare function Seeded__GetSampleForLargeRange(this$: Seeded): float64;
export declare function nonSeeded(): NonSeeded;
export declare function seeded(seed: int32): Seeded;
