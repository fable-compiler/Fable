import { Union } from "./Types.js";
import { TypeInfo } from "./Reflection.js";
import { Option } from "./Option.js";
export type FSharpResult$2_$union<T, TError> = FSharpResult$2<T, TError, 0> | FSharpResult$2<T, TError, 1>;
export type FSharpResult$2_$cases<T, TError> = {
    0: ["Ok", [T]];
    1: ["Error", [TError]];
};
export declare function FSharpResult$2_Ok<T, TError>(ResultValue: T): FSharpResult$2<T, TError, 0>;
export declare function FSharpResult$2_Error<T, TError>(ErrorValue: TError): FSharpResult$2<T, TError, 1>;
export declare class FSharpResult$2<T, TError, Tag extends keyof FSharpResult$2_$cases<T, TError>> extends Union<Tag, FSharpResult$2_$cases<T, TError>[Tag][0]> {
    readonly tag: Tag;
    readonly fields: FSharpResult$2_$cases<T, TError>[Tag][1];
    constructor(tag: Tag, fields: FSharpResult$2_$cases<T, TError>[Tag][1]);
    cases(): string[];
}
export declare function FSharpResult$2_$reflection(gen0: TypeInfo, gen1: TypeInfo): TypeInfo;
export declare function Result_Map<a, b, c>(mapping: ((arg0: a) => b), result: FSharpResult$2_$union<a, c>): FSharpResult$2_$union<b, c>;
export declare function Result_MapError<a, b, c>(mapping: ((arg0: a) => b), result: FSharpResult$2_$union<c, a>): FSharpResult$2_$union<c, b>;
export declare function Result_Bind<a, b, c>(binder: ((arg0: a) => FSharpResult$2_$union<b, c>), result: FSharpResult$2_$union<a, c>): FSharpResult$2_$union<b, c>;
export type FSharpChoice$2_$union<T1, T2> = FSharpChoice$2<T1, T2, 0> | FSharpChoice$2<T1, T2, 1>;
export type FSharpChoice$2_$cases<T1, T2> = {
    0: ["Choice1Of2", [T1]];
    1: ["Choice2Of2", [T2]];
};
export declare function FSharpChoice$2_Choice1Of2<T1, T2>(Item: T1): FSharpChoice$2<T1, T2, 0>;
export declare function FSharpChoice$2_Choice2Of2<T1, T2>(Item: T2): FSharpChoice$2<T1, T2, 1>;
export declare class FSharpChoice$2<T1, T2, Tag extends keyof FSharpChoice$2_$cases<T1, T2>> extends Union<Tag, FSharpChoice$2_$cases<T1, T2>[Tag][0]> {
    readonly tag: Tag;
    readonly fields: FSharpChoice$2_$cases<T1, T2>[Tag][1];
    constructor(tag: Tag, fields: FSharpChoice$2_$cases<T1, T2>[Tag][1]);
    cases(): string[];
}
export declare function FSharpChoice$2_$reflection(gen0: TypeInfo, gen1: TypeInfo): TypeInfo;
export type FSharpChoice$3_$union<T1, T2, T3> = FSharpChoice$3<T1, T2, T3, 0> | FSharpChoice$3<T1, T2, T3, 1> | FSharpChoice$3<T1, T2, T3, 2>;
export type FSharpChoice$3_$cases<T1, T2, T3> = {
    0: ["Choice1Of3", [T1]];
    1: ["Choice2Of3", [T2]];
    2: ["Choice3Of3", [T3]];
};
export declare function FSharpChoice$3_Choice1Of3<T1, T2, T3>(Item: T1): FSharpChoice$3<T1, T2, T3, 0>;
export declare function FSharpChoice$3_Choice2Of3<T1, T2, T3>(Item: T2): FSharpChoice$3<T1, T2, T3, 1>;
export declare function FSharpChoice$3_Choice3Of3<T1, T2, T3>(Item: T3): FSharpChoice$3<T1, T2, T3, 2>;
export declare class FSharpChoice$3<T1, T2, T3, Tag extends keyof FSharpChoice$3_$cases<T1, T2, T3>> extends Union<Tag, FSharpChoice$3_$cases<T1, T2, T3>[Tag][0]> {
    readonly tag: Tag;
    readonly fields: FSharpChoice$3_$cases<T1, T2, T3>[Tag][1];
    constructor(tag: Tag, fields: FSharpChoice$3_$cases<T1, T2, T3>[Tag][1]);
    cases(): string[];
}
export declare function FSharpChoice$3_$reflection(gen0: TypeInfo, gen1: TypeInfo, gen2: TypeInfo): TypeInfo;
export type FSharpChoice$4_$union<T1, T2, T3, T4> = FSharpChoice$4<T1, T2, T3, T4, 0> | FSharpChoice$4<T1, T2, T3, T4, 1> | FSharpChoice$4<T1, T2, T3, T4, 2> | FSharpChoice$4<T1, T2, T3, T4, 3>;
export type FSharpChoice$4_$cases<T1, T2, T3, T4> = {
    0: ["Choice1Of4", [T1]];
    1: ["Choice2Of4", [T2]];
    2: ["Choice3Of4", [T3]];
    3: ["Choice4Of4", [T4]];
};
export declare function FSharpChoice$4_Choice1Of4<T1, T2, T3, T4>(Item: T1): FSharpChoice$4<T1, T2, T3, T4, 0>;
export declare function FSharpChoice$4_Choice2Of4<T1, T2, T3, T4>(Item: T2): FSharpChoice$4<T1, T2, T3, T4, 1>;
export declare function FSharpChoice$4_Choice3Of4<T1, T2, T3, T4>(Item: T3): FSharpChoice$4<T1, T2, T3, T4, 2>;
export declare function FSharpChoice$4_Choice4Of4<T1, T2, T3, T4>(Item: T4): FSharpChoice$4<T1, T2, T3, T4, 3>;
export declare class FSharpChoice$4<T1, T2, T3, T4, Tag extends keyof FSharpChoice$4_$cases<T1, T2, T3, T4>> extends Union<Tag, FSharpChoice$4_$cases<T1, T2, T3, T4>[Tag][0]> {
    readonly tag: Tag;
    readonly fields: FSharpChoice$4_$cases<T1, T2, T3, T4>[Tag][1];
    constructor(tag: Tag, fields: FSharpChoice$4_$cases<T1, T2, T3, T4>[Tag][1]);
    cases(): string[];
}
export declare function FSharpChoice$4_$reflection(gen0: TypeInfo, gen1: TypeInfo, gen2: TypeInfo, gen3: TypeInfo): TypeInfo;
export type FSharpChoice$5_$union<T1, T2, T3, T4, T5> = FSharpChoice$5<T1, T2, T3, T4, T5, 0> | FSharpChoice$5<T1, T2, T3, T4, T5, 1> | FSharpChoice$5<T1, T2, T3, T4, T5, 2> | FSharpChoice$5<T1, T2, T3, T4, T5, 3> | FSharpChoice$5<T1, T2, T3, T4, T5, 4>;
export type FSharpChoice$5_$cases<T1, T2, T3, T4, T5> = {
    0: ["Choice1Of5", [T1]];
    1: ["Choice2Of5", [T2]];
    2: ["Choice3Of5", [T3]];
    3: ["Choice4Of5", [T4]];
    4: ["Choice5Of5", [T5]];
};
export declare function FSharpChoice$5_Choice1Of5<T1, T2, T3, T4, T5>(Item: T1): FSharpChoice$5<T1, T2, T3, T4, T5, 0>;
export declare function FSharpChoice$5_Choice2Of5<T1, T2, T3, T4, T5>(Item: T2): FSharpChoice$5<T1, T2, T3, T4, T5, 1>;
export declare function FSharpChoice$5_Choice3Of5<T1, T2, T3, T4, T5>(Item: T3): FSharpChoice$5<T1, T2, T3, T4, T5, 2>;
export declare function FSharpChoice$5_Choice4Of5<T1, T2, T3, T4, T5>(Item: T4): FSharpChoice$5<T1, T2, T3, T4, T5, 3>;
export declare function FSharpChoice$5_Choice5Of5<T1, T2, T3, T4, T5>(Item: T5): FSharpChoice$5<T1, T2, T3, T4, T5, 4>;
export declare class FSharpChoice$5<T1, T2, T3, T4, T5, Tag extends keyof FSharpChoice$5_$cases<T1, T2, T3, T4, T5>> extends Union<Tag, FSharpChoice$5_$cases<T1, T2, T3, T4, T5>[Tag][0]> {
    readonly tag: Tag;
    readonly fields: FSharpChoice$5_$cases<T1, T2, T3, T4, T5>[Tag][1];
    constructor(tag: Tag, fields: FSharpChoice$5_$cases<T1, T2, T3, T4, T5>[Tag][1]);
    cases(): string[];
}
export declare function FSharpChoice$5_$reflection(gen0: TypeInfo, gen1: TypeInfo, gen2: TypeInfo, gen3: TypeInfo, gen4: TypeInfo): TypeInfo;
export type FSharpChoice$6_$union<T1, T2, T3, T4, T5, T6> = FSharpChoice$6<T1, T2, T3, T4, T5, T6, 0> | FSharpChoice$6<T1, T2, T3, T4, T5, T6, 1> | FSharpChoice$6<T1, T2, T3, T4, T5, T6, 2> | FSharpChoice$6<T1, T2, T3, T4, T5, T6, 3> | FSharpChoice$6<T1, T2, T3, T4, T5, T6, 4> | FSharpChoice$6<T1, T2, T3, T4, T5, T6, 5>;
export type FSharpChoice$6_$cases<T1, T2, T3, T4, T5, T6> = {
    0: ["Choice1Of6", [T1]];
    1: ["Choice2Of6", [T2]];
    2: ["Choice3Of6", [T3]];
    3: ["Choice4Of6", [T4]];
    4: ["Choice5Of6", [T5]];
    5: ["Choice6Of6", [T6]];
};
export declare function FSharpChoice$6_Choice1Of6<T1, T2, T3, T4, T5, T6>(Item: T1): FSharpChoice$6<T1, T2, T3, T4, T5, T6, 0>;
export declare function FSharpChoice$6_Choice2Of6<T1, T2, T3, T4, T5, T6>(Item: T2): FSharpChoice$6<T1, T2, T3, T4, T5, T6, 1>;
export declare function FSharpChoice$6_Choice3Of6<T1, T2, T3, T4, T5, T6>(Item: T3): FSharpChoice$6<T1, T2, T3, T4, T5, T6, 2>;
export declare function FSharpChoice$6_Choice4Of6<T1, T2, T3, T4, T5, T6>(Item: T4): FSharpChoice$6<T1, T2, T3, T4, T5, T6, 3>;
export declare function FSharpChoice$6_Choice5Of6<T1, T2, T3, T4, T5, T6>(Item: T5): FSharpChoice$6<T1, T2, T3, T4, T5, T6, 4>;
export declare function FSharpChoice$6_Choice6Of6<T1, T2, T3, T4, T5, T6>(Item: T6): FSharpChoice$6<T1, T2, T3, T4, T5, T6, 5>;
export declare class FSharpChoice$6<T1, T2, T3, T4, T5, T6, Tag extends keyof FSharpChoice$6_$cases<T1, T2, T3, T4, T5, T6>> extends Union<Tag, FSharpChoice$6_$cases<T1, T2, T3, T4, T5, T6>[Tag][0]> {
    readonly tag: Tag;
    readonly fields: FSharpChoice$6_$cases<T1, T2, T3, T4, T5, T6>[Tag][1];
    constructor(tag: Tag, fields: FSharpChoice$6_$cases<T1, T2, T3, T4, T5, T6>[Tag][1]);
    cases(): string[];
}
export declare function FSharpChoice$6_$reflection(gen0: TypeInfo, gen1: TypeInfo, gen2: TypeInfo, gen3: TypeInfo, gen4: TypeInfo, gen5: TypeInfo): TypeInfo;
export type FSharpChoice$7_$union<T1, T2, T3, T4, T5, T6, T7> = FSharpChoice$7<T1, T2, T3, T4, T5, T6, T7, 0> | FSharpChoice$7<T1, T2, T3, T4, T5, T6, T7, 1> | FSharpChoice$7<T1, T2, T3, T4, T5, T6, T7, 2> | FSharpChoice$7<T1, T2, T3, T4, T5, T6, T7, 3> | FSharpChoice$7<T1, T2, T3, T4, T5, T6, T7, 4> | FSharpChoice$7<T1, T2, T3, T4, T5, T6, T7, 5> | FSharpChoice$7<T1, T2, T3, T4, T5, T6, T7, 6>;
export type FSharpChoice$7_$cases<T1, T2, T3, T4, T5, T6, T7> = {
    0: ["Choice1Of7", [T1]];
    1: ["Choice2Of7", [T2]];
    2: ["Choice3Of7", [T3]];
    3: ["Choice4Of7", [T4]];
    4: ["Choice5Of7", [T5]];
    5: ["Choice6Of7", [T6]];
    6: ["Choice7Of7", [T7]];
};
export declare function FSharpChoice$7_Choice1Of7<T1, T2, T3, T4, T5, T6, T7>(Item: T1): FSharpChoice$7<T1, T2, T3, T4, T5, T6, T7, 0>;
export declare function FSharpChoice$7_Choice2Of7<T1, T2, T3, T4, T5, T6, T7>(Item: T2): FSharpChoice$7<T1, T2, T3, T4, T5, T6, T7, 1>;
export declare function FSharpChoice$7_Choice3Of7<T1, T2, T3, T4, T5, T6, T7>(Item: T3): FSharpChoice$7<T1, T2, T3, T4, T5, T6, T7, 2>;
export declare function FSharpChoice$7_Choice4Of7<T1, T2, T3, T4, T5, T6, T7>(Item: T4): FSharpChoice$7<T1, T2, T3, T4, T5, T6, T7, 3>;
export declare function FSharpChoice$7_Choice5Of7<T1, T2, T3, T4, T5, T6, T7>(Item: T5): FSharpChoice$7<T1, T2, T3, T4, T5, T6, T7, 4>;
export declare function FSharpChoice$7_Choice6Of7<T1, T2, T3, T4, T5, T6, T7>(Item: T6): FSharpChoice$7<T1, T2, T3, T4, T5, T6, T7, 5>;
export declare function FSharpChoice$7_Choice7Of7<T1, T2, T3, T4, T5, T6, T7>(Item: T7): FSharpChoice$7<T1, T2, T3, T4, T5, T6, T7, 6>;
export declare class FSharpChoice$7<T1, T2, T3, T4, T5, T6, T7, Tag extends keyof FSharpChoice$7_$cases<T1, T2, T3, T4, T5, T6, T7>> extends Union<Tag, FSharpChoice$7_$cases<T1, T2, T3, T4, T5, T6, T7>[Tag][0]> {
    readonly tag: Tag;
    readonly fields: FSharpChoice$7_$cases<T1, T2, T3, T4, T5, T6, T7>[Tag][1];
    constructor(tag: Tag, fields: FSharpChoice$7_$cases<T1, T2, T3, T4, T5, T6, T7>[Tag][1]);
    cases(): string[];
}
export declare function FSharpChoice$7_$reflection(gen0: TypeInfo, gen1: TypeInfo, gen2: TypeInfo, gen3: TypeInfo, gen4: TypeInfo, gen5: TypeInfo, gen6: TypeInfo): TypeInfo;
export declare function Choice_makeChoice1Of2<T1, a>(x: T1): FSharpChoice$2_$union<T1, a>;
export declare function Choice_makeChoice2Of2<T2, a>(x: T2): FSharpChoice$2_$union<a, T2>;
export declare function Choice_tryValueIfChoice1Of2<T1, T2>(x: FSharpChoice$2_$union<T1, T2>): Option<T1>;
export declare function Choice_tryValueIfChoice2Of2<T1, T2>(x: FSharpChoice$2_$union<T1, T2>): Option<T2>;
