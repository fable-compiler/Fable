import { Option } from "./Option.js";

declare type FSharpChoice$2_$union<T1, T2> = never;

declare function Choice_makeChoice1Of2<T1, a$>(x: T1): FSharpChoice$2_$union<T1, a$>;
declare function Choice_makeChoice2Of2<T2, a$>(x: T2): FSharpChoice$2_$union<a$, T2>;
declare function Choice_tryValueIfChoice1Of2<T1, T2>(x: FSharpChoice$2_$union<T1, T2>): Option<T1>;
declare function Choice_tryValueIfChoice2Of2<T1, T2>(x: FSharpChoice$2_$union<T1, T2>): Option<T2>;
