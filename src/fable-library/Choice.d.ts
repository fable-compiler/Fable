import { int32 } from "./Int32.js";
import { Option } from "./Option.js";
import { Union } from "./Types.js";
import { TypeInfo } from "./Reflection.js";

declare class FSharpChoice$2<T1, T2> extends Union {
    tag: int32;
    fields: Array<any>;
    constructor(tag: int32, ...fields: Array<any>);
    cases(): string[];
}

declare function FSharpChoice$2$reflection(gen0: TypeInfo, gen1: TypeInfo): TypeInfo;
declare function Choice_makeChoice1Of2<T1, a$>(x: T1): FSharpChoice$2<T1, a$>;
declare function Choice_makeChoice2Of2<T2, a$>(x: T2): FSharpChoice$2<a$, T2>;
declare function Choice_tryValueIfChoice1Of2<T1, T2>(x: FSharpChoice$2<T1, T2>): Option<T1>;
declare function Choice_tryValueIfChoice2Of2<T1, T2>(x: FSharpChoice$2<T1, T2>): Option<T2>;
