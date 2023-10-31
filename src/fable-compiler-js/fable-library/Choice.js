import { Union } from "./Types.js";
import { union_type } from "./Reflection.js";
import { some } from "./Option.js";
export function FSharpResult$2_Ok(ResultValue) {
    return new FSharpResult$2(0, [ResultValue]);
}
export function FSharpResult$2_Error(ErrorValue) {
    return new FSharpResult$2(1, [ErrorValue]);
}
export class FSharpResult$2 extends Union {
    constructor(tag, fields) {
        super();
        this.tag = tag;
        this.fields = fields;
    }
    cases() {
        return ["Ok", "Error"];
    }
}
export function FSharpResult$2_$reflection(gen0, gen1) {
    return union_type("FSharp.Core.FSharpResult`2", [gen0, gen1], FSharpResult$2, () => [[["ResultValue", gen0]], [["ErrorValue", gen1]]]);
}
export function Result_Map(mapping, result) {
    if (result.tag === /* Ok */ 0) {
        return FSharpResult$2_Ok(mapping(result.fields[0]));
    }
    else {
        return FSharpResult$2_Error(result.fields[0]);
    }
}
export function Result_MapError(mapping, result) {
    if (result.tag === /* Ok */ 0) {
        return FSharpResult$2_Ok(result.fields[0]);
    }
    else {
        return FSharpResult$2_Error(mapping(result.fields[0]));
    }
}
export function Result_Bind(binder, result) {
    if (result.tag === /* Ok */ 0) {
        return binder(result.fields[0]);
    }
    else {
        return FSharpResult$2_Error(result.fields[0]);
    }
}
export function FSharpChoice$2_Choice1Of2(Item) {
    return new FSharpChoice$2(0, [Item]);
}
export function FSharpChoice$2_Choice2Of2(Item) {
    return new FSharpChoice$2(1, [Item]);
}
export class FSharpChoice$2 extends Union {
    constructor(tag, fields) {
        super();
        this.tag = tag;
        this.fields = fields;
    }
    cases() {
        return ["Choice1Of2", "Choice2Of2"];
    }
}
export function FSharpChoice$2_$reflection(gen0, gen1) {
    return union_type("FSharp.Core.FSharpChoice`2", [gen0, gen1], FSharpChoice$2, () => [[["Item", gen0]], [["Item", gen1]]]);
}
export function FSharpChoice$3_Choice1Of3(Item) {
    return new FSharpChoice$3(0, [Item]);
}
export function FSharpChoice$3_Choice2Of3(Item) {
    return new FSharpChoice$3(1, [Item]);
}
export function FSharpChoice$3_Choice3Of3(Item) {
    return new FSharpChoice$3(2, [Item]);
}
export class FSharpChoice$3 extends Union {
    constructor(tag, fields) {
        super();
        this.tag = tag;
        this.fields = fields;
    }
    cases() {
        return ["Choice1Of3", "Choice2Of3", "Choice3Of3"];
    }
}
export function FSharpChoice$3_$reflection(gen0, gen1, gen2) {
    return union_type("FSharp.Core.FSharpChoice`3", [gen0, gen1, gen2], FSharpChoice$3, () => [[["Item", gen0]], [["Item", gen1]], [["Item", gen2]]]);
}
export function FSharpChoice$4_Choice1Of4(Item) {
    return new FSharpChoice$4(0, [Item]);
}
export function FSharpChoice$4_Choice2Of4(Item) {
    return new FSharpChoice$4(1, [Item]);
}
export function FSharpChoice$4_Choice3Of4(Item) {
    return new FSharpChoice$4(2, [Item]);
}
export function FSharpChoice$4_Choice4Of4(Item) {
    return new FSharpChoice$4(3, [Item]);
}
export class FSharpChoice$4 extends Union {
    constructor(tag, fields) {
        super();
        this.tag = tag;
        this.fields = fields;
    }
    cases() {
        return ["Choice1Of4", "Choice2Of4", "Choice3Of4", "Choice4Of4"];
    }
}
export function FSharpChoice$4_$reflection(gen0, gen1, gen2, gen3) {
    return union_type("FSharp.Core.FSharpChoice`4", [gen0, gen1, gen2, gen3], FSharpChoice$4, () => [[["Item", gen0]], [["Item", gen1]], [["Item", gen2]], [["Item", gen3]]]);
}
export function FSharpChoice$5_Choice1Of5(Item) {
    return new FSharpChoice$5(0, [Item]);
}
export function FSharpChoice$5_Choice2Of5(Item) {
    return new FSharpChoice$5(1, [Item]);
}
export function FSharpChoice$5_Choice3Of5(Item) {
    return new FSharpChoice$5(2, [Item]);
}
export function FSharpChoice$5_Choice4Of5(Item) {
    return new FSharpChoice$5(3, [Item]);
}
export function FSharpChoice$5_Choice5Of5(Item) {
    return new FSharpChoice$5(4, [Item]);
}
export class FSharpChoice$5 extends Union {
    constructor(tag, fields) {
        super();
        this.tag = tag;
        this.fields = fields;
    }
    cases() {
        return ["Choice1Of5", "Choice2Of5", "Choice3Of5", "Choice4Of5", "Choice5Of5"];
    }
}
export function FSharpChoice$5_$reflection(gen0, gen1, gen2, gen3, gen4) {
    return union_type("FSharp.Core.FSharpChoice`5", [gen0, gen1, gen2, gen3, gen4], FSharpChoice$5, () => [[["Item", gen0]], [["Item", gen1]], [["Item", gen2]], [["Item", gen3]], [["Item", gen4]]]);
}
export function FSharpChoice$6_Choice1Of6(Item) {
    return new FSharpChoice$6(0, [Item]);
}
export function FSharpChoice$6_Choice2Of6(Item) {
    return new FSharpChoice$6(1, [Item]);
}
export function FSharpChoice$6_Choice3Of6(Item) {
    return new FSharpChoice$6(2, [Item]);
}
export function FSharpChoice$6_Choice4Of6(Item) {
    return new FSharpChoice$6(3, [Item]);
}
export function FSharpChoice$6_Choice5Of6(Item) {
    return new FSharpChoice$6(4, [Item]);
}
export function FSharpChoice$6_Choice6Of6(Item) {
    return new FSharpChoice$6(5, [Item]);
}
export class FSharpChoice$6 extends Union {
    constructor(tag, fields) {
        super();
        this.tag = tag;
        this.fields = fields;
    }
    cases() {
        return ["Choice1Of6", "Choice2Of6", "Choice3Of6", "Choice4Of6", "Choice5Of6", "Choice6Of6"];
    }
}
export function FSharpChoice$6_$reflection(gen0, gen1, gen2, gen3, gen4, gen5) {
    return union_type("FSharp.Core.FSharpChoice`6", [gen0, gen1, gen2, gen3, gen4, gen5], FSharpChoice$6, () => [[["Item", gen0]], [["Item", gen1]], [["Item", gen2]], [["Item", gen3]], [["Item", gen4]], [["Item", gen5]]]);
}
export function FSharpChoice$7_Choice1Of7(Item) {
    return new FSharpChoice$7(0, [Item]);
}
export function FSharpChoice$7_Choice2Of7(Item) {
    return new FSharpChoice$7(1, [Item]);
}
export function FSharpChoice$7_Choice3Of7(Item) {
    return new FSharpChoice$7(2, [Item]);
}
export function FSharpChoice$7_Choice4Of7(Item) {
    return new FSharpChoice$7(3, [Item]);
}
export function FSharpChoice$7_Choice5Of7(Item) {
    return new FSharpChoice$7(4, [Item]);
}
export function FSharpChoice$7_Choice6Of7(Item) {
    return new FSharpChoice$7(5, [Item]);
}
export function FSharpChoice$7_Choice7Of7(Item) {
    return new FSharpChoice$7(6, [Item]);
}
export class FSharpChoice$7 extends Union {
    constructor(tag, fields) {
        super();
        this.tag = tag;
        this.fields = fields;
    }
    cases() {
        return ["Choice1Of7", "Choice2Of7", "Choice3Of7", "Choice4Of7", "Choice5Of7", "Choice6Of7", "Choice7Of7"];
    }
}
export function FSharpChoice$7_$reflection(gen0, gen1, gen2, gen3, gen4, gen5, gen6) {
    return union_type("FSharp.Core.FSharpChoice`7", [gen0, gen1, gen2, gen3, gen4, gen5, gen6], FSharpChoice$7, () => [[["Item", gen0]], [["Item", gen1]], [["Item", gen2]], [["Item", gen3]], [["Item", gen4]], [["Item", gen5]], [["Item", gen6]]]);
}
export function Choice_makeChoice1Of2(x) {
    return FSharpChoice$2_Choice1Of2(x);
}
export function Choice_makeChoice2Of2(x) {
    return FSharpChoice$2_Choice2Of2(x);
}
export function Choice_tryValueIfChoice1Of2(x) {
    if (x.tag === /* Choice1Of2 */ 0) {
        return some(x.fields[0]);
    }
    else {
        return void 0;
    }
}
export function Choice_tryValueIfChoice2Of2(x) {
    if (x.tag === /* Choice2Of2 */ 1) {
        return some(x.fields[0]);
    }
    else {
        return void 0;
    }
}
