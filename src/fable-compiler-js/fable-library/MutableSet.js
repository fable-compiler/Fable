import { disposeSafe, defaultOf, toIterator, getEnumerator } from "./Util.js";
import { iterate, map, iterateIndexed, concat } from "./Seq.js";
import { some } from "./Option.js";
import { FSharpRef } from "./Types.js";
import { class_type } from "./Reflection.js";
import { getItemFromDict, tryGetValue } from "./MapUtil.js";
export class HashSet {
    constructor(items, comparer) {
        const this$ = new FSharpRef(defaultOf());
        this.comparer = comparer;
        this$.contents = this;
        this.hashMap = (new Map([]));
        this["init@9"] = 1;
        const enumerator = getEnumerator(items);
        try {
            while (enumerator["System.Collections.IEnumerator.MoveNext"]()) {
                const item = enumerator["System.Collections.Generic.IEnumerator`1.get_Current"]();
                HashSet__Add_2B595(this$.contents, item);
            }
        }
        finally {
            disposeSafe(enumerator);
        }
    }
    get [Symbol.toStringTag]() {
        return "HashSet";
    }
    toJSON() {
        const this$ = this;
        return Array.from(this$);
    }
    "System.Collections.IEnumerable.GetEnumerator"() {
        const this$ = this;
        return getEnumerator(this$);
    }
    GetEnumerator() {
        const this$ = this;
        return getEnumerator(concat(this$.hashMap.values()));
    }
    [Symbol.iterator]() {
        return toIterator(getEnumerator(this));
    }
    "System.Collections.Generic.ICollection`1.Add2B595"(item) {
        const this$ = this;
        HashSet__Add_2B595(this$, item);
    }
    "System.Collections.Generic.ICollection`1.Clear"() {
        const this$ = this;
        HashSet__Clear(this$);
    }
    "System.Collections.Generic.ICollection`1.Contains2B595"(item) {
        const this$ = this;
        return HashSet__Contains_2B595(this$, item);
    }
    "System.Collections.Generic.ICollection`1.CopyToZ3B4C077E"(array, arrayIndex) {
        const this$ = this;
        iterateIndexed((i, e) => {
            array[arrayIndex + i] = e;
        }, this$);
    }
    "System.Collections.Generic.ICollection`1.get_Count"() {
        const this$ = this;
        return HashSet__get_Count(this$) | 0;
    }
    "System.Collections.Generic.ICollection`1.get_IsReadOnly"() {
        return false;
    }
    "System.Collections.Generic.ICollection`1.Remove2B595"(item) {
        const this$ = this;
        return HashSet__Remove_2B595(this$, item);
    }
    get size() {
        const this$ = this;
        return HashSet__get_Count(this$) | 0;
    }
    add(k) {
        const this$ = this;
        HashSet__Add_2B595(this$, k);
        return this$;
    }
    clear() {
        const this$ = this;
        HashSet__Clear(this$);
    }
    delete(k) {
        const this$ = this;
        return HashSet__Remove_2B595(this$, k);
    }
    has(k) {
        const this$ = this;
        return HashSet__Contains_2B595(this$, k);
    }
    keys() {
        const this$ = this;
        return map((x) => x, this$);
    }
    values() {
        const this$ = this;
        return map((x) => x, this$);
    }
    entries() {
        const this$ = this;
        return map((v) => [v, v], this$);
    }
    forEach(f, thisArg) {
        const this$ = this;
        iterate((x) => {
            f(x, x, this$);
        }, this$);
    }
}
export function HashSet_$reflection(gen0) {
    return class_type("Fable.Collections.HashSet", [gen0], HashSet);
}
export function HashSet_$ctor_Z6150332D(items, comparer) {
    return new HashSet(items, comparer);
}
function HashSet__TryFindIndex_2B595(this$, k) {
    const h = this$.comparer.GetHashCode(k) | 0;
    let matchValue;
    let outArg = defaultOf();
    matchValue = [tryGetValue(this$.hashMap, h, new FSharpRef(() => outArg, (v) => {
            outArg = v;
        })), outArg];
    if (matchValue[0]) {
        return [true, h, matchValue[1].findIndex((v_1) => this$.comparer.Equals(k, v_1))];
    }
    else {
        return [false, h, -1];
    }
}
function HashSet__TryFind_2B595(this$, k) {
    const matchValue = HashSet__TryFindIndex_2B595(this$, k);
    let matchResult;
    if (matchValue[0]) {
        if (matchValue[2] > -1) {
            matchResult = 0;
        }
        else {
            matchResult = 1;
        }
    }
    else {
        matchResult = 1;
    }
    switch (matchResult) {
        case 0:
            return some(getItemFromDict(this$.hashMap, matchValue[1])[matchValue[2]]);
        default:
            return void 0;
    }
}
export function HashSet__get_Comparer(this$) {
    return this$.comparer;
}
export function HashSet__Clear(this$) {
    this$.hashMap.clear();
}
export function HashSet__get_Count(this$) {
    let count = 0;
    let enumerator = getEnumerator(this$.hashMap.values());
    try {
        while (enumerator["System.Collections.IEnumerator.MoveNext"]()) {
            const items = enumerator["System.Collections.Generic.IEnumerator`1.get_Current"]();
            count = ((count + items.length) | 0);
        }
    }
    finally {
        disposeSafe(enumerator);
    }
    return count | 0;
}
export function HashSet__Add_2B595(this$, k) {
    const matchValue = HashSet__TryFindIndex_2B595(this$, k);
    if (matchValue[0]) {
        if (matchValue[2] > -1) {
            return false;
        }
        else {
            const value = void (getItemFromDict(this$.hashMap, matchValue[1]).push(k));
            return true;
        }
    }
    else {
        this$.hashMap.set(matchValue[1], [k]);
        return true;
    }
}
export function HashSet__Contains_2B595(this$, k) {
    const matchValue = HashSet__TryFindIndex_2B595(this$, k);
    let matchResult;
    if (matchValue[0]) {
        if (matchValue[2] > -1) {
            matchResult = 0;
        }
        else {
            matchResult = 1;
        }
    }
    else {
        matchResult = 1;
    }
    switch (matchResult) {
        case 0:
            return true;
        default:
            return false;
    }
}
export function HashSet__Remove_2B595(this$, k) {
    const matchValue = HashSet__TryFindIndex_2B595(this$, k);
    let matchResult;
    if (matchValue[0]) {
        if (matchValue[2] > -1) {
            matchResult = 0;
        }
        else {
            matchResult = 1;
        }
    }
    else {
        matchResult = 1;
    }
    switch (matchResult) {
        case 0: {
            getItemFromDict(this$.hashMap, matchValue[1]).splice(matchValue[2], 1);
            return true;
        }
        default:
            return false;
    }
}
