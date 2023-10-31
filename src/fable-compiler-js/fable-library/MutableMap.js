import { disposeSafe, defaultOf, equals, toIterator, getEnumerator } from "./Util.js";
import { iterate, map, delay, toArray, iterateIndexed, concat } from "./Seq.js";
import { value as value_1 } from "./Option.js";
import { FSharpRef } from "./Types.js";
import { class_type } from "./Reflection.js";
import { getItemFromDict, tryGetValue } from "./MapUtil.js";
import { format } from "./String.js";
export class Dictionary {
    constructor(pairs, comparer) {
        const this$ = new FSharpRef(defaultOf());
        this.comparer = comparer;
        this$.contents = this;
        this.hashMap = (new Map([]));
        this["init@9"] = 1;
        const enumerator = getEnumerator(pairs);
        try {
            while (enumerator["System.Collections.IEnumerator.MoveNext"]()) {
                const pair = enumerator["System.Collections.Generic.IEnumerator`1.get_Current"]();
                Dictionary__Add_5BDDA1(this$.contents, pair[0], pair[1]);
            }
        }
        finally {
            disposeSafe(enumerator);
        }
    }
    get [Symbol.toStringTag]() {
        return "Dictionary";
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
        Dictionary__Add_5BDDA1(this$, item[0], item[1]);
    }
    "System.Collections.Generic.ICollection`1.Clear"() {
        const this$ = this;
        Dictionary__Clear(this$);
    }
    "System.Collections.Generic.ICollection`1.Contains2B595"(item) {
        const this$ = this;
        const matchValue = Dictionary__TryFind_2B595(this$, item[0]);
        let matchResult, p_1;
        if (matchValue != null) {
            if (equals(value_1(matchValue)[1], item[1])) {
                matchResult = 0;
                p_1 = value_1(matchValue);
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
    "System.Collections.Generic.ICollection`1.CopyToZ3B4C077E"(array, arrayIndex) {
        const this$ = this;
        iterateIndexed((i, e) => {
            array[arrayIndex + i] = e;
        }, this$);
    }
    "System.Collections.Generic.ICollection`1.get_Count"() {
        const this$ = this;
        return Dictionary__get_Count(this$) | 0;
    }
    "System.Collections.Generic.ICollection`1.get_IsReadOnly"() {
        return false;
    }
    "System.Collections.Generic.ICollection`1.Remove2B595"(item) {
        const this$ = this;
        const matchValue = Dictionary__TryFind_2B595(this$, item[0]);
        if (matchValue != null) {
            if (equals(value_1(matchValue)[1], item[1])) {
                Dictionary__Remove_2B595(this$, item[0]);
            }
            return true;
        }
        else {
            return false;
        }
    }
    "System.Collections.Generic.IDictionary`2.Add5BDDA1"(key, value) {
        const this$ = this;
        Dictionary__Add_5BDDA1(this$, key, value);
    }
    "System.Collections.Generic.IDictionary`2.ContainsKey2B595"(key) {
        const this$ = this;
        return Dictionary__ContainsKey_2B595(this$, key);
    }
    "System.Collections.Generic.IDictionary`2.get_Item2B595"(key) {
        const this$ = this;
        return Dictionary__get_Item_2B595(this$, key);
    }
    "System.Collections.Generic.IDictionary`2.set_Item5BDDA1"(key, v) {
        const this$ = this;
        Dictionary__set_Item_5BDDA1(this$, key, v);
    }
    "System.Collections.Generic.IDictionary`2.get_Keys"() {
        const this$ = this;
        return toArray(delay(() => map((pair) => pair[0], this$)));
    }
    "System.Collections.Generic.IDictionary`2.Remove2B595"(key) {
        const this$ = this;
        return Dictionary__Remove_2B595(this$, key);
    }
    "System.Collections.Generic.IDictionary`2.TryGetValue6DC89625"(key, value) {
        const this$ = this;
        const matchValue = Dictionary__TryFind_2B595(this$, key);
        if (matchValue != null) {
            const pair = value_1(matchValue);
            value.contents = pair[1];
            return true;
        }
        else {
            return false;
        }
    }
    "System.Collections.Generic.IDictionary`2.get_Values"() {
        const this$ = this;
        return toArray(delay(() => map((pair) => pair[1], this$)));
    }
    get size() {
        const this$ = this;
        return Dictionary__get_Count(this$) | 0;
    }
    clear() {
        const this$ = this;
        Dictionary__Clear(this$);
    }
    delete(k) {
        const this$ = this;
        return Dictionary__Remove_2B595(this$, k);
    }
    entries() {
        const this$ = this;
        return map((p) => [p[0], p[1]], this$);
    }
    get(k) {
        const this$ = this;
        return Dictionary__get_Item_2B595(this$, k);
    }
    has(k) {
        const this$ = this;
        return Dictionary__ContainsKey_2B595(this$, k);
    }
    keys() {
        const this$ = this;
        return map((p) => p[0], this$);
    }
    set(k, v) {
        const this$ = this;
        Dictionary__set_Item_5BDDA1(this$, k, v);
        return this$;
    }
    values() {
        const this$ = this;
        return map((p) => p[1], this$);
    }
    forEach(f, thisArg) {
        const this$ = this;
        iterate((p) => {
            f(p[1], p[0], this$);
        }, this$);
    }
}
export function Dictionary_$reflection(gen0, gen1) {
    return class_type("Fable.Collections.Dictionary", [gen0, gen1], Dictionary);
}
export function Dictionary_$ctor_6623D9B3(pairs, comparer) {
    return new Dictionary(pairs, comparer);
}
function Dictionary__TryFindIndex_2B595(this$, k) {
    const h = this$.comparer.GetHashCode(k) | 0;
    let matchValue;
    let outArg = defaultOf();
    matchValue = [tryGetValue(this$.hashMap, h, new FSharpRef(() => outArg, (v) => {
            outArg = v;
        })), outArg];
    if (matchValue[0]) {
        return [true, h, matchValue[1].findIndex((pair) => this$.comparer.Equals(k, pair[0]))];
    }
    else {
        return [false, h, -1];
    }
}
export function Dictionary__TryFind_2B595(this$, k) {
    const matchValue = Dictionary__TryFindIndex_2B595(this$, k);
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
            return getItemFromDict(this$.hashMap, matchValue[1])[matchValue[2]];
        default:
            return void 0;
    }
}
export function Dictionary__get_Comparer(this$) {
    return this$.comparer;
}
export function Dictionary__Clear(this$) {
    this$.hashMap.clear();
}
export function Dictionary__get_Count(this$) {
    let count = 0;
    let enumerator = getEnumerator(this$.hashMap.values());
    try {
        while (enumerator["System.Collections.IEnumerator.MoveNext"]()) {
            const pairs = enumerator["System.Collections.Generic.IEnumerator`1.get_Current"]();
            count = ((count + pairs.length) | 0);
        }
    }
    finally {
        disposeSafe(enumerator);
    }
    return count | 0;
}
export function Dictionary__get_Item_2B595(this$, k) {
    const matchValue = Dictionary__TryFind_2B595(this$, k);
    if (matchValue != null) {
        return value_1(matchValue)[1];
    }
    else {
        throw new Error("The item was not found in collection");
    }
}
export function Dictionary__set_Item_5BDDA1(this$, k, v) {
    const matchValue = Dictionary__TryFindIndex_2B595(this$, k);
    if (matchValue[0]) {
        if (matchValue[2] > -1) {
            getItemFromDict(this$.hashMap, matchValue[1])[matchValue[2]] = [k, v];
        }
        else {
            const value = void (getItemFromDict(this$.hashMap, matchValue[1]).push([k, v]));
        }
    }
    else {
        this$.hashMap.set(matchValue[1], [[k, v]]);
    }
}
export function Dictionary__Add_5BDDA1(this$, k, v) {
    const matchValue = Dictionary__TryFindIndex_2B595(this$, k);
    if (matchValue[0]) {
        if (matchValue[2] > -1) {
            throw new Error(format("An item with the same key has already been added. Key: {0}", k));
        }
        else {
            const value = void (getItemFromDict(this$.hashMap, matchValue[1]).push([k, v]));
        }
    }
    else {
        this$.hashMap.set(matchValue[1], [[k, v]]);
    }
}
export function Dictionary__ContainsKey_2B595(this$, k) {
    const matchValue = Dictionary__TryFindIndex_2B595(this$, k);
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
export function Dictionary__Remove_2B595(this$, k) {
    const matchValue = Dictionary__TryFindIndex_2B595(this$, k);
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
