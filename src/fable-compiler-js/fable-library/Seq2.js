import { toList, toArray, map, filter, delay } from "./Seq.js";
import { HashSet } from "./MutableSet.js";
import { defaultOf, disposeSafe, getEnumerator } from "./Util.js";
import { addToDict, getItemFromDict, tryGetValue, addToSet } from "./MapUtil.js";
import { Dictionary } from "./MutableMap.js";
import { FSharpRef } from "./Types.js";
export function distinct(xs, comparer) {
    return delay(() => {
        const hashSet = new HashSet([], comparer);
        return filter((x) => addToSet(x, hashSet), xs);
    });
}
export function distinctBy(projection, xs, comparer) {
    return delay(() => {
        const hashSet = new HashSet([], comparer);
        return filter((x) => addToSet(projection(x), hashSet), xs);
    });
}
export function except(itemsToExclude, xs, comparer) {
    return delay(() => {
        const hashSet = new HashSet(itemsToExclude, comparer);
        return filter((x) => addToSet(x, hashSet), xs);
    });
}
export function countBy(projection, xs, comparer) {
    return delay(() => {
        const dict = new Dictionary([], comparer);
        const keys = [];
        const enumerator = getEnumerator(xs);
        try {
            while (enumerator["System.Collections.IEnumerator.MoveNext"]()) {
                const key = projection(enumerator["System.Collections.Generic.IEnumerator`1.get_Current"]());
                let matchValue;
                let outArg = 0;
                matchValue = [tryGetValue(dict, key, new FSharpRef(() => outArg, (v) => {
                        outArg = (v | 0);
                    })), outArg];
                if (matchValue[0]) {
                    dict.set(key, matchValue[1] + 1);
                }
                else {
                    dict.set(key, 1);
                    void (keys.push(key));
                }
            }
        }
        finally {
            disposeSafe(enumerator);
        }
        return map((key_1) => [key_1, getItemFromDict(dict, key_1)], keys);
    });
}
export function groupBy(projection, xs, comparer) {
    return delay(() => {
        const dict = new Dictionary([], comparer);
        const keys = [];
        const enumerator = getEnumerator(xs);
        try {
            while (enumerator["System.Collections.IEnumerator.MoveNext"]()) {
                const x = enumerator["System.Collections.Generic.IEnumerator`1.get_Current"]();
                const key = projection(x);
                let matchValue;
                let outArg = defaultOf();
                matchValue = [tryGetValue(dict, key, new FSharpRef(() => outArg, (v) => {
                        outArg = v;
                    })), outArg];
                if (matchValue[0]) {
                    void (matchValue[1].push(x));
                }
                else {
                    addToDict(dict, key, [x]);
                    void (keys.push(key));
                }
            }
        }
        finally {
            disposeSafe(enumerator);
        }
        return map((key_1) => [key_1, getItemFromDict(dict, key_1)], keys);
    });
}
export function Array_distinct(xs, comparer) {
    return toArray(distinct(xs, comparer));
}
export function Array_distinctBy(projection, xs, comparer) {
    return toArray(distinctBy(projection, xs, comparer));
}
export function Array_except(itemsToExclude, xs, comparer) {
    return toArray(except(itemsToExclude, xs, comparer));
}
export function Array_countBy(projection, xs, comparer) {
    return toArray(countBy(projection, xs, comparer));
}
export function Array_groupBy(projection, xs, comparer) {
    return toArray(map((tupledArg) => [tupledArg[0], toArray(tupledArg[1])], groupBy(projection, xs, comparer)));
}
export function List_distinct(xs, comparer) {
    return toList(distinct(xs, comparer));
}
export function List_distinctBy(projection, xs, comparer) {
    return toList(distinctBy(projection, xs, comparer));
}
export function List_except(itemsToExclude, xs, comparer) {
    return toList(except(itemsToExclude, xs, comparer));
}
export function List_countBy(projection, xs, comparer) {
    return toList(countBy(projection, xs, comparer));
}
export function List_groupBy(projection, xs, comparer) {
    return toList(map((tupledArg) => [tupledArg[0], toList(tupledArg[1])], groupBy(projection, xs, comparer)));
}
