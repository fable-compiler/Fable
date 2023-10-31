import { Helpers_allocateArrayFromCons } from "./Native.js";
import { value as value_2, map as map_1, defaultArg, some } from "./Option.js";
import { min as min_1, max as max_1 } from "./Double.js";
import { equals as equals_1, disposeSafe, getEnumerator, copyToArray, defaultOf } from "./Util.js";
import { SR_indexOutOfBounds } from "./Global.js";
function indexNotFound() {
    throw new Error("An index satisfying the predicate was not found in the collection.");
}
function differentLengths() {
    throw new Error("Arrays had different lengths");
}
export function append(array1, array2, cons) {
    const len1 = array1.length | 0;
    const len2 = array2.length | 0;
    const newArray = Helpers_allocateArrayFromCons(cons, len1 + len2);
    for (let i = 0; i <= (len1 - 1); i++) {
        newArray[i] = array1[i];
    }
    for (let i_1 = 0; i_1 <= (len2 - 1); i_1++) {
        newArray[i_1 + len1] = array2[i_1];
    }
    return newArray;
}
export function filter(predicate, array) {
    return array.filter(predicate);
}
export function fill(target, targetIndex, count, value) {
    const start = targetIndex | 0;
    return target.fill(value, start, (start + count));
}
export function getSubArray(array, start, count) {
    const start_1 = start | 0;
    return array.slice(start_1, (start_1 + count));
}
export function last(array) {
    if (array.length === 0) {
        throw new Error("The input array was empty\\nParameter name: array");
    }
    return array[array.length - 1];
}
export function tryLast(array) {
    if (array.length === 0) {
        return void 0;
    }
    else {
        return some(array[array.length - 1]);
    }
}
export function mapIndexed(f, source, cons) {
    const len = source.length | 0;
    const target = Helpers_allocateArrayFromCons(cons, len);
    for (let i = 0; i <= (len - 1); i++) {
        target[i] = f(i, source[i]);
    }
    return target;
}
export function map(f, source, cons) {
    const len = source.length | 0;
    const target = Helpers_allocateArrayFromCons(cons, len);
    for (let i = 0; i <= (len - 1); i++) {
        target[i] = f(source[i]);
    }
    return target;
}
export function mapIndexed2(f, source1, source2, cons) {
    if (source1.length !== source2.length) {
        throw new Error("Arrays had different lengths");
    }
    const result = Helpers_allocateArrayFromCons(cons, source1.length);
    for (let i = 0; i <= (source1.length - 1); i++) {
        result[i] = f(i, source1[i], source2[i]);
    }
    return result;
}
export function map2(f, source1, source2, cons) {
    if (source1.length !== source2.length) {
        throw new Error("Arrays had different lengths");
    }
    const result = Helpers_allocateArrayFromCons(cons, source1.length);
    for (let i = 0; i <= (source1.length - 1); i++) {
        result[i] = f(source1[i], source2[i]);
    }
    return result;
}
export function mapIndexed3(f, source1, source2, source3, cons) {
    if ((source1.length !== source2.length) ? true : (source2.length !== source3.length)) {
        throw new Error("Arrays had different lengths");
    }
    const result = Helpers_allocateArrayFromCons(cons, source1.length);
    for (let i = 0; i <= (source1.length - 1); i++) {
        result[i] = f(i, source1[i], source2[i], source3[i]);
    }
    return result;
}
export function map3(f, source1, source2, source3, cons) {
    if ((source1.length !== source2.length) ? true : (source2.length !== source3.length)) {
        throw new Error("Arrays had different lengths");
    }
    const result = Helpers_allocateArrayFromCons(cons, source1.length);
    for (let i = 0; i <= (source1.length - 1); i++) {
        result[i] = f(source1[i], source2[i], source3[i]);
    }
    return result;
}
export function mapFold(mapping, state, array, cons) {
    const matchValue = array.length | 0;
    if (matchValue === 0) {
        return [[], state];
    }
    else {
        let acc = state;
        const res = Helpers_allocateArrayFromCons(cons, matchValue);
        for (let i = 0; i <= (array.length - 1); i++) {
            const patternInput = mapping(acc, array[i]);
            res[i] = patternInput[0];
            acc = patternInput[1];
        }
        return [res, acc];
    }
}
export function mapFoldBack(mapping, array, state, cons) {
    const matchValue = array.length | 0;
    if (matchValue === 0) {
        return [[], state];
    }
    else {
        let acc = state;
        const res = Helpers_allocateArrayFromCons(cons, matchValue);
        for (let i = array.length - 1; i >= 0; i--) {
            const patternInput = mapping(array[i], acc);
            res[i] = patternInput[0];
            acc = patternInput[1];
        }
        return [res, acc];
    }
}
export function indexed(source) {
    const len = source.length | 0;
    const target = new Array(len);
    for (let i = 0; i <= (len - 1); i++) {
        target[i] = [i, source[i]];
    }
    return target;
}
export function truncate(count, array) {
    const count_1 = max_1(0, count) | 0;
    return array.slice(0, (0 + count_1));
}
export function concat(arrays, cons) {
    const arrays_1 = Array.isArray(arrays) ? arrays : (Array.from(arrays));
    const matchValue = arrays_1.length | 0;
    switch (matchValue) {
        case 0:
            return Helpers_allocateArrayFromCons(cons, 0);
        case 1:
            return arrays_1[0];
        default: {
            let totalIdx = 0;
            let totalLength = 0;
            for (let idx = 0; idx <= (arrays_1.length - 1); idx++) {
                const arr_1 = arrays_1[idx];
                totalLength = ((totalLength + arr_1.length) | 0);
            }
            const result = Helpers_allocateArrayFromCons(cons, totalLength);
            for (let idx_1 = 0; idx_1 <= (arrays_1.length - 1); idx_1++) {
                const arr_2 = arrays_1[idx_1];
                for (let j = 0; j <= (arr_2.length - 1); j++) {
                    result[totalIdx] = arr_2[j];
                    totalIdx = ((totalIdx + 1) | 0);
                }
            }
            return result;
        }
    }
}
export function collect(mapping, array, cons) {
    return concat(map(mapping, array, defaultOf()), cons);
}
export function where(predicate, array) {
    return array.filter(predicate);
}
export function indexOf(array, item_1, start, count, eq) {
    const start_1 = defaultArg(start, 0) | 0;
    const end$0027 = defaultArg(map_1((c) => (start_1 + c), count), array.length) | 0;
    const loop = (i_mut) => {
        loop: while (true) {
            const i = i_mut;
            if (i >= end$0027) {
                return -1;
            }
            else if (eq.Equals(item_1, array[i])) {
                return i | 0;
            }
            else {
                i_mut = (i + 1);
                continue loop;
            }
            break;
        }
    };
    return loop(start_1) | 0;
}
export function contains(value, array, eq) {
    return indexOf(array, value, void 0, void 0, eq) >= 0;
}
export function empty(cons) {
    return Helpers_allocateArrayFromCons(cons, 0);
}
export function singleton(value, cons) {
    const ar = Helpers_allocateArrayFromCons(cons, 1);
    ar[0] = value;
    return ar;
}
export function initialize(count, initializer, cons) {
    if (count < 0) {
        throw new Error("The input must be non-negative\\nParameter name: count");
    }
    const result = Helpers_allocateArrayFromCons(cons, count);
    for (let i = 0; i <= (count - 1); i++) {
        result[i] = initializer(i);
    }
    return result;
}
export function pairwise(array) {
    if (array.length < 2) {
        return [];
    }
    else {
        const count = (array.length - 1) | 0;
        const result = new Array(count);
        for (let i = 0; i <= (count - 1); i++) {
            result[i] = [array[i], array[i + 1]];
        }
        return result;
    }
}
export function replicate(count, initial, cons) {
    if (count < 0) {
        throw new Error("The input must be non-negative\\nParameter name: count");
    }
    const result = Helpers_allocateArrayFromCons(cons, count);
    for (let i = 0; i <= (result.length - 1); i++) {
        result[i] = initial;
    }
    return result;
}
export function copy(array) {
    return array.slice();
}
export function copyTo(source, sourceIndex, target, targetIndex, count) {
    copyToArray(source, sourceIndex, target, targetIndex, count);
}
export function reverse(array) {
    const array_2 = array.slice();
    return array_2.reverse();
}
export function scan(folder, state, array, cons) {
    const res = Helpers_allocateArrayFromCons(cons, array.length + 1);
    res[0] = state;
    for (let i = 0; i <= (array.length - 1); i++) {
        res[i + 1] = folder(res[i], array[i]);
    }
    return res;
}
export function scanBack(folder, array, state, cons) {
    const res = Helpers_allocateArrayFromCons(cons, array.length + 1);
    res[array.length] = state;
    for (let i = array.length - 1; i >= 0; i--) {
        res[i] = folder(array[i], res[i + 1]);
    }
    return res;
}
export function skip(count, array, cons) {
    if (count > array.length) {
        throw new Error("count is greater than array length\\nParameter name: count");
    }
    if (count === array.length) {
        return Helpers_allocateArrayFromCons(cons, 0);
    }
    else {
        const count_1 = ((count < 0) ? 0 : count) | 0;
        return array.slice(count_1);
    }
}
export function skipWhile(predicate, array, cons) {
    let count = 0;
    while ((count < array.length) && predicate(array[count])) {
        count = ((count + 1) | 0);
    }
    if (count === array.length) {
        return Helpers_allocateArrayFromCons(cons, 0);
    }
    else {
        const count_1 = count | 0;
        return array.slice(count_1);
    }
}
export function take(count, array, cons) {
    if (count < 0) {
        throw new Error("The input must be non-negative\\nParameter name: count");
    }
    if (count > array.length) {
        throw new Error("count is greater than array length\\nParameter name: count");
    }
    if (count === 0) {
        return Helpers_allocateArrayFromCons(cons, 0);
    }
    else {
        return array.slice(0, (0 + count));
    }
}
export function takeWhile(predicate, array, cons) {
    let count = 0;
    while ((count < array.length) && predicate(array[count])) {
        count = ((count + 1) | 0);
    }
    if (count === 0) {
        return Helpers_allocateArrayFromCons(cons, 0);
    }
    else {
        const count_1 = count | 0;
        return array.slice(0, (0 + count_1));
    }
}
export function addInPlace(x, array) {
    array.push(x);
}
export function addRangeInPlace(range, array) {
    const enumerator = getEnumerator(range);
    try {
        while (enumerator["System.Collections.IEnumerator.MoveNext"]()) {
            addInPlace(enumerator["System.Collections.Generic.IEnumerator`1.get_Current"](), array);
        }
    }
    finally {
        disposeSafe(enumerator);
    }
}
export function insertRangeInPlace(index, range, array) {
    let index_1;
    let i = index;
    const enumerator = getEnumerator(range);
    try {
        while (enumerator["System.Collections.IEnumerator.MoveNext"]()) {
            const x = enumerator["System.Collections.Generic.IEnumerator`1.get_Current"]();
            (index_1 = (i | 0), array.splice(index_1, 0, x));
            i = ((i + 1) | 0);
        }
    }
    finally {
        disposeSafe(enumerator);
    }
}
export function removeInPlace(item_1, array, eq) {
    const i = indexOf(array, item_1, void 0, void 0, eq) | 0;
    if (i > -1) {
        array.splice(i, 1);
        return true;
    }
    else {
        return false;
    }
}
export function removeAllInPlace(predicate, array) {
    const countRemoveAll = (count) => {
        const i = (array.findIndex(predicate)) | 0;
        if (i > -1) {
            array.splice(i, 1);
            return (countRemoveAll(count) + 1) | 0;
        }
        else {
            return count | 0;
        }
    };
    return countRemoveAll(0) | 0;
}
export function partition(f, source, cons) {
    const len = source.length | 0;
    const res1 = Helpers_allocateArrayFromCons(cons, len);
    const res2 = Helpers_allocateArrayFromCons(cons, len);
    let iTrue = 0;
    let iFalse = 0;
    for (let i = 0; i <= (len - 1); i++) {
        if (f(source[i])) {
            res1[iTrue] = source[i];
            iTrue = ((iTrue + 1) | 0);
        }
        else {
            res2[iFalse] = source[i];
            iFalse = ((iFalse + 1) | 0);
        }
    }
    return [truncate(iTrue, res1), truncate(iFalse, res2)];
}
export function find(predicate, array) {
    const matchValue = array.find(predicate);
    if (matchValue == null) {
        return indexNotFound();
    }
    else {
        return value_2(matchValue);
    }
}
export function tryFind(predicate, array) {
    return array.find(predicate);
}
export function findIndex(predicate, array) {
    const matchValue = (array.findIndex(predicate)) | 0;
    if (matchValue > -1) {
        return matchValue | 0;
    }
    else {
        indexNotFound();
        return -1;
    }
}
export function tryFindIndex(predicate, array) {
    const matchValue = (array.findIndex(predicate)) | 0;
    if (matchValue > -1) {
        return matchValue;
    }
    else {
        return void 0;
    }
}
export function pick(chooser, array) {
    const loop = (i_mut) => {
        loop: while (true) {
            const i = i_mut;
            if (i >= array.length) {
                return indexNotFound();
            }
            else {
                const matchValue = chooser(array[i]);
                if (matchValue != null) {
                    return value_2(matchValue);
                }
                else {
                    i_mut = (i + 1);
                    continue loop;
                }
            }
            break;
        }
    };
    return loop(0);
}
export function tryPick(chooser, array) {
    const loop = (i_mut) => {
        loop: while (true) {
            const i = i_mut;
            if (i >= array.length) {
                return void 0;
            }
            else {
                const matchValue = chooser(array[i]);
                if (matchValue == null) {
                    i_mut = (i + 1);
                    continue loop;
                }
                else {
                    return matchValue;
                }
            }
            break;
        }
    };
    return loop(0);
}
export function findBack(predicate, array) {
    const loop = (i_mut) => {
        loop: while (true) {
            const i = i_mut;
            if (i < 0) {
                return indexNotFound();
            }
            else if (predicate(array[i])) {
                return array[i];
            }
            else {
                i_mut = (i - 1);
                continue loop;
            }
            break;
        }
    };
    return loop(array.length - 1);
}
export function tryFindBack(predicate, array) {
    const loop = (i_mut) => {
        loop: while (true) {
            const i = i_mut;
            if (i < 0) {
                return void 0;
            }
            else if (predicate(array[i])) {
                return some(array[i]);
            }
            else {
                i_mut = (i - 1);
                continue loop;
            }
            break;
        }
    };
    return loop(array.length - 1);
}
export function findLastIndex(predicate, array) {
    const loop = (i_mut) => {
        loop: while (true) {
            const i = i_mut;
            if (i < 0) {
                return -1;
            }
            else if (predicate(array[i])) {
                return i | 0;
            }
            else {
                i_mut = (i - 1);
                continue loop;
            }
            break;
        }
    };
    return loop(array.length - 1) | 0;
}
export function findIndexBack(predicate, array) {
    const loop = (i_mut) => {
        loop: while (true) {
            const i = i_mut;
            if (i < 0) {
                indexNotFound();
                return -1;
            }
            else if (predicate(array[i])) {
                return i | 0;
            }
            else {
                i_mut = (i - 1);
                continue loop;
            }
            break;
        }
    };
    return loop(array.length - 1) | 0;
}
export function tryFindIndexBack(predicate, array) {
    const loop = (i_mut) => {
        loop: while (true) {
            const i = i_mut;
            if (i < 0) {
                return void 0;
            }
            else if (predicate(array[i])) {
                return i;
            }
            else {
                i_mut = (i - 1);
                continue loop;
            }
            break;
        }
    };
    return loop(array.length - 1);
}
export function choose(chooser, array, cons) {
    const res = [];
    for (let i = 0; i <= (array.length - 1); i++) {
        const matchValue = chooser(array[i]);
        if (matchValue != null) {
            const y = value_2(matchValue);
            res.push(y);
        }
    }
    if (equals_1(cons, defaultOf())) {
        return res;
    }
    else {
        return map((x) => x, res, cons);
    }
}
export function foldIndexed(folder, state, array) {
    return array.reduce(((delegateArg, delegateArg_1, delegateArg_2) => folder(delegateArg_2, delegateArg, delegateArg_1)), state);
}
export function fold(folder, state, array) {
    return array.reduce((folder), state);
}
export function iterate(action, array) {
    for (let i = 0; i <= (array.length - 1); i++) {
        action(array[i]);
    }
}
export function iterateIndexed(action, array) {
    for (let i = 0; i <= (array.length - 1); i++) {
        action(i, array[i]);
    }
}
export function iterate2(action, array1, array2) {
    if (array1.length !== array2.length) {
        differentLengths();
    }
    for (let i = 0; i <= (array1.length - 1); i++) {
        action(array1[i], array2[i]);
    }
}
export function iterateIndexed2(action, array1, array2) {
    if (array1.length !== array2.length) {
        differentLengths();
    }
    for (let i = 0; i <= (array1.length - 1); i++) {
        action(i, array1[i], array2[i]);
    }
}
export function isEmpty(array) {
    return array.length === 0;
}
export function forAll(predicate, array) {
    return array.every(predicate);
}
export function permute(f, array) {
    const size = array.length | 0;
    const res = array.slice();
    const checkFlags = new Array(size);
    iterateIndexed((i, x) => {
        const j = f(i) | 0;
        if ((j < 0) ? true : (j >= size)) {
            throw new Error("Not a valid permutation");
        }
        res[j] = x;
        checkFlags[j] = 1;
    }, array);
    if (!(checkFlags.every((y) => (1 === y)))) {
        throw new Error("Not a valid permutation");
    }
    return res;
}
export function setSlice(target, lower, upper, source) {
    const lower_1 = defaultArg(lower, 0) | 0;
    const upper_1 = defaultArg(upper, -1) | 0;
    const length = (((upper_1 >= 0) ? upper_1 : (target.length - 1)) - lower_1) | 0;
    for (let i = 0; i <= length; i++) {
        target[i + lower_1] = source[i];
    }
}
export function sortInPlaceBy(projection, xs, comparer) {
    xs.sort((x, y) => comparer.Compare(projection(x), projection(y)));
}
export function sortInPlace(xs, comparer) {
    xs.sort((x, y) => comparer.Compare(x, y));
}
export function sort(xs, comparer) {
    const xs_1 = xs.slice();
    xs_1.sort((x, y) => comparer.Compare(x, y));
    return xs_1;
}
export function sortBy(projection, xs, comparer) {
    const xs_1 = xs.slice();
    xs_1.sort((x, y) => comparer.Compare(projection(x), projection(y)));
    return xs_1;
}
export function sortDescending(xs, comparer) {
    const xs_1 = xs.slice();
    xs_1.sort((x, y) => (comparer.Compare(x, y) * -1));
    return xs_1;
}
export function sortByDescending(projection, xs, comparer) {
    const xs_1 = xs.slice();
    xs_1.sort((x, y) => (comparer.Compare(projection(x), projection(y)) * -1));
    return xs_1;
}
export function sortWith(comparer, xs) {
    const comparer_1 = comparer;
    const xs_1 = xs.slice();
    xs_1.sort(comparer_1);
    return xs_1;
}
export function allPairs(xs, ys) {
    const len1 = xs.length | 0;
    const len2 = ys.length | 0;
    const res = new Array(len1 * len2);
    for (let i = 0; i <= (xs.length - 1); i++) {
        for (let j = 0; j <= (ys.length - 1); j++) {
            res[(i * len2) + j] = [xs[i], ys[j]];
        }
    }
    return res;
}
export function unfold(generator, state) {
    const res = [];
    const loop = (state_1_mut) => {
        loop: while (true) {
            const state_1 = state_1_mut;
            const matchValue = generator(state_1);
            if (matchValue != null) {
                const x = value_2(matchValue)[0];
                const s = value_2(matchValue)[1];
                res.push(x);
                state_1_mut = s;
                continue loop;
            }
            break;
        }
    };
    loop(state);
    return res;
}
export function unzip(array) {
    const len = array.length | 0;
    const res1 = new Array(len);
    const res2 = new Array(len);
    iterateIndexed((i, tupledArg) => {
        res1[i] = tupledArg[0];
        res2[i] = tupledArg[1];
    }, array);
    return [res1, res2];
}
export function unzip3(array) {
    const len = array.length | 0;
    const res1 = new Array(len);
    const res2 = new Array(len);
    const res3 = new Array(len);
    iterateIndexed((i, tupledArg) => {
        res1[i] = tupledArg[0];
        res2[i] = tupledArg[1];
        res3[i] = tupledArg[2];
    }, array);
    return [res1, res2, res3];
}
export function zip(array1, array2) {
    if (array1.length !== array2.length) {
        differentLengths();
    }
    const result = new Array(array1.length);
    for (let i = 0; i <= (array1.length - 1); i++) {
        result[i] = [array1[i], array2[i]];
    }
    return result;
}
export function zip3(array1, array2, array3) {
    if ((array1.length !== array2.length) ? true : (array2.length !== array3.length)) {
        differentLengths();
    }
    const result = new Array(array1.length);
    for (let i = 0; i <= (array1.length - 1); i++) {
        result[i] = [array1[i], array2[i], array3[i]];
    }
    return result;
}
export function chunkBySize(chunkSize, array) {
    if (chunkSize < 1) {
        throw new Error("The input must be positive.\\nParameter name: size");
    }
    if (array.length === 0) {
        return [[]];
    }
    else {
        const result = [];
        for (let x = 0; x <= (~~Math.ceil(array.length / chunkSize) - 1); x++) {
            let slice;
            const start_1 = (x * chunkSize) | 0;
            slice = (array.slice(start_1, (start_1 + chunkSize)));
            result.push(slice);
        }
        return result;
    }
}
export function splitAt(index, array) {
    if ((index < 0) ? true : (index > array.length)) {
        throw new Error((SR_indexOutOfBounds + "\\nParameter name: ") + "index");
    }
    return [array.slice(0, (0 + index)), array.slice(index)];
}
export function compareWith(comparer, source1, source2) {
    if (source1 == null) {
        if (source2 == null) {
            return 0;
        }
        else {
            return -1;
        }
    }
    else if (source2 == null) {
        return 1;
    }
    else {
        const len1 = source1.length | 0;
        const len2 = source2.length | 0;
        const len = ((len1 < len2) ? len1 : len2) | 0;
        let i = 0;
        let res = 0;
        while ((res === 0) && (i < len)) {
            res = (comparer(source1[i], source2[i]) | 0);
            i = ((i + 1) | 0);
        }
        if (res !== 0) {
            return res | 0;
        }
        else if (len1 > len2) {
            return 1;
        }
        else if (len1 < len2) {
            return -1;
        }
        else {
            return 0;
        }
    }
}
export function compareTo(comparer, source1, source2) {
    if (source1 == null) {
        if (source2 == null) {
            return 0;
        }
        else {
            return -1;
        }
    }
    else if (source2 == null) {
        return 1;
    }
    else {
        const len1 = source1.length | 0;
        const len2 = source2.length | 0;
        if (len1 > len2) {
            return 1;
        }
        else if (len1 < len2) {
            return -1;
        }
        else {
            let i = 0;
            let res = 0;
            while ((res === 0) && (i < len1)) {
                res = (comparer(source1[i], source2[i]) | 0);
                i = ((i + 1) | 0);
            }
            return res | 0;
        }
    }
}
export function equalsWith(equals, array1, array2) {
    if (array1 == null) {
        if (array2 == null) {
            return true;
        }
        else {
            return false;
        }
    }
    else if (array2 == null) {
        return false;
    }
    else {
        let i = 0;
        let result = true;
        const length1 = array1.length | 0;
        const length2 = array2.length | 0;
        if (length1 > length2) {
            return false;
        }
        else if (length1 < length2) {
            return false;
        }
        else {
            while ((i < length1) && result) {
                result = equals(array1[i], array2[i]);
                i = ((i + 1) | 0);
            }
            return result;
        }
    }
}
export function exactlyOne(array) {
    switch (array.length) {
        case 1:
            return array[0];
        case 0:
            throw new Error("The input sequence was empty\\nParameter name: array");
        default:
            throw new Error("Input array too long\\nParameter name: array");
    }
}
export function tryExactlyOne(array) {
    if (array.length === 1) {
        return some(array[0]);
    }
    else {
        return void 0;
    }
}
export function head(array) {
    if (array.length === 0) {
        throw new Error("The input array was empty\\nParameter name: array");
    }
    else {
        return array[0];
    }
}
export function tryHead(array) {
    if (array.length === 0) {
        return void 0;
    }
    else {
        return some(array[0]);
    }
}
export function tail(array) {
    if (array.length === 0) {
        throw new Error("Not enough elements\\nParameter name: array");
    }
    return array.slice(1);
}
export function item(index, array) {
    return array[index];
}
export function tryItem(index, array) {
    if ((index < 0) ? true : (index >= array.length)) {
        return void 0;
    }
    else {
        return some(array[index]);
    }
}
export function foldBackIndexed(folder, array, state) {
    return array.reduceRight(((delegateArg, delegateArg_1, delegateArg_2) => folder(delegateArg_2, delegateArg_1, delegateArg)), state);
}
export function foldBack(folder, array, state) {
    return array.reduceRight(((delegateArg, delegateArg_1) => folder(delegateArg_1, delegateArg)), state);
}
export function foldIndexed2(folder, state, array1, array2) {
    let acc = state;
    if (array1.length !== array2.length) {
        throw new Error("Arrays have different lengths");
    }
    for (let i = 0; i <= (array1.length - 1); i++) {
        acc = folder(i, acc, array1[i], array2[i]);
    }
    return acc;
}
export function fold2(folder, state, array1, array2) {
    return foldIndexed2((_arg, acc, x, y) => folder(acc, x, y), state, array1, array2);
}
export function foldBackIndexed2(folder, array1, array2, state) {
    let acc = state;
    if (array1.length !== array2.length) {
        differentLengths();
    }
    const size = array1.length | 0;
    for (let i = 1; i <= size; i++) {
        acc = folder(i - 1, array1[size - i], array2[size - i], acc);
    }
    return acc;
}
export function foldBack2(f, array1, array2, state) {
    return foldBackIndexed2((_arg, x, y, acc) => f(x, y, acc), array1, array2, state);
}
export function reduce(reduction, array) {
    if (array.length === 0) {
        throw new Error("The input array was empty");
    }
    const reduction_1 = reduction;
    return array.reduce(reduction_1);
}
export function reduceBack(reduction, array) {
    if (array.length === 0) {
        throw new Error("The input array was empty");
    }
    const reduction_1 = reduction;
    return array.reduceRight(reduction_1);
}
export function forAll2(predicate, array1, array2) {
    return fold2((acc, x, y) => (acc && predicate(x, y)), true, array1, array2);
}
export function existsOffset(predicate_mut, array_mut, index_mut) {
    existsOffset: while (true) {
        const predicate = predicate_mut, array = array_mut, index = index_mut;
        if (index === array.length) {
            return false;
        }
        else if (predicate(array[index])) {
            return true;
        }
        else {
            predicate_mut = predicate;
            array_mut = array;
            index_mut = (index + 1);
            continue existsOffset;
        }
        break;
    }
}
export function exists(predicate, array) {
    return existsOffset(predicate, array, 0);
}
export function existsOffset2(predicate_mut, array1_mut, array2_mut, index_mut) {
    existsOffset2: while (true) {
        const predicate = predicate_mut, array1 = array1_mut, array2 = array2_mut, index = index_mut;
        if (index === array1.length) {
            return false;
        }
        else if (predicate(array1[index], array2[index])) {
            return true;
        }
        else {
            predicate_mut = predicate;
            array1_mut = array1;
            array2_mut = array2;
            index_mut = (index + 1);
            continue existsOffset2;
        }
        break;
    }
}
export function exists2(predicate, array1, array2) {
    if (array1.length !== array2.length) {
        differentLengths();
    }
    return existsOffset2(predicate, array1, array2, 0);
}
export function sum(array, adder) {
    let acc = adder.GetZero();
    for (let i = 0; i <= (array.length - 1); i++) {
        acc = adder.Add(acc, array[i]);
    }
    return acc;
}
export function sumBy(projection, array, adder) {
    let acc = adder.GetZero();
    for (let i = 0; i <= (array.length - 1); i++) {
        acc = adder.Add(acc, projection(array[i]));
    }
    return acc;
}
export function maxBy(projection, xs, comparer) {
    return reduce((x, y) => ((comparer.Compare(projection(y), projection(x)) > 0) ? y : x), xs);
}
export function max(xs, comparer) {
    return reduce((x, y) => ((comparer.Compare(y, x) > 0) ? y : x), xs);
}
export function minBy(projection, xs, comparer) {
    return reduce((x, y) => ((comparer.Compare(projection(y), projection(x)) > 0) ? x : y), xs);
}
export function min(xs, comparer) {
    return reduce((x, y) => ((comparer.Compare(y, x) > 0) ? x : y), xs);
}
export function average(array, averager) {
    if (array.length === 0) {
        throw new Error("The input array was empty\\nParameter name: array");
    }
    let total = averager.GetZero();
    for (let i = 0; i <= (array.length - 1); i++) {
        total = averager.Add(total, array[i]);
    }
    return averager.DivideByInt(total, array.length);
}
export function averageBy(projection, array, averager) {
    if (array.length === 0) {
        throw new Error("The input array was empty\\nParameter name: array");
    }
    let total = averager.GetZero();
    for (let i = 0; i <= (array.length - 1); i++) {
        total = averager.Add(total, projection(array[i]));
    }
    return averager.DivideByInt(total, array.length);
}
export function windowed(windowSize, source) {
    if (windowSize <= 0) {
        throw new Error("windowSize must be positive");
    }
    let res;
    const len = max_1(0, (source.length - windowSize) + 1) | 0;
    res = (new Array(len));
    for (let i = windowSize; i <= source.length; i++) {
        res[i - windowSize] = source.slice(i - windowSize, (i - 1) + 1);
    }
    return res;
}
export function splitInto(chunks, array) {
    if (chunks < 1) {
        throw new Error("The input must be positive.\\nParameter name: chunks");
    }
    if (array.length === 0) {
        return [[]];
    }
    else {
        const result = [];
        const chunks_1 = min_1(chunks, array.length) | 0;
        const minChunkSize = ~~(array.length / chunks_1) | 0;
        const chunksWithExtraItem = (array.length % chunks_1) | 0;
        for (let i = 0; i <= (chunks_1 - 1); i++) {
            const chunkSize = ((i < chunksWithExtraItem) ? (minChunkSize + 1) : minChunkSize) | 0;
            let slice;
            const start_1 = ((i * minChunkSize) + min_1(chunksWithExtraItem, i)) | 0;
            slice = (array.slice(start_1, (start_1 + chunkSize)));
            result.push(slice);
        }
        return result;
    }
}
export function transpose(arrays, cons) {
    const arrays_1 = Array.isArray(arrays) ? arrays : (Array.from(arrays));
    const len = arrays_1.length | 0;
    if (len === 0) {
        return new Array(0);
    }
    else {
        const firstArray = arrays_1[0];
        const lenInner = firstArray.length | 0;
        if (!forAll((a) => (a.length === lenInner), arrays_1)) {
            differentLengths();
        }
        const result = new Array(lenInner);
        for (let i = 0; i <= (lenInner - 1); i++) {
            result[i] = Helpers_allocateArrayFromCons(cons, len);
            for (let j = 0; j <= (len - 1); j++) {
                result[i][j] = arrays_1[j][i];
            }
        }
        return result;
    }
}
export function insertAt(index, y, xs, cons) {
    const len = xs.length | 0;
    if ((index < 0) ? true : (index > len)) {
        throw new Error((SR_indexOutOfBounds + "\\nParameter name: ") + "index");
    }
    const target = Helpers_allocateArrayFromCons(cons, len + 1);
    for (let i = 0; i <= (index - 1); i++) {
        target[i] = xs[i];
    }
    target[index] = y;
    for (let i_1 = index; i_1 <= (len - 1); i_1++) {
        target[i_1 + 1] = xs[i_1];
    }
    return target;
}
export function insertManyAt(index, ys, xs, cons) {
    const len = xs.length | 0;
    if ((index < 0) ? true : (index > len)) {
        throw new Error((SR_indexOutOfBounds + "\\nParameter name: ") + "index");
    }
    const ys_1 = Array.from(ys);
    const len2 = ys_1.length | 0;
    const target = Helpers_allocateArrayFromCons(cons, len + len2);
    for (let i = 0; i <= (index - 1); i++) {
        target[i] = xs[i];
    }
    for (let i_1 = 0; i_1 <= (len2 - 1); i_1++) {
        target[index + i_1] = ys_1[i_1];
    }
    for (let i_2 = index; i_2 <= (len - 1); i_2++) {
        target[i_2 + len2] = xs[i_2];
    }
    return target;
}
export function removeAt(index, xs) {
    if ((index < 0) ? true : (index >= xs.length)) {
        throw new Error((SR_indexOutOfBounds + "\\nParameter name: ") + "index");
    }
    let i = -1;
    return filter((_arg) => {
        i = ((i + 1) | 0);
        return i !== index;
    }, xs);
}
export function removeManyAt(index, count, xs) {
    let i = -1;
    let status = -1;
    const ys = filter((_arg) => {
        i = ((i + 1) | 0);
        if (i === index) {
            status = 0;
            return false;
        }
        else if (i > index) {
            if (i < (index + count)) {
                return false;
            }
            else {
                status = 1;
                return true;
            }
        }
        else {
            return true;
        }
    }, xs);
    const status_1 = (((status === 0) && ((i + 1) === (index + count))) ? 1 : status) | 0;
    if (status_1 < 1) {
        throw new Error((SR_indexOutOfBounds + "\\nParameter name: ") + ((status_1 < 0) ? "index" : "count"));
    }
    return ys;
}
export function updateAt(index, y, xs, cons) {
    const len = xs.length | 0;
    if ((index < 0) ? true : (index >= len)) {
        throw new Error((SR_indexOutOfBounds + "\\nParameter name: ") + "index");
    }
    const target = Helpers_allocateArrayFromCons(cons, len);
    for (let i = 0; i <= (len - 1); i++) {
        target[i] = ((i === index) ? y : xs[i]);
    }
    return target;
}
