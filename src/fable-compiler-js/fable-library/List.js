import { join } from "./String.js";
import { defaultArg, some, value as value_1 } from "./Option.js";
import { disposeSafe, isArrayLike, defaultOf, toIterator, getEnumerator, compare, structuralHash, equals } from "./Util.js";
import { Record } from "./Types.js";
import { class_type, record_type, option_type } from "./Reflection.js";
import { SR_inputSequenceTooLong, SR_inputSequenceEmpty, SR_inputMustBeNonNegative, SR_notEnoughElements, SR_differentLengths, SR_keyNotFoundAlt, SR_indexOutOfBounds, SR_inputWasEmpty } from "./Global.js";
import { transpose as transpose_1, splitInto as splitInto_1, windowed as windowed_1, pairwise as pairwise_1, chunkBySize as chunkBySize_1, map as map_1, permute as permute_1, tryFindIndexBack as tryFindIndexBack_1, tryFindBack as tryFindBack_1, scanBack as scanBack_1, foldBack2 as foldBack2_1, foldBack as foldBack_1, fill } from "./Array.js";
export class FSharpList extends Record {
    constructor(head, tail) {
        super();
        this.head = head;
        this.tail = tail;
    }
    toString() {
        const xs = this;
        return ("[" + join("; ", xs)) + "]";
    }
    Equals(other) {
        const xs = this;
        if (xs === other) {
            return true;
        }
        else {
            const loop = (xs_1_mut, ys_1_mut) => {
                loop: while (true) {
                    const xs_1 = xs_1_mut, ys_1 = ys_1_mut;
                    const matchValue = xs_1.tail;
                    const matchValue_1 = ys_1.tail;
                    if (matchValue != null) {
                        if (matchValue_1 != null) {
                            const xt = value_1(matchValue);
                            const yt = value_1(matchValue_1);
                            if (equals(xs_1.head, ys_1.head)) {
                                xs_1_mut = xt;
                                ys_1_mut = yt;
                                continue loop;
                            }
                            else {
                                return false;
                            }
                        }
                        else {
                            return false;
                        }
                    }
                    else if (matchValue_1 != null) {
                        return false;
                    }
                    else {
                        return true;
                    }
                    break;
                }
            };
            return loop(xs, other);
        }
    }
    GetHashCode() {
        const xs = this;
        const loop = (i_mut, h_mut, xs_1_mut) => {
            loop: while (true) {
                const i = i_mut, h = h_mut, xs_1 = xs_1_mut;
                const matchValue = xs_1.tail;
                if (matchValue != null) {
                    const t = value_1(matchValue);
                    if (i > 18) {
                        return h | 0;
                    }
                    else {
                        i_mut = (i + 1);
                        h_mut = (((h << 1) + structuralHash(xs_1.head)) + (631 * i));
                        xs_1_mut = t;
                        continue loop;
                    }
                }
                else {
                    return h | 0;
                }
                break;
            }
        };
        return loop(0, 0, xs) | 0;
    }
    toJSON() {
        const this$ = this;
        return Array.from(this$);
    }
    CompareTo(other) {
        const xs = this;
        const loop = (xs_1_mut, ys_1_mut) => {
            loop: while (true) {
                const xs_1 = xs_1_mut, ys_1 = ys_1_mut;
                const matchValue = xs_1.tail;
                const matchValue_1 = ys_1.tail;
                if (matchValue != null) {
                    if (matchValue_1 != null) {
                        const xt = value_1(matchValue);
                        const yt = value_1(matchValue_1);
                        const c = compare(xs_1.head, ys_1.head) | 0;
                        if (c === 0) {
                            xs_1_mut = xt;
                            ys_1_mut = yt;
                            continue loop;
                        }
                        else {
                            return c | 0;
                        }
                    }
                    else {
                        return 1;
                    }
                }
                else if (matchValue_1 != null) {
                    return -1;
                }
                else {
                    return 0;
                }
                break;
            }
        };
        return loop(xs, other) | 0;
    }
    GetEnumerator() {
        const xs = this;
        return ListEnumerator$1_$ctor_3002E699(xs);
    }
    [Symbol.iterator]() {
        return toIterator(getEnumerator(this));
    }
    "System.Collections.IEnumerable.GetEnumerator"() {
        const xs = this;
        return getEnumerator(xs);
    }
}
export function FSharpList_$reflection(gen0) {
    return record_type("ListModule.FSharpList", [gen0], FSharpList, () => [["head", gen0], ["tail", option_type(FSharpList_$reflection(gen0))]]);
}
export class ListEnumerator$1 {
    constructor(xs) {
        this.xs = xs;
        this.it = this.xs;
        this.current = defaultOf();
    }
    "System.Collections.Generic.IEnumerator`1.get_Current"() {
        const _ = this;
        return _.current;
    }
    "System.Collections.IEnumerator.get_Current"() {
        const _ = this;
        return _.current;
    }
    "System.Collections.IEnumerator.MoveNext"() {
        const _ = this;
        const matchValue = _.it.tail;
        if (matchValue != null) {
            const t = value_1(matchValue);
            _.current = _.it.head;
            _.it = t;
            return true;
        }
        else {
            return false;
        }
    }
    "System.Collections.IEnumerator.Reset"() {
        const _ = this;
        _.it = _.xs;
        _.current = defaultOf();
    }
    Dispose() {
    }
}
export function ListEnumerator$1_$reflection(gen0) {
    return class_type("ListModule.ListEnumerator`1", [gen0], ListEnumerator$1);
}
export function ListEnumerator$1_$ctor_3002E699(xs) {
    return new ListEnumerator$1(xs);
}
export function FSharpList_get_Empty() {
    return new FSharpList(defaultOf(), void 0);
}
export function FSharpList_Cons_305B8EAC(x, xs) {
    return new FSharpList(x, xs);
}
export function FSharpList__get_IsEmpty(xs) {
    return xs.tail == null;
}
export function FSharpList__get_Length(xs) {
    const loop = (i_mut, xs_1_mut) => {
        loop: while (true) {
            const i = i_mut, xs_1 = xs_1_mut;
            const matchValue = xs_1.tail;
            if (matchValue != null) {
                i_mut = (i + 1);
                xs_1_mut = value_1(matchValue);
                continue loop;
            }
            else {
                return i | 0;
            }
            break;
        }
    };
    return loop(0, xs) | 0;
}
export function FSharpList__get_Head(xs) {
    const matchValue = xs.tail;
    if (matchValue != null) {
        return xs.head;
    }
    else {
        throw new Error((SR_inputWasEmpty + "\\nParameter name: ") + "list");
    }
}
export function FSharpList__get_Tail(xs) {
    const matchValue = xs.tail;
    if (matchValue != null) {
        return value_1(matchValue);
    }
    else {
        throw new Error((SR_inputWasEmpty + "\\nParameter name: ") + "list");
    }
}
export function FSharpList__get_Item_Z524259A4(xs, index) {
    const loop = (i_mut, xs_1_mut) => {
        loop: while (true) {
            const i = i_mut, xs_1 = xs_1_mut;
            const matchValue = xs_1.tail;
            if (matchValue != null) {
                if (i === index) {
                    return xs_1.head;
                }
                else {
                    i_mut = (i + 1);
                    xs_1_mut = value_1(matchValue);
                    continue loop;
                }
            }
            else {
                throw new Error((SR_indexOutOfBounds + "\\nParameter name: ") + "index");
            }
            break;
        }
    };
    return loop(0, xs);
}
export function indexNotFound() {
    throw new Error(SR_keyNotFoundAlt);
}
export function empty() {
    return FSharpList_get_Empty();
}
export function cons(x, xs) {
    return FSharpList_Cons_305B8EAC(x, xs);
}
export function singleton(x) {
    return FSharpList_Cons_305B8EAC(x, FSharpList_get_Empty());
}
export function isEmpty(xs) {
    return FSharpList__get_IsEmpty(xs);
}
export function length(xs) {
    return FSharpList__get_Length(xs);
}
export function head(xs) {
    return FSharpList__get_Head(xs);
}
export function tryHead(xs) {
    if (FSharpList__get_IsEmpty(xs)) {
        return void 0;
    }
    else {
        return some(FSharpList__get_Head(xs));
    }
}
export function tail(xs) {
    return FSharpList__get_Tail(xs);
}
export function tryLast(xs_mut) {
    tryLast: while (true) {
        const xs = xs_mut;
        if (FSharpList__get_IsEmpty(xs)) {
            return void 0;
        }
        else {
            const t = FSharpList__get_Tail(xs);
            if (FSharpList__get_IsEmpty(t)) {
                return some(FSharpList__get_Head(xs));
            }
            else {
                xs_mut = t;
                continue tryLast;
            }
        }
        break;
    }
}
export function last(xs) {
    const matchValue = tryLast(xs);
    if (matchValue == null) {
        throw new Error(SR_inputWasEmpty);
    }
    else {
        return value_1(matchValue);
    }
}
export function compareWith(comparer, xs, ys) {
    const loop = (xs_1_mut, ys_1_mut) => {
        loop: while (true) {
            const xs_1 = xs_1_mut, ys_1 = ys_1_mut;
            const matchValue = FSharpList__get_IsEmpty(xs_1);
            const matchValue_1 = FSharpList__get_IsEmpty(ys_1);
            if (matchValue) {
                if (matchValue_1) {
                    return 0;
                }
                else {
                    return -1;
                }
            }
            else if (matchValue_1) {
                return 1;
            }
            else {
                const c = comparer(FSharpList__get_Head(xs_1), FSharpList__get_Head(ys_1)) | 0;
                if (c === 0) {
                    xs_1_mut = FSharpList__get_Tail(xs_1);
                    ys_1_mut = FSharpList__get_Tail(ys_1);
                    continue loop;
                }
                else {
                    return c | 0;
                }
            }
            break;
        }
    };
    return loop(xs, ys) | 0;
}
export function toArray(xs) {
    const len = FSharpList__get_Length(xs) | 0;
    const res = fill(new Array(len), 0, len, null);
    const loop = (i_mut, xs_1_mut) => {
        loop: while (true) {
            const i = i_mut, xs_1 = xs_1_mut;
            if (!FSharpList__get_IsEmpty(xs_1)) {
                res[i] = FSharpList__get_Head(xs_1);
                i_mut = (i + 1);
                xs_1_mut = FSharpList__get_Tail(xs_1);
                continue loop;
            }
            break;
        }
    };
    loop(0, xs);
    return res;
}
export function fold(folder, state, xs) {
    let acc = state;
    let xs_1 = xs;
    while (!FSharpList__get_IsEmpty(xs_1)) {
        acc = folder(acc, head(xs_1));
        xs_1 = FSharpList__get_Tail(xs_1);
    }
    return acc;
}
export function reverse(xs) {
    return fold((acc, x) => FSharpList_Cons_305B8EAC(x, acc), FSharpList_get_Empty(), xs);
}
export function foldBack(folder, xs, state) {
    return foldBack_1(folder, toArray(xs), state);
}
export function foldIndexed(folder, state, xs) {
    const loop = (i_mut, acc_mut, xs_1_mut) => {
        loop: while (true) {
            const i = i_mut, acc = acc_mut, xs_1 = xs_1_mut;
            if (FSharpList__get_IsEmpty(xs_1)) {
                return acc;
            }
            else {
                i_mut = (i + 1);
                acc_mut = folder(i, acc, FSharpList__get_Head(xs_1));
                xs_1_mut = FSharpList__get_Tail(xs_1);
                continue loop;
            }
            break;
        }
    };
    return loop(0, state, xs);
}
export function fold2(folder, state, xs, ys) {
    let acc = state;
    let xs_1 = xs;
    let ys_1 = ys;
    while (!FSharpList__get_IsEmpty(xs_1) && !FSharpList__get_IsEmpty(ys_1)) {
        acc = folder(acc, FSharpList__get_Head(xs_1), FSharpList__get_Head(ys_1));
        xs_1 = FSharpList__get_Tail(xs_1);
        ys_1 = FSharpList__get_Tail(ys_1);
    }
    return acc;
}
export function foldBack2(folder, xs, ys, state) {
    return foldBack2_1(folder, toArray(xs), toArray(ys), state);
}
export function unfold(gen, state) {
    const loop = (acc_mut, node_mut) => {
        let t;
        loop: while (true) {
            const acc = acc_mut, node = node_mut;
            const matchValue = gen(acc);
            if (matchValue != null) {
                acc_mut = value_1(matchValue)[1];
                node_mut = ((t = (new FSharpList(value_1(matchValue)[0], void 0)), (node.tail = t, t)));
                continue loop;
            }
            else {
                return node;
            }
            break;
        }
    };
    const root = FSharpList_get_Empty();
    const node_1 = loop(state, root);
    const t_2 = FSharpList_get_Empty();
    node_1.tail = t_2;
    return FSharpList__get_Tail(root);
}
export function iterate(action, xs) {
    fold((unitVar, x) => {
        action(x);
    }, void 0, xs);
}
export function iterate2(action, xs, ys) {
    fold2((unitVar, x, y) => {
        action(x, y);
    }, void 0, xs, ys);
}
export function iterateIndexed(action, xs) {
    fold((i, x) => {
        action(i, x);
        return (i + 1) | 0;
    }, 0, xs);
}
export function iterateIndexed2(action, xs, ys) {
    fold2((i, x, y) => {
        action(i, x, y);
        return (i + 1) | 0;
    }, 0, xs, ys);
}
export function toSeq(xs) {
    return xs;
}
export function ofArrayWithTail(xs, tail_1) {
    let res = tail_1;
    for (let i = xs.length - 1; i >= 0; i--) {
        res = FSharpList_Cons_305B8EAC(xs[i], res);
    }
    return res;
}
export function ofArray(xs) {
    return ofArrayWithTail(xs, FSharpList_get_Empty());
}
export function ofSeq(xs) {
    let xs_3, t;
    if (isArrayLike(xs)) {
        return ofArray(xs);
    }
    else if (xs instanceof FSharpList) {
        return xs;
    }
    else {
        const root = FSharpList_get_Empty();
        let node = root;
        const enumerator = getEnumerator(xs);
        try {
            while (enumerator["System.Collections.IEnumerator.MoveNext"]()) {
                const x = enumerator["System.Collections.Generic.IEnumerator`1.get_Current"]();
                node = ((xs_3 = node, (t = (new FSharpList(x, void 0)), (xs_3.tail = t, t))));
            }
        }
        finally {
            disposeSafe(enumerator);
        }
        const xs_5 = node;
        const t_2 = FSharpList_get_Empty();
        xs_5.tail = t_2;
        return FSharpList__get_Tail(root);
    }
}
export function concat(lists) {
    const root = FSharpList_get_Empty();
    let node = root;
    const action = (xs) => {
        node = fold((acc, x) => {
            const t = new FSharpList(x, void 0);
            acc.tail = t;
            return t;
        }, node, xs);
    };
    if (isArrayLike(lists)) {
        const xs_3 = lists;
        xs_3.forEach(action);
    }
    else if (lists instanceof FSharpList) {
        iterate(action, lists);
    }
    else {
        const enumerator = getEnumerator(lists);
        try {
            while (enumerator["System.Collections.IEnumerator.MoveNext"]()) {
                action(enumerator["System.Collections.Generic.IEnumerator`1.get_Current"]());
            }
        }
        finally {
            disposeSafe(enumerator);
        }
    }
    const xs_6 = node;
    const t_2 = FSharpList_get_Empty();
    xs_6.tail = t_2;
    return FSharpList__get_Tail(root);
}
export function scan(folder, state, xs) {
    let xs_4, t_2;
    const root = FSharpList_get_Empty();
    let node;
    const t = new FSharpList(state, void 0);
    root.tail = t;
    node = t;
    let acc = state;
    let xs_3 = xs;
    while (!FSharpList__get_IsEmpty(xs_3)) {
        acc = folder(acc, FSharpList__get_Head(xs_3));
        node = ((xs_4 = node, (t_2 = (new FSharpList(acc, void 0)), (xs_4.tail = t_2, t_2))));
        xs_3 = FSharpList__get_Tail(xs_3);
    }
    const xs_6 = node;
    const t_4 = FSharpList_get_Empty();
    xs_6.tail = t_4;
    return FSharpList__get_Tail(root);
}
export function scanBack(folder, xs, state) {
    return ofArray(scanBack_1(folder, toArray(xs), state));
}
export function append(xs, ys) {
    return fold((acc, x) => FSharpList_Cons_305B8EAC(x, acc), ys, reverse(xs));
}
export function collect(mapping, xs) {
    let xs_1, t;
    const root = FSharpList_get_Empty();
    let node = root;
    let ys = xs;
    while (!FSharpList__get_IsEmpty(ys)) {
        let zs = mapping(FSharpList__get_Head(ys));
        while (!FSharpList__get_IsEmpty(zs)) {
            node = ((xs_1 = node, (t = (new FSharpList(FSharpList__get_Head(zs), void 0)), (xs_1.tail = t, t))));
            zs = FSharpList__get_Tail(zs);
        }
        ys = FSharpList__get_Tail(ys);
    }
    const xs_3 = node;
    const t_2 = FSharpList_get_Empty();
    xs_3.tail = t_2;
    return FSharpList__get_Tail(root);
}
export function mapIndexed(mapping, xs) {
    const root = FSharpList_get_Empty();
    const node = foldIndexed((i, acc, x) => {
        const t = new FSharpList(mapping(i, x), void 0);
        acc.tail = t;
        return t;
    }, root, xs);
    const t_2 = FSharpList_get_Empty();
    node.tail = t_2;
    return FSharpList__get_Tail(root);
}
export function map(mapping, xs) {
    const root = FSharpList_get_Empty();
    const node = fold((acc, x) => {
        const t = new FSharpList(mapping(x), void 0);
        acc.tail = t;
        return t;
    }, root, xs);
    const t_2 = FSharpList_get_Empty();
    node.tail = t_2;
    return FSharpList__get_Tail(root);
}
export function indexed(xs) {
    return mapIndexed((i, x) => [i, x], xs);
}
export function map2(mapping, xs, ys) {
    const root = FSharpList_get_Empty();
    const node = fold2((acc, x, y) => {
        const t = new FSharpList(mapping(x, y), void 0);
        acc.tail = t;
        return t;
    }, root, xs, ys);
    const t_2 = FSharpList_get_Empty();
    node.tail = t_2;
    return FSharpList__get_Tail(root);
}
export function mapIndexed2(mapping, xs, ys) {
    const loop = (i_mut, acc_mut, xs_1_mut, ys_1_mut) => {
        let t;
        loop: while (true) {
            const i = i_mut, acc = acc_mut, xs_1 = xs_1_mut, ys_1 = ys_1_mut;
            if (FSharpList__get_IsEmpty(xs_1) ? true : FSharpList__get_IsEmpty(ys_1)) {
                return acc;
            }
            else {
                i_mut = (i + 1);
                acc_mut = ((t = (new FSharpList(mapping(i, FSharpList__get_Head(xs_1), FSharpList__get_Head(ys_1)), void 0)), (acc.tail = t, t)));
                xs_1_mut = FSharpList__get_Tail(xs_1);
                ys_1_mut = FSharpList__get_Tail(ys_1);
                continue loop;
            }
            break;
        }
    };
    const root = FSharpList_get_Empty();
    const node_1 = loop(0, root, xs, ys);
    const t_2 = FSharpList_get_Empty();
    node_1.tail = t_2;
    return FSharpList__get_Tail(root);
}
export function map3(mapping, xs, ys, zs) {
    const loop = (acc_mut, xs_1_mut, ys_1_mut, zs_1_mut) => {
        let t;
        loop: while (true) {
            const acc = acc_mut, xs_1 = xs_1_mut, ys_1 = ys_1_mut, zs_1 = zs_1_mut;
            if ((FSharpList__get_IsEmpty(xs_1) ? true : FSharpList__get_IsEmpty(ys_1)) ? true : FSharpList__get_IsEmpty(zs_1)) {
                return acc;
            }
            else {
                acc_mut = ((t = (new FSharpList(mapping(FSharpList__get_Head(xs_1), FSharpList__get_Head(ys_1), FSharpList__get_Head(zs_1)), void 0)), (acc.tail = t, t)));
                xs_1_mut = FSharpList__get_Tail(xs_1);
                ys_1_mut = FSharpList__get_Tail(ys_1);
                zs_1_mut = FSharpList__get_Tail(zs_1);
                continue loop;
            }
            break;
        }
    };
    const root = FSharpList_get_Empty();
    const node_1 = loop(root, xs, ys, zs);
    const t_2 = FSharpList_get_Empty();
    node_1.tail = t_2;
    return FSharpList__get_Tail(root);
}
export function mapFold(mapping, state, xs) {
    const root = FSharpList_get_Empty();
    const patternInput_1 = fold((tupledArg, x) => {
        let t;
        const patternInput = mapping(tupledArg[1], x);
        return [(t = (new FSharpList(patternInput[0], void 0)), (tupledArg[0].tail = t, t)), patternInput[1]];
    }, [root, state], xs);
    const t_2 = FSharpList_get_Empty();
    patternInput_1[0].tail = t_2;
    return [FSharpList__get_Tail(root), patternInput_1[1]];
}
export function mapFoldBack(mapping, xs, state) {
    return mapFold((acc, x) => mapping(x, acc), state, reverse(xs));
}
export function tryPick(f, xs) {
    const loop = (xs_1_mut) => {
        loop: while (true) {
            const xs_1 = xs_1_mut;
            if (FSharpList__get_IsEmpty(xs_1)) {
                return void 0;
            }
            else {
                const matchValue = f(FSharpList__get_Head(xs_1));
                if (matchValue == null) {
                    xs_1_mut = FSharpList__get_Tail(xs_1);
                    continue loop;
                }
                else {
                    return matchValue;
                }
            }
            break;
        }
    };
    return loop(xs);
}
export function pick(f, xs) {
    const matchValue = tryPick(f, xs);
    if (matchValue == null) {
        return indexNotFound();
    }
    else {
        return value_1(matchValue);
    }
}
export function tryFind(f, xs) {
    return tryPick((x) => (f(x) ? some(x) : void 0), xs);
}
export function find(f, xs) {
    const matchValue = tryFind(f, xs);
    if (matchValue == null) {
        return indexNotFound();
    }
    else {
        return value_1(matchValue);
    }
}
export function tryFindBack(f, xs) {
    return tryFindBack_1(f, toArray(xs));
}
export function findBack(f, xs) {
    const matchValue = tryFindBack(f, xs);
    if (matchValue == null) {
        return indexNotFound();
    }
    else {
        return value_1(matchValue);
    }
}
export function tryFindIndex(f, xs) {
    const loop = (i_mut, xs_1_mut) => {
        loop: while (true) {
            const i = i_mut, xs_1 = xs_1_mut;
            if (FSharpList__get_IsEmpty(xs_1)) {
                return void 0;
            }
            else if (f(FSharpList__get_Head(xs_1))) {
                return i;
            }
            else {
                i_mut = (i + 1);
                xs_1_mut = FSharpList__get_Tail(xs_1);
                continue loop;
            }
            break;
        }
    };
    return loop(0, xs);
}
export function findIndex(f, xs) {
    const matchValue = tryFindIndex(f, xs);
    if (matchValue == null) {
        indexNotFound();
        return -1;
    }
    else {
        return value_1(matchValue) | 0;
    }
}
export function tryFindIndexBack(f, xs) {
    return tryFindIndexBack_1(f, toArray(xs));
}
export function findIndexBack(f, xs) {
    const matchValue = tryFindIndexBack(f, xs);
    if (matchValue == null) {
        indexNotFound();
        return -1;
    }
    else {
        return value_1(matchValue) | 0;
    }
}
export function tryItem(n, xs) {
    const loop = (i_mut, xs_1_mut) => {
        loop: while (true) {
            const i = i_mut, xs_1 = xs_1_mut;
            if (FSharpList__get_IsEmpty(xs_1)) {
                return void 0;
            }
            else if (i === n) {
                return some(FSharpList__get_Head(xs_1));
            }
            else {
                i_mut = (i + 1);
                xs_1_mut = FSharpList__get_Tail(xs_1);
                continue loop;
            }
            break;
        }
    };
    return loop(0, xs);
}
export function item(n, xs) {
    return FSharpList__get_Item_Z524259A4(xs, n);
}
export function filter(f, xs) {
    const root = FSharpList_get_Empty();
    const node = fold((acc, x) => {
        if (f(x)) {
            const t = new FSharpList(x, void 0);
            acc.tail = t;
            return t;
        }
        else {
            return acc;
        }
    }, root, xs);
    const t_2 = FSharpList_get_Empty();
    node.tail = t_2;
    return FSharpList__get_Tail(root);
}
export function partition(f, xs) {
    const matchValue = FSharpList_get_Empty();
    const root2 = FSharpList_get_Empty();
    const root1 = matchValue;
    const patternInput_1 = fold((tupledArg, x) => {
        let t, t_2;
        const lacc = tupledArg[0];
        const racc = tupledArg[1];
        if (f(x)) {
            return [(t = (new FSharpList(x, void 0)), (lacc.tail = t, t)), racc];
        }
        else {
            return [lacc, (t_2 = (new FSharpList(x, void 0)), (racc.tail = t_2, t_2))];
        }
    }, [root1, root2], xs);
    const t_4 = FSharpList_get_Empty();
    patternInput_1[0].tail = t_4;
    const t_5 = FSharpList_get_Empty();
    patternInput_1[1].tail = t_5;
    return [FSharpList__get_Tail(root1), FSharpList__get_Tail(root2)];
}
export function choose(f, xs) {
    const root = FSharpList_get_Empty();
    const node = fold((acc, x) => {
        const matchValue = f(x);
        if (matchValue == null) {
            return acc;
        }
        else {
            const t = new FSharpList(value_1(matchValue), void 0);
            acc.tail = t;
            return t;
        }
    }, root, xs);
    const t_2 = FSharpList_get_Empty();
    node.tail = t_2;
    return FSharpList__get_Tail(root);
}
export function contains(value, xs, eq) {
    return tryFindIndex((v) => eq.Equals(value, v), xs) != null;
}
export function initialize(n, f) {
    let xs, t;
    const root = FSharpList_get_Empty();
    let node = root;
    for (let i = 0; i <= (n - 1); i++) {
        node = ((xs = node, (t = (new FSharpList(f(i), void 0)), (xs.tail = t, t))));
    }
    const xs_2 = node;
    const t_2 = FSharpList_get_Empty();
    xs_2.tail = t_2;
    return FSharpList__get_Tail(root);
}
export function replicate(n, x) {
    return initialize(n, (_arg) => x);
}
export function reduce(f, xs) {
    if (FSharpList__get_IsEmpty(xs)) {
        throw new Error(SR_inputWasEmpty);
    }
    else {
        return fold(f, head(xs), tail(xs));
    }
}
export function reduceBack(f, xs) {
    if (FSharpList__get_IsEmpty(xs)) {
        throw new Error(SR_inputWasEmpty);
    }
    else {
        return foldBack(f, tail(xs), head(xs));
    }
}
export function forAll(f, xs) {
    return fold((acc, x) => (acc && f(x)), true, xs);
}
export function forAll2(f, xs, ys) {
    return fold2((acc, x, y) => (acc && f(x, y)), true, xs, ys);
}
export function exists(f, xs) {
    return tryFindIndex(f, xs) != null;
}
export function exists2(f_mut, xs_mut, ys_mut) {
    exists2: while (true) {
        const f = f_mut, xs = xs_mut, ys = ys_mut;
        const matchValue = FSharpList__get_IsEmpty(xs);
        const matchValue_1 = FSharpList__get_IsEmpty(ys);
        let matchResult;
        if (matchValue) {
            if (matchValue_1) {
                matchResult = 0;
            }
            else {
                matchResult = 2;
            }
        }
        else if (matchValue_1) {
            matchResult = 2;
        }
        else {
            matchResult = 1;
        }
        switch (matchResult) {
            case 0:
                return false;
            case 1:
                if (f(FSharpList__get_Head(xs), FSharpList__get_Head(ys))) {
                    return true;
                }
                else {
                    f_mut = f;
                    xs_mut = FSharpList__get_Tail(xs);
                    ys_mut = FSharpList__get_Tail(ys);
                    continue exists2;
                }
            default:
                throw new Error((SR_differentLengths + "\\nParameter name: ") + "list2");
        }
        break;
    }
}
export function unzip(xs) {
    return foldBack((tupledArg, tupledArg_1) => [FSharpList_Cons_305B8EAC(tupledArg[0], tupledArg_1[0]), FSharpList_Cons_305B8EAC(tupledArg[1], tupledArg_1[1])], xs, [FSharpList_get_Empty(), FSharpList_get_Empty()]);
}
export function unzip3(xs) {
    return foldBack((tupledArg, tupledArg_1) => [FSharpList_Cons_305B8EAC(tupledArg[0], tupledArg_1[0]), FSharpList_Cons_305B8EAC(tupledArg[1], tupledArg_1[1]), FSharpList_Cons_305B8EAC(tupledArg[2], tupledArg_1[2])], xs, [FSharpList_get_Empty(), FSharpList_get_Empty(), FSharpList_get_Empty()]);
}
export function zip(xs, ys) {
    return map2((x, y) => [x, y], xs, ys);
}
export function zip3(xs, ys, zs) {
    return map3((x, y, z) => [x, y, z], xs, ys, zs);
}
export function sortWith(comparer, xs) {
    const arr = toArray(xs);
    arr.sort(comparer);
    return ofArray(arr);
}
export function sort(xs, comparer) {
    return sortWith((x, y) => comparer.Compare(x, y), xs);
}
export function sortBy(projection, xs, comparer) {
    return sortWith((x, y) => comparer.Compare(projection(x), projection(y)), xs);
}
export function sortDescending(xs, comparer) {
    return sortWith((x, y) => (comparer.Compare(x, y) * -1), xs);
}
export function sortByDescending(projection, xs, comparer) {
    return sortWith((x, y) => (comparer.Compare(projection(x), projection(y)) * -1), xs);
}
export function sum(xs, adder) {
    return fold((acc, x) => adder.Add(acc, x), adder.GetZero(), xs);
}
export function sumBy(f, xs, adder) {
    return fold((acc, x) => adder.Add(acc, f(x)), adder.GetZero(), xs);
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
export function average(xs, averager) {
    let count = 0;
    const total = fold((acc, x) => {
        count = ((count + 1) | 0);
        return averager.Add(acc, x);
    }, averager.GetZero(), xs);
    return averager.DivideByInt(total, count);
}
export function averageBy(f, xs, averager) {
    let count = 0;
    const total = fold((acc, x) => {
        count = ((count + 1) | 0);
        return averager.Add(acc, f(x));
    }, averager.GetZero(), xs);
    return averager.DivideByInt(total, count);
}
export function permute(f, xs) {
    return ofArray(permute_1(f, toArray(xs)));
}
export function chunkBySize(chunkSize, xs) {
    return ofArray(map_1(ofArray, chunkBySize_1(chunkSize, toArray(xs))));
}
export function allPairs(xs, ys) {
    const root = FSharpList_get_Empty();
    let node = root;
    iterate((x) => {
        iterate((y) => {
            let xs_1, t;
            node = ((xs_1 = node, (t = (new FSharpList([x, y], void 0)), (xs_1.tail = t, t))));
        }, ys);
    }, xs);
    const xs_3 = node;
    const t_2 = FSharpList_get_Empty();
    xs_3.tail = t_2;
    return FSharpList__get_Tail(root);
}
export function skip(count_mut, xs_mut) {
    skip: while (true) {
        const count = count_mut, xs = xs_mut;
        if (count <= 0) {
            return xs;
        }
        else if (FSharpList__get_IsEmpty(xs)) {
            throw new Error((SR_notEnoughElements + "\\nParameter name: ") + "list");
        }
        else {
            count_mut = (count - 1);
            xs_mut = FSharpList__get_Tail(xs);
            continue skip;
        }
        break;
    }
}
export function skipWhile(predicate_mut, xs_mut) {
    skipWhile: while (true) {
        const predicate = predicate_mut, xs = xs_mut;
        if (FSharpList__get_IsEmpty(xs)) {
            return xs;
        }
        else if (!predicate(FSharpList__get_Head(xs))) {
            return xs;
        }
        else {
            predicate_mut = predicate;
            xs_mut = FSharpList__get_Tail(xs);
            continue skipWhile;
        }
        break;
    }
}
export function take(count, xs) {
    if (count < 0) {
        throw new Error((SR_inputMustBeNonNegative + "\\nParameter name: ") + "count");
    }
    const loop = (i_mut, acc_mut, xs_1_mut) => {
        let t;
        loop: while (true) {
            const i = i_mut, acc = acc_mut, xs_1 = xs_1_mut;
            if (i <= 0) {
                return acc;
            }
            else if (FSharpList__get_IsEmpty(xs_1)) {
                throw new Error((SR_notEnoughElements + "\\nParameter name: ") + "list");
            }
            else {
                i_mut = (i - 1);
                acc_mut = ((t = (new FSharpList(FSharpList__get_Head(xs_1), void 0)), (acc.tail = t, t)));
                xs_1_mut = FSharpList__get_Tail(xs_1);
                continue loop;
            }
            break;
        }
    };
    const root = FSharpList_get_Empty();
    const node = loop(count, root, xs);
    const t_2 = FSharpList_get_Empty();
    node.tail = t_2;
    return FSharpList__get_Tail(root);
}
export function takeWhile(predicate, xs) {
    const loop = (acc_mut, xs_1_mut) => {
        let t;
        loop: while (true) {
            const acc = acc_mut, xs_1 = xs_1_mut;
            if (FSharpList__get_IsEmpty(xs_1)) {
                return acc;
            }
            else if (!predicate(FSharpList__get_Head(xs_1))) {
                return acc;
            }
            else {
                acc_mut = ((t = (new FSharpList(FSharpList__get_Head(xs_1), void 0)), (acc.tail = t, t)));
                xs_1_mut = FSharpList__get_Tail(xs_1);
                continue loop;
            }
            break;
        }
    };
    const root = FSharpList_get_Empty();
    const node = loop(root, xs);
    const t_2 = FSharpList_get_Empty();
    node.tail = t_2;
    return FSharpList__get_Tail(root);
}
export function truncate(count, xs) {
    const loop = (i_mut, acc_mut, xs_1_mut) => {
        let t;
        loop: while (true) {
            const i = i_mut, acc = acc_mut, xs_1 = xs_1_mut;
            if (i <= 0) {
                return acc;
            }
            else if (FSharpList__get_IsEmpty(xs_1)) {
                return acc;
            }
            else {
                i_mut = (i - 1);
                acc_mut = ((t = (new FSharpList(FSharpList__get_Head(xs_1), void 0)), (acc.tail = t, t)));
                xs_1_mut = FSharpList__get_Tail(xs_1);
                continue loop;
            }
            break;
        }
    };
    const root = FSharpList_get_Empty();
    const node = loop(count, root, xs);
    const t_2 = FSharpList_get_Empty();
    node.tail = t_2;
    return FSharpList__get_Tail(root);
}
export function getSlice(startIndex, endIndex, xs) {
    const len = length(xs) | 0;
    let startIndex_1;
    const index = defaultArg(startIndex, 0) | 0;
    startIndex_1 = ((index < 0) ? 0 : index);
    let endIndex_1;
    const index_1 = defaultArg(endIndex, len - 1) | 0;
    endIndex_1 = ((index_1 >= len) ? (len - 1) : index_1);
    if (endIndex_1 < startIndex_1) {
        return FSharpList_get_Empty();
    }
    else {
        return take((endIndex_1 - startIndex_1) + 1, skip(startIndex_1, xs));
    }
}
export function splitAt(index, xs) {
    if (index < 0) {
        throw new Error((SR_inputMustBeNonNegative + "\\nParameter name: ") + "index");
    }
    if (index > FSharpList__get_Length(xs)) {
        throw new Error((SR_notEnoughElements + "\\nParameter name: ") + "index");
    }
    return [take(index, xs), skip(index, xs)];
}
export function exactlyOne(xs) {
    if (FSharpList__get_IsEmpty(xs)) {
        throw new Error((SR_inputSequenceEmpty + "\\nParameter name: ") + "list");
    }
    else if (FSharpList__get_IsEmpty(FSharpList__get_Tail(xs))) {
        return FSharpList__get_Head(xs);
    }
    else {
        throw new Error((SR_inputSequenceTooLong + "\\nParameter name: ") + "list");
    }
}
export function tryExactlyOne(xs) {
    if (!FSharpList__get_IsEmpty(xs) && FSharpList__get_IsEmpty(FSharpList__get_Tail(xs))) {
        return some(FSharpList__get_Head(xs));
    }
    else {
        return void 0;
    }
}
export function where(predicate, xs) {
    return filter(predicate, xs);
}
export function pairwise(xs) {
    return ofArray(pairwise_1(toArray(xs)));
}
export function windowed(windowSize, xs) {
    return ofArray(map_1(ofArray, windowed_1(windowSize, toArray(xs))));
}
export function splitInto(chunks, xs) {
    return ofArray(map_1(ofArray, splitInto_1(chunks, toArray(xs))));
}
export function transpose(lists) {
    return ofArray(map_1(ofArray, transpose_1(map_1(toArray, Array.from(lists)))));
}
export function insertAt(index, y, xs) {
    let i = -1;
    let isDone = false;
    const result = fold((acc, x) => {
        i = ((i + 1) | 0);
        if (i === index) {
            isDone = true;
            return FSharpList_Cons_305B8EAC(x, FSharpList_Cons_305B8EAC(y, acc));
        }
        else {
            return FSharpList_Cons_305B8EAC(x, acc);
        }
    }, FSharpList_get_Empty(), xs);
    return reverse(isDone ? result : (((i + 1) === index) ? FSharpList_Cons_305B8EAC(y, result) : (() => {
        throw new Error((SR_indexOutOfBounds + "\\nParameter name: ") + "index");
    })()));
}
export function insertManyAt(index, ys, xs) {
    let i = -1;
    let isDone = false;
    const ys_1 = ofSeq(ys);
    const result = fold((acc, x) => {
        i = ((i + 1) | 0);
        if (i === index) {
            isDone = true;
            return FSharpList_Cons_305B8EAC(x, append(ys_1, acc));
        }
        else {
            return FSharpList_Cons_305B8EAC(x, acc);
        }
    }, FSharpList_get_Empty(), xs);
    return reverse(isDone ? result : (((i + 1) === index) ? append(ys_1, result) : (() => {
        throw new Error((SR_indexOutOfBounds + "\\nParameter name: ") + "index");
    })()));
}
export function removeAt(index, xs) {
    let i = -1;
    let isDone = false;
    const ys = filter((_arg) => {
        i = ((i + 1) | 0);
        if (i === index) {
            isDone = true;
            return false;
        }
        else {
            return true;
        }
    }, xs);
    if (!isDone) {
        throw new Error((SR_indexOutOfBounds + "\\nParameter name: ") + "index");
    }
    return ys;
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
export function updateAt(index, y, xs) {
    let isDone = false;
    const ys = mapIndexed((i, x) => {
        if (i === index) {
            isDone = true;
            return y;
        }
        else {
            return x;
        }
    }, xs);
    if (!isDone) {
        throw new Error((SR_indexOutOfBounds + "\\nParameter name: ") + "index");
    }
    return ys;
}
