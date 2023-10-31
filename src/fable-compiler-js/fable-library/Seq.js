import { clear, defaultOf, equals, isDisposable, isArrayLike, toIterator, disposeSafe, getEnumerator } from "./Util.js";
import { toString } from "./Types.js";
import { class_type } from "./Reflection.js";
import { some, value as value_1 } from "./Option.js";
import { Operators_Lock, Operators_NullArg } from "./FSharp.Core.js";
import { chunkBySize as chunkBySize_1, permute as permute_1, transpose as transpose_1, map as map_1, windowed as windowed_1, splitInto as splitInto_1, pairwise as pairwise_1, scanBack as scanBack_1, reverse as reverse_1, mapFoldBack as mapFoldBack_1, mapFold as mapFold_1, tryItem as tryItem_1, tryHead as tryHead_1, foldBack2 as foldBack2_1, foldBack as foldBack_1, tryFindIndexBack as tryFindIndexBack_1, tryFindBack as tryFindBack_1, singleton as singleton_1 } from "./Array.js";
import { length as length_1, tryItem as tryItem_2, isEmpty as isEmpty_1, tryHead as tryHead_2, ofSeq as ofSeq_1, ofArray as ofArray_1, toArray as toArray_1, FSharpList } from "./List.js";
import { SR_indexOutOfBounds } from "./Global.js";
export const SR_enumerationAlreadyFinished = "Enumeration already finished.";
export const SR_enumerationNotStarted = "Enumeration has not started. Call MoveNext.";
export const SR_inputSequenceEmpty = "The input sequence was empty.";
export const SR_inputSequenceTooLong = "The input sequence contains more than one element.";
export const SR_keyNotFoundAlt = "An index satisfying the predicate was not found in the collection.";
export const SR_notEnoughElements = "The input sequence has an insufficient number of elements.";
export const SR_resetNotSupported = "Reset is not supported on this enumerator.";
export function Enumerator_noReset() {
    throw new Error(SR_resetNotSupported);
}
export function Enumerator_notStarted() {
    throw new Error(SR_enumerationNotStarted);
}
export function Enumerator_alreadyFinished() {
    throw new Error(SR_enumerationAlreadyFinished);
}
export class Enumerator_Seq {
    constructor(f) {
        this.f = f;
    }
    toString() {
        const xs = this;
        let i = 0;
        let str = "seq [";
        const e = getEnumerator(xs);
        try {
            while ((i < 4) && e["System.Collections.IEnumerator.MoveNext"]()) {
                if (i > 0) {
                    str = (str + "; ");
                }
                str = (str + toString(e["System.Collections.Generic.IEnumerator`1.get_Current"]()));
                i = ((i + 1) | 0);
            }
            if (i === 4) {
                str = (str + "; ...");
            }
            return str + "]";
        }
        finally {
            disposeSafe(e);
        }
    }
    GetEnumerator() {
        const x = this;
        return x.f();
    }
    [Symbol.iterator]() {
        return toIterator(getEnumerator(this));
    }
    "System.Collections.IEnumerable.GetEnumerator"() {
        const x = this;
        return x.f();
    }
}
export function Enumerator_Seq_$reflection(gen0) {
    return class_type("SeqModule.Enumerator.Seq", [gen0], Enumerator_Seq);
}
export function Enumerator_Seq_$ctor_673A07F2(f) {
    return new Enumerator_Seq(f);
}
export class Enumerator_FromFunctions$1 {
    constructor(current, next, dispose) {
        this.current = current;
        this.next = next;
        this.dispose = dispose;
    }
    "System.Collections.Generic.IEnumerator`1.get_Current"() {
        const _ = this;
        return _.current();
    }
    "System.Collections.IEnumerator.get_Current"() {
        const _ = this;
        return _.current();
    }
    "System.Collections.IEnumerator.MoveNext"() {
        const _ = this;
        return _.next();
    }
    "System.Collections.IEnumerator.Reset"() {
        Enumerator_noReset();
    }
    Dispose() {
        const _ = this;
        _.dispose();
    }
}
export function Enumerator_FromFunctions$1_$reflection(gen0) {
    return class_type("SeqModule.Enumerator.FromFunctions`1", [gen0], Enumerator_FromFunctions$1);
}
export function Enumerator_FromFunctions$1_$ctor_58C54629(current, next, dispose) {
    return new Enumerator_FromFunctions$1(current, next, dispose);
}
export function Enumerator_cast(e) {
    return Enumerator_FromFunctions$1_$ctor_58C54629(() => e["System.Collections.Generic.IEnumerator`1.get_Current"](), () => e["System.Collections.IEnumerator.MoveNext"](), () => {
        const e_1 = e;
        disposeSafe(e_1);
    });
}
export function Enumerator_concat(sources) {
    let outerOpt = void 0;
    let innerOpt = void 0;
    let started = false;
    let finished = false;
    let curr = void 0;
    const finish = () => {
        finished = true;
        if (innerOpt != null) {
            const inner = value_1(innerOpt);
            try {
                disposeSafe(inner);
            }
            finally {
                innerOpt = void 0;
            }
        }
        if (outerOpt != null) {
            const outer = value_1(outerOpt);
            try {
                disposeSafe(outer);
            }
            finally {
                outerOpt = void 0;
            }
        }
    };
    return Enumerator_FromFunctions$1_$ctor_58C54629(() => {
        if (!started) {
            Enumerator_notStarted();
        }
        else if (finished) {
            Enumerator_alreadyFinished();
        }
        if (curr != null) {
            return value_1(curr);
        }
        else {
            return Enumerator_alreadyFinished();
        }
    }, () => {
        let copyOfStruct;
        if (!started) {
            started = true;
        }
        if (finished) {
            return false;
        }
        else {
            let res = void 0;
            while (res == null) {
                const outerOpt_1 = outerOpt;
                const innerOpt_1 = innerOpt;
                if (outerOpt_1 != null) {
                    if (innerOpt_1 != null) {
                        const inner_1 = value_1(innerOpt_1);
                        if (inner_1["System.Collections.IEnumerator.MoveNext"]()) {
                            curr = some(inner_1["System.Collections.Generic.IEnumerator`1.get_Current"]());
                            res = true;
                        }
                        else {
                            try {
                                disposeSafe(inner_1);
                            }
                            finally {
                                innerOpt = void 0;
                            }
                        }
                    }
                    else {
                        const outer_1 = value_1(outerOpt_1);
                        if (outer_1["System.Collections.IEnumerator.MoveNext"]()) {
                            const ie = outer_1["System.Collections.Generic.IEnumerator`1.get_Current"]();
                            innerOpt = ((copyOfStruct = ie, getEnumerator(copyOfStruct)));
                        }
                        else {
                            finish();
                            res = false;
                        }
                    }
                }
                else {
                    outerOpt = getEnumerator(sources);
                }
            }
            return value_1(res);
        }
    }, () => {
        if (!finished) {
            finish();
        }
    });
}
export function Enumerator_enumerateThenFinally(f, e) {
    return Enumerator_FromFunctions$1_$ctor_58C54629(() => e["System.Collections.Generic.IEnumerator`1.get_Current"](), () => e["System.Collections.IEnumerator.MoveNext"](), () => {
        try {
            disposeSafe(e);
        }
        finally {
            f();
        }
    });
}
export function Enumerator_generateWhileSome(openf, compute, closef) {
    let started = false;
    let curr = void 0;
    let state = some(openf());
    const dispose = () => {
        if (state != null) {
            const x_1 = value_1(state);
            try {
                closef(x_1);
            }
            finally {
                state = void 0;
            }
        }
    };
    const finish = () => {
        try {
            dispose();
        }
        finally {
            curr = void 0;
        }
    };
    return Enumerator_FromFunctions$1_$ctor_58C54629(() => {
        if (!started) {
            Enumerator_notStarted();
        }
        if (curr != null) {
            return value_1(curr);
        }
        else {
            return Enumerator_alreadyFinished();
        }
    }, () => {
        if (!started) {
            started = true;
        }
        if (state != null) {
            const s = value_1(state);
            let matchValue_1;
            try {
                matchValue_1 = compute(s);
            }
            catch (matchValue) {
                finish();
                throw matchValue;
            }
            if (matchValue_1 != null) {
                curr = matchValue_1;
                return true;
            }
            else {
                finish();
                return false;
            }
        }
        else {
            return false;
        }
    }, dispose);
}
export function Enumerator_unfold(f, state) {
    let curr = void 0;
    let acc = state;
    return Enumerator_FromFunctions$1_$ctor_58C54629(() => {
        if (curr != null) {
            const x = value_1(curr)[0];
            const st = value_1(curr)[1];
            return x;
        }
        else {
            return Enumerator_notStarted();
        }
    }, () => {
        curr = f(acc);
        if (curr != null) {
            const x_1 = value_1(curr)[0];
            const st_1 = value_1(curr)[1];
            acc = st_1;
            return true;
        }
        else {
            return false;
        }
    }, () => {
    });
}
export function indexNotFound() {
    throw new Error(SR_keyNotFoundAlt);
}
export function checkNonNull(argName, arg) {
    if (arg == null) {
        Operators_NullArg(argName);
    }
}
export function mkSeq(f) {
    return Enumerator_Seq_$ctor_673A07F2(f);
}
export function ofSeq(xs) {
    checkNonNull("source", xs);
    return getEnumerator(xs);
}
export function delay(generator) {
    return mkSeq(() => getEnumerator(generator()));
}
export function concat(sources) {
    return mkSeq(() => Enumerator_concat(sources));
}
export function unfold(generator, state) {
    return mkSeq(() => Enumerator_unfold(generator, state));
}
export function empty() {
    return delay(() => (new Array(0)));
}
export function singleton(x) {
    return delay(() => singleton_1(x));
}
export function ofArray(arr) {
    return arr;
}
export function toArray(xs) {
    if (xs instanceof FSharpList) {
        const a = xs;
        return toArray_1(a);
    }
    else {
        return Array.from(xs);
    }
}
export function ofList(xs) {
    return xs;
}
export function toList(xs) {
    if (isArrayLike(xs)) {
        return ofArray_1(xs);
    }
    else if (xs instanceof FSharpList) {
        return xs;
    }
    else {
        return ofSeq_1(xs);
    }
}
export function generate(create, compute, dispose) {
    return mkSeq(() => Enumerator_generateWhileSome(create, compute, dispose));
}
export function generateIndexed(create, compute, dispose) {
    return mkSeq(() => {
        let i = -1;
        return Enumerator_generateWhileSome(create, (x) => {
            i = ((i + 1) | 0);
            return compute(i, x);
        }, dispose);
    });
}
export function append(xs, ys) {
    return concat([xs, ys]);
}
export function cast(xs) {
    return mkSeq(() => {
        checkNonNull("source", xs);
        return Enumerator_cast(getEnumerator(xs));
    });
}
export function choose(chooser, xs) {
    return generate(() => ofSeq(xs), (e) => {
        let curr = void 0;
        while ((curr == null) && e["System.Collections.IEnumerator.MoveNext"]()) {
            curr = chooser(e["System.Collections.Generic.IEnumerator`1.get_Current"]());
        }
        return curr;
    }, (e_1) => {
        disposeSafe(e_1);
    });
}
export function compareWith(comparer, xs, ys) {
    const e1 = ofSeq(xs);
    try {
        const e2 = ofSeq(ys);
        try {
            let c = 0;
            let b1 = e1["System.Collections.IEnumerator.MoveNext"]();
            let b2 = e2["System.Collections.IEnumerator.MoveNext"]();
            while (((c === 0) && b1) && b2) {
                c = (comparer(e1["System.Collections.Generic.IEnumerator`1.get_Current"](), e2["System.Collections.Generic.IEnumerator`1.get_Current"]()) | 0);
                if (c === 0) {
                    b1 = e1["System.Collections.IEnumerator.MoveNext"]();
                    b2 = e2["System.Collections.IEnumerator.MoveNext"]();
                }
            }
            return ((c !== 0) ? c : (b1 ? 1 : (b2 ? -1 : 0))) | 0;
        }
        finally {
            disposeSafe(e2);
        }
    }
    finally {
        disposeSafe(e1);
    }
}
export function contains(value, xs, comparer) {
    const e = ofSeq(xs);
    try {
        let found = false;
        while (!found && e["System.Collections.IEnumerator.MoveNext"]()) {
            found = comparer.Equals(value, e["System.Collections.Generic.IEnumerator`1.get_Current"]());
        }
        return found;
    }
    finally {
        disposeSafe(e);
    }
}
export function enumerateFromFunctions(create, moveNext, current) {
    return generate(create, (x) => (moveNext(x) ? some(current(x)) : void 0), (x_1) => {
        const matchValue = x_1;
        if (isDisposable(matchValue)) {
            const id = matchValue;
            disposeSafe(id);
        }
    });
}
export function enumerateThenFinally(source, compensation) {
    const compensation_1 = compensation;
    return mkSeq(() => {
        try {
            return Enumerator_enumerateThenFinally(compensation_1, ofSeq(source));
        }
        catch (matchValue) {
            compensation_1();
            throw matchValue;
        }
    });
}
export function enumerateUsing(resource, source) {
    const compensation = () => {
        if (equals(resource, defaultOf())) {
        }
        else {
            let copyOfStruct = resource;
            disposeSafe(copyOfStruct);
        }
    };
    return mkSeq(() => {
        try {
            return Enumerator_enumerateThenFinally(compensation, ofSeq(source(resource)));
        }
        catch (matchValue_1) {
            compensation();
            throw matchValue_1;
        }
    });
}
export function enumerateWhile(guard, xs) {
    return concat(unfold((i) => (guard() ? [xs, i + 1] : void 0), 0));
}
export function filter(f, xs) {
    return choose((x) => {
        if (f(x)) {
            return some(x);
        }
        else {
            return void 0;
        }
    }, xs);
}
export function exists(predicate, xs) {
    const e = ofSeq(xs);
    try {
        let found = false;
        while (!found && e["System.Collections.IEnumerator.MoveNext"]()) {
            found = predicate(e["System.Collections.Generic.IEnumerator`1.get_Current"]());
        }
        return found;
    }
    finally {
        disposeSafe(e);
    }
}
export function exists2(predicate, xs, ys) {
    const e1 = ofSeq(xs);
    try {
        const e2 = ofSeq(ys);
        try {
            let found = false;
            while ((!found && e1["System.Collections.IEnumerator.MoveNext"]()) && e2["System.Collections.IEnumerator.MoveNext"]()) {
                found = predicate(e1["System.Collections.Generic.IEnumerator`1.get_Current"](), e2["System.Collections.Generic.IEnumerator`1.get_Current"]());
            }
            return found;
        }
        finally {
            disposeSafe(e2);
        }
    }
    finally {
        disposeSafe(e1);
    }
}
export function exactlyOne(xs) {
    const e = ofSeq(xs);
    try {
        if (e["System.Collections.IEnumerator.MoveNext"]()) {
            const v = e["System.Collections.Generic.IEnumerator`1.get_Current"]();
            if (e["System.Collections.IEnumerator.MoveNext"]()) {
                throw new Error((SR_inputSequenceTooLong + "\\nParameter name: ") + "source");
            }
            else {
                return v;
            }
        }
        else {
            throw new Error((SR_inputSequenceEmpty + "\\nParameter name: ") + "source");
        }
    }
    finally {
        disposeSafe(e);
    }
}
export function tryExactlyOne(xs) {
    const e = ofSeq(xs);
    try {
        if (e["System.Collections.IEnumerator.MoveNext"]()) {
            const v = e["System.Collections.Generic.IEnumerator`1.get_Current"]();
            return e["System.Collections.IEnumerator.MoveNext"]() ? void 0 : some(v);
        }
        else {
            return void 0;
        }
    }
    finally {
        disposeSafe(e);
    }
}
export function tryFind(predicate, xs) {
    const e = ofSeq(xs);
    try {
        let res = void 0;
        while ((res == null) && e["System.Collections.IEnumerator.MoveNext"]()) {
            const c = e["System.Collections.Generic.IEnumerator`1.get_Current"]();
            if (predicate(c)) {
                res = some(c);
            }
        }
        return res;
    }
    finally {
        disposeSafe(e);
    }
}
export function find(predicate, xs) {
    const matchValue = tryFind(predicate, xs);
    if (matchValue == null) {
        return indexNotFound();
    }
    else {
        return value_1(matchValue);
    }
}
export function tryFindBack(predicate, xs) {
    return tryFindBack_1(predicate, toArray(xs));
}
export function findBack(predicate, xs) {
    const matchValue = tryFindBack(predicate, xs);
    if (matchValue == null) {
        return indexNotFound();
    }
    else {
        return value_1(matchValue);
    }
}
export function tryFindIndex(predicate, xs) {
    const e = ofSeq(xs);
    try {
        const loop = (i_mut) => {
            loop: while (true) {
                const i = i_mut;
                if (e["System.Collections.IEnumerator.MoveNext"]()) {
                    if (predicate(e["System.Collections.Generic.IEnumerator`1.get_Current"]())) {
                        return i;
                    }
                    else {
                        i_mut = (i + 1);
                        continue loop;
                    }
                }
                else {
                    return void 0;
                }
                break;
            }
        };
        return loop(0);
    }
    finally {
        disposeSafe(e);
    }
}
export function findIndex(predicate, xs) {
    const matchValue = tryFindIndex(predicate, xs);
    if (matchValue == null) {
        indexNotFound();
        return -1;
    }
    else {
        return value_1(matchValue) | 0;
    }
}
export function tryFindIndexBack(predicate, xs) {
    return tryFindIndexBack_1(predicate, toArray(xs));
}
export function findIndexBack(predicate, xs) {
    const matchValue = tryFindIndexBack(predicate, xs);
    if (matchValue == null) {
        indexNotFound();
        return -1;
    }
    else {
        return value_1(matchValue) | 0;
    }
}
export function fold(folder, state, xs) {
    const e = ofSeq(xs);
    try {
        let acc = state;
        while (e["System.Collections.IEnumerator.MoveNext"]()) {
            acc = folder(acc, e["System.Collections.Generic.IEnumerator`1.get_Current"]());
        }
        return acc;
    }
    finally {
        disposeSafe(e);
    }
}
export function foldBack(folder, xs, state) {
    return foldBack_1(folder, toArray(xs), state);
}
export function fold2(folder, state, xs, ys) {
    const e1 = ofSeq(xs);
    try {
        const e2 = ofSeq(ys);
        try {
            let acc = state;
            while (e1["System.Collections.IEnumerator.MoveNext"]() && e2["System.Collections.IEnumerator.MoveNext"]()) {
                acc = folder(acc, e1["System.Collections.Generic.IEnumerator`1.get_Current"](), e2["System.Collections.Generic.IEnumerator`1.get_Current"]());
            }
            return acc;
        }
        finally {
            disposeSafe(e2);
        }
    }
    finally {
        disposeSafe(e1);
    }
}
export function foldBack2(folder, xs, ys, state) {
    return foldBack2_1(folder, toArray(xs), toArray(ys), state);
}
export function forAll(predicate, xs) {
    return !exists((x) => !predicate(x), xs);
}
export function forAll2(predicate, xs, ys) {
    return !exists2((x, y) => !predicate(x, y), xs, ys);
}
export function tryHead(xs) {
    if (isArrayLike(xs)) {
        return tryHead_1(xs);
    }
    else if (xs instanceof FSharpList) {
        return tryHead_2(xs);
    }
    else {
        const e = ofSeq(xs);
        try {
            return e["System.Collections.IEnumerator.MoveNext"]() ? some(e["System.Collections.Generic.IEnumerator`1.get_Current"]()) : void 0;
        }
        finally {
            disposeSafe(e);
        }
    }
}
export function head(xs) {
    const matchValue = tryHead(xs);
    if (matchValue == null) {
        throw new Error((SR_inputSequenceEmpty + "\\nParameter name: ") + "source");
    }
    else {
        return value_1(matchValue);
    }
}
export function initialize(count, f) {
    return unfold((i) => ((i < count) ? [f(i), i + 1] : void 0), 0);
}
export function initializeInfinite(f) {
    return initialize(2147483647, f);
}
export function isEmpty(xs) {
    if (isArrayLike(xs)) {
        const a = xs;
        return a.length === 0;
    }
    else if (xs instanceof FSharpList) {
        return isEmpty_1(xs);
    }
    else {
        const e = ofSeq(xs);
        try {
            return !e["System.Collections.IEnumerator.MoveNext"]();
        }
        finally {
            disposeSafe(e);
        }
    }
}
export function tryItem(index, xs) {
    if (isArrayLike(xs)) {
        return tryItem_1(index, xs);
    }
    else if (xs instanceof FSharpList) {
        return tryItem_2(index, xs);
    }
    else {
        const e = ofSeq(xs);
        try {
            const loop = (index_1_mut) => {
                loop: while (true) {
                    const index_1 = index_1_mut;
                    if (!e["System.Collections.IEnumerator.MoveNext"]()) {
                        return void 0;
                    }
                    else if (index_1 === 0) {
                        return some(e["System.Collections.Generic.IEnumerator`1.get_Current"]());
                    }
                    else {
                        index_1_mut = (index_1 - 1);
                        continue loop;
                    }
                    break;
                }
            };
            return loop(index);
        }
        finally {
            disposeSafe(e);
        }
    }
}
export function item(index, xs) {
    const matchValue = tryItem(index, xs);
    if (matchValue == null) {
        throw new Error((SR_notEnoughElements + "\\nParameter name: ") + "index");
    }
    else {
        return value_1(matchValue);
    }
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
export function tryLast(xs) {
    const e = ofSeq(xs);
    try {
        const loop = (acc_mut) => {
            loop: while (true) {
                const acc = acc_mut;
                if (!e["System.Collections.IEnumerator.MoveNext"]()) {
                    return acc;
                }
                else {
                    acc_mut = e["System.Collections.Generic.IEnumerator`1.get_Current"]();
                    continue loop;
                }
                break;
            }
        };
        return e["System.Collections.IEnumerator.MoveNext"]() ? some(loop(e["System.Collections.Generic.IEnumerator`1.get_Current"]())) : void 0;
    }
    finally {
        disposeSafe(e);
    }
}
export function last(xs) {
    const matchValue = tryLast(xs);
    if (matchValue == null) {
        throw new Error((SR_notEnoughElements + "\\nParameter name: ") + "source");
    }
    else {
        return value_1(matchValue);
    }
}
export function length(xs) {
    if (isArrayLike(xs)) {
        const a = xs;
        return a.length | 0;
    }
    else if (xs instanceof FSharpList) {
        return length_1(xs) | 0;
    }
    else {
        const e = ofSeq(xs);
        try {
            let count = 0;
            while (e["System.Collections.IEnumerator.MoveNext"]()) {
                count = ((count + 1) | 0);
            }
            return count | 0;
        }
        finally {
            disposeSafe(e);
        }
    }
}
export function map(mapping, xs) {
    return generate(() => ofSeq(xs), (e) => (e["System.Collections.IEnumerator.MoveNext"]() ? some(mapping(e["System.Collections.Generic.IEnumerator`1.get_Current"]())) : void 0), (e_1) => {
        disposeSafe(e_1);
    });
}
export function mapIndexed(mapping, xs) {
    return generateIndexed(() => ofSeq(xs), (i, e) => (e["System.Collections.IEnumerator.MoveNext"]() ? some(mapping(i, e["System.Collections.Generic.IEnumerator`1.get_Current"]())) : void 0), (e_1) => {
        disposeSafe(e_1);
    });
}
export function indexed(xs) {
    return mapIndexed((i, x) => [i, x], xs);
}
export function map2(mapping, xs, ys) {
    return generate(() => [ofSeq(xs), ofSeq(ys)], (tupledArg) => {
        const e1 = tupledArg[0];
        const e2 = tupledArg[1];
        return (e1["System.Collections.IEnumerator.MoveNext"]() && e2["System.Collections.IEnumerator.MoveNext"]()) ? some(mapping(e1["System.Collections.Generic.IEnumerator`1.get_Current"](), e2["System.Collections.Generic.IEnumerator`1.get_Current"]())) : void 0;
    }, (tupledArg_1) => {
        try {
            disposeSafe(tupledArg_1[0]);
        }
        finally {
            disposeSafe(tupledArg_1[1]);
        }
    });
}
export function mapIndexed2(mapping, xs, ys) {
    return generateIndexed(() => [ofSeq(xs), ofSeq(ys)], (i, tupledArg) => {
        const e1 = tupledArg[0];
        const e2 = tupledArg[1];
        return (e1["System.Collections.IEnumerator.MoveNext"]() && e2["System.Collections.IEnumerator.MoveNext"]()) ? some(mapping(i, e1["System.Collections.Generic.IEnumerator`1.get_Current"](), e2["System.Collections.Generic.IEnumerator`1.get_Current"]())) : void 0;
    }, (tupledArg_1) => {
        try {
            disposeSafe(tupledArg_1[0]);
        }
        finally {
            disposeSafe(tupledArg_1[1]);
        }
    });
}
export function map3(mapping, xs, ys, zs) {
    return generate(() => [ofSeq(xs), ofSeq(ys), ofSeq(zs)], (tupledArg) => {
        const e1 = tupledArg[0];
        const e2 = tupledArg[1];
        const e3 = tupledArg[2];
        return ((e1["System.Collections.IEnumerator.MoveNext"]() && e2["System.Collections.IEnumerator.MoveNext"]()) && e3["System.Collections.IEnumerator.MoveNext"]()) ? some(mapping(e1["System.Collections.Generic.IEnumerator`1.get_Current"](), e2["System.Collections.Generic.IEnumerator`1.get_Current"](), e3["System.Collections.Generic.IEnumerator`1.get_Current"]())) : void 0;
    }, (tupledArg_1) => {
        try {
            disposeSafe(tupledArg_1[0]);
        }
        finally {
            try {
                disposeSafe(tupledArg_1[1]);
            }
            finally {
                disposeSafe(tupledArg_1[2]);
            }
        }
    });
}
export function readOnly(xs) {
    checkNonNull("source", xs);
    return map((x) => x, xs);
}
export class CachedSeq$1 {
    constructor(cleanup, res) {
        this.cleanup = cleanup;
        this.res = res;
    }
    Dispose() {
        const _ = this;
        _.cleanup();
    }
    GetEnumerator() {
        const _ = this;
        return getEnumerator(_.res);
    }
    [Symbol.iterator]() {
        return toIterator(getEnumerator(this));
    }
    "System.Collections.IEnumerable.GetEnumerator"() {
        const _ = this;
        return getEnumerator(_.res);
    }
}
export function CachedSeq$1_$reflection(gen0) {
    return class_type("SeqModule.CachedSeq`1", [gen0], CachedSeq$1);
}
export function CachedSeq$1_$ctor_Z7A8347D4(cleanup, res) {
    return new CachedSeq$1(cleanup, res);
}
export function CachedSeq$1__Clear(_) {
    _.cleanup();
}
export function cache(source) {
    checkNonNull("source", source);
    const prefix = [];
    let enumeratorR = void 0;
    return CachedSeq$1_$ctor_Z7A8347D4(() => {
        Operators_Lock(prefix, () => {
            clear(prefix);
            let matchResult, e;
            if (enumeratorR != null) {
                if (value_1(enumeratorR) != null) {
                    matchResult = 0;
                    e = value_1(value_1(enumeratorR));
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
                    disposeSafe(e);
                    break;
                }
            }
            enumeratorR = void 0;
        });
    }, unfold((i_1) => Operators_Lock(prefix, () => {
        if (i_1 < prefix.length) {
            return [prefix[i_1], i_1 + 1];
        }
        else {
            if (i_1 >= prefix.length) {
                let optEnumerator_2;
                if (enumeratorR != null) {
                    optEnumerator_2 = value_1(enumeratorR);
                }
                else {
                    const optEnumerator = getEnumerator(source);
                    enumeratorR = some(optEnumerator);
                    optEnumerator_2 = optEnumerator;
                }
                if (optEnumerator_2 == null) {
                }
                else {
                    const enumerator = value_1(optEnumerator_2);
                    if (enumerator["System.Collections.IEnumerator.MoveNext"]()) {
                        void (prefix.push(enumerator["System.Collections.Generic.IEnumerator`1.get_Current"]()));
                    }
                    else {
                        disposeSafe(enumerator);
                        enumeratorR = some(void 0);
                    }
                }
            }
            if (i_1 < prefix.length) {
                return [prefix[i_1], i_1 + 1];
            }
            else {
                return void 0;
            }
        }
    }), 0));
}
export function allPairs(xs, ys) {
    const ysCache = cache(ys);
    return delay(() => concat(map((x) => map((y) => [x, y], ysCache), xs)));
}
export function mapFold(mapping, state, xs) {
    const patternInput = mapFold_1(mapping, state, toArray(xs));
    return [readOnly(patternInput[0]), patternInput[1]];
}
export function mapFoldBack(mapping, xs, state) {
    const patternInput = mapFoldBack_1(mapping, toArray(xs), state);
    return [readOnly(patternInput[0]), patternInput[1]];
}
export function tryPick(chooser, xs) {
    const e = ofSeq(xs);
    try {
        let res = void 0;
        while ((res == null) && e["System.Collections.IEnumerator.MoveNext"]()) {
            res = chooser(e["System.Collections.Generic.IEnumerator`1.get_Current"]());
        }
        return res;
    }
    finally {
        disposeSafe(e);
    }
}
export function pick(chooser, xs) {
    const matchValue = tryPick(chooser, xs);
    if (matchValue == null) {
        return indexNotFound();
    }
    else {
        return value_1(matchValue);
    }
}
export function reduce(folder, xs) {
    const e = ofSeq(xs);
    try {
        const loop = (acc_mut) => {
            loop: while (true) {
                const acc = acc_mut;
                if (e["System.Collections.IEnumerator.MoveNext"]()) {
                    acc_mut = folder(acc, e["System.Collections.Generic.IEnumerator`1.get_Current"]());
                    continue loop;
                }
                else {
                    return acc;
                }
                break;
            }
        };
        if (e["System.Collections.IEnumerator.MoveNext"]()) {
            return loop(e["System.Collections.Generic.IEnumerator`1.get_Current"]());
        }
        else {
            throw new Error(SR_inputSequenceEmpty);
        }
    }
    finally {
        disposeSafe(e);
    }
}
export function reduceBack(folder, xs) {
    const arr = toArray(xs);
    if (arr.length > 0) {
        return arr.reduceRight(folder);
    }
    else {
        throw new Error(SR_inputSequenceEmpty);
    }
}
export function replicate(n, x) {
    return initialize(n, (_arg) => x);
}
export function reverse(xs) {
    return delay(() => ofArray(reverse_1(toArray(xs))));
}
export function scan(folder, state, xs) {
    return delay(() => {
        let acc = state;
        return concat([singleton(state), map((x) => {
                acc = folder(acc, x);
                return acc;
            }, xs)]);
    });
}
export function scanBack(folder, xs, state) {
    return delay(() => ofArray(scanBack_1(folder, toArray(xs), state)));
}
export function skip(count, source) {
    return mkSeq(() => {
        const e = ofSeq(source);
        try {
            for (let _ = 1; _ <= count; _++) {
                if (!e["System.Collections.IEnumerator.MoveNext"]()) {
                    throw new Error((SR_notEnoughElements + "\\nParameter name: ") + "source");
                }
            }
            return Enumerator_enumerateThenFinally(() => {
            }, e);
        }
        catch (matchValue) {
            disposeSafe(e);
            throw matchValue;
        }
    });
}
export function skipWhile(predicate, xs) {
    return delay(() => {
        let skipped = true;
        return filter((x) => {
            if (skipped) {
                skipped = predicate(x);
            }
            return !skipped;
        }, xs);
    });
}
export function tail(xs) {
    return skip(1, xs);
}
export function take(count, xs) {
    return generateIndexed(() => ofSeq(xs), (i, e) => {
        if (i < count) {
            if (e["System.Collections.IEnumerator.MoveNext"]()) {
                return some(e["System.Collections.Generic.IEnumerator`1.get_Current"]());
            }
            else {
                throw new Error((SR_notEnoughElements + "\\nParameter name: ") + "source");
            }
        }
        else {
            return void 0;
        }
    }, (e_1) => {
        disposeSafe(e_1);
    });
}
export function takeWhile(predicate, xs) {
    return generate(() => ofSeq(xs), (e) => ((e["System.Collections.IEnumerator.MoveNext"]() && predicate(e["System.Collections.Generic.IEnumerator`1.get_Current"]())) ? some(e["System.Collections.Generic.IEnumerator`1.get_Current"]()) : void 0), (e_1) => {
        disposeSafe(e_1);
    });
}
export function truncate(count, xs) {
    return generateIndexed(() => ofSeq(xs), (i, e) => (((i < count) && e["System.Collections.IEnumerator.MoveNext"]()) ? some(e["System.Collections.Generic.IEnumerator`1.get_Current"]()) : void 0), (e_1) => {
        disposeSafe(e_1);
    });
}
export function zip(xs, ys) {
    return map2((x, y) => [x, y], xs, ys);
}
export function zip3(xs, ys, zs) {
    return map3((x, y, z) => [x, y, z], xs, ys, zs);
}
export function collect(mapping, xs) {
    return delay(() => concat(map(mapping, xs)));
}
export function where(predicate, xs) {
    return filter(predicate, xs);
}
export function pairwise(xs) {
    return delay(() => ofArray(pairwise_1(toArray(xs))));
}
export function splitInto(chunks, xs) {
    return delay(() => ofArray(splitInto_1(chunks, toArray(xs))));
}
export function windowed(windowSize, xs) {
    return delay(() => ofArray(windowed_1(windowSize, toArray(xs))));
}
export function transpose(xss) {
    return delay(() => ofArray(map_1(ofArray, transpose_1(map_1(toArray, toArray(xss))))));
}
export function sortWith(comparer, xs) {
    return delay(() => {
        const arr = toArray(xs);
        arr.sort(comparer);
        return ofArray(arr);
    });
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
    if (count === 0) {
        throw new Error((SR_inputSequenceEmpty + "\\nParameter name: ") + "source");
    }
    else {
        return averager.DivideByInt(total, count);
    }
}
export function averageBy(f, xs, averager) {
    let count = 0;
    const total = fold((acc, x) => {
        count = ((count + 1) | 0);
        return averager.Add(acc, f(x));
    }, averager.GetZero(), xs);
    if (count === 0) {
        throw new Error((SR_inputSequenceEmpty + "\\nParameter name: ") + "source");
    }
    else {
        return averager.DivideByInt(total, count);
    }
}
export function permute(f, xs) {
    return delay(() => ofArray(permute_1(f, toArray(xs))));
}
export function chunkBySize(chunkSize, xs) {
    return delay(() => ofArray(chunkBySize_1(chunkSize, toArray(xs))));
}
export function insertAt(index, y, xs) {
    let isDone = false;
    if (index < 0) {
        throw new Error((SR_indexOutOfBounds + "\\nParameter name: ") + "index");
    }
    return generateIndexed(() => ofSeq(xs), (i, e) => {
        if ((isDone ? true : (i < index)) && e["System.Collections.IEnumerator.MoveNext"]()) {
            return some(e["System.Collections.Generic.IEnumerator`1.get_Current"]());
        }
        else if (i === index) {
            isDone = true;
            return some(y);
        }
        else {
            if (!isDone) {
                throw new Error((SR_indexOutOfBounds + "\\nParameter name: ") + "index");
            }
            return void 0;
        }
    }, (e_1) => {
        disposeSafe(e_1);
    });
}
export function insertManyAt(index, ys, xs) {
    let status = -1;
    if (index < 0) {
        throw new Error((SR_indexOutOfBounds + "\\nParameter name: ") + "index");
    }
    return generateIndexed(() => [ofSeq(xs), ofSeq(ys)], (i, tupledArg) => {
        const e1 = tupledArg[0];
        const e2 = tupledArg[1];
        if (i === index) {
            status = 0;
        }
        let inserted;
        if (status === 0) {
            if (e2["System.Collections.IEnumerator.MoveNext"]()) {
                inserted = some(e2["System.Collections.Generic.IEnumerator`1.get_Current"]());
            }
            else {
                status = 1;
                inserted = void 0;
            }
        }
        else {
            inserted = void 0;
        }
        if (inserted == null) {
            if (e1["System.Collections.IEnumerator.MoveNext"]()) {
                return some(e1["System.Collections.Generic.IEnumerator`1.get_Current"]());
            }
            else {
                if (status < 1) {
                    throw new Error((SR_indexOutOfBounds + "\\nParameter name: ") + "index");
                }
                return void 0;
            }
        }
        else {
            return some(value_1(inserted));
        }
    }, (tupledArg_1) => {
        disposeSafe(tupledArg_1[0]);
        disposeSafe(tupledArg_1[1]);
    });
}
export function removeAt(index, xs) {
    let isDone = false;
    if (index < 0) {
        throw new Error((SR_indexOutOfBounds + "\\nParameter name: ") + "index");
    }
    return generateIndexed(() => ofSeq(xs), (i, e) => {
        if ((isDone ? true : (i < index)) && e["System.Collections.IEnumerator.MoveNext"]()) {
            return some(e["System.Collections.Generic.IEnumerator`1.get_Current"]());
        }
        else if ((i === index) && e["System.Collections.IEnumerator.MoveNext"]()) {
            isDone = true;
            return e["System.Collections.IEnumerator.MoveNext"]() ? some(e["System.Collections.Generic.IEnumerator`1.get_Current"]()) : void 0;
        }
        else {
            if (!isDone) {
                throw new Error((SR_indexOutOfBounds + "\\nParameter name: ") + "index");
            }
            return void 0;
        }
    }, (e_1) => {
        disposeSafe(e_1);
    });
}
export function removeManyAt(index, count, xs) {
    if (index < 0) {
        throw new Error((SR_indexOutOfBounds + "\\nParameter name: ") + "index");
    }
    return generateIndexed(() => ofSeq(xs), (i, e) => {
        if (i < index) {
            if (e["System.Collections.IEnumerator.MoveNext"]()) {
                return some(e["System.Collections.Generic.IEnumerator`1.get_Current"]());
            }
            else {
                throw new Error((SR_indexOutOfBounds + "\\nParameter name: ") + "index");
            }
        }
        else {
            if (i === index) {
                for (let _ = 1; _ <= count; _++) {
                    if (!e["System.Collections.IEnumerator.MoveNext"]()) {
                        throw new Error((SR_indexOutOfBounds + "\\nParameter name: ") + "count");
                    }
                }
            }
            return e["System.Collections.IEnumerator.MoveNext"]() ? some(e["System.Collections.Generic.IEnumerator`1.get_Current"]()) : void 0;
        }
    }, (e_1) => {
        disposeSafe(e_1);
    });
}
export function updateAt(index, y, xs) {
    let isDone = false;
    if (index < 0) {
        throw new Error((SR_indexOutOfBounds + "\\nParameter name: ") + "index");
    }
    return generateIndexed(() => ofSeq(xs), (i, e) => {
        if ((isDone ? true : (i < index)) && e["System.Collections.IEnumerator.MoveNext"]()) {
            return some(e["System.Collections.Generic.IEnumerator`1.get_Current"]());
        }
        else if ((i === index) && e["System.Collections.IEnumerator.MoveNext"]()) {
            isDone = true;
            return some(y);
        }
        else {
            if (!isDone) {
                throw new Error((SR_indexOutOfBounds + "\\nParameter name: ") + "index");
            }
            return void 0;
        }
    }, (e_1) => {
        disposeSafe(e_1);
    });
}
