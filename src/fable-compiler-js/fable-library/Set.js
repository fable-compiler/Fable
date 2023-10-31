import { record_type, bool_type, list_type, option_type, class_type } from "./Reflection.js";
import { some, value as value_3 } from "./Option.js";
import { structuralHash, toIterator, disposeSafe, getEnumerator, isArrayLike } from "./Util.js";
import { toString, Record } from "./Types.js";
import { fold as fold_2, cons, singleton as singleton_1, empty as empty_1, ofArrayWithTail, tail, head, isEmpty as isEmpty_1, FSharpList } from "./List.js";
import { fold as fold_1, fill } from "./Array.js";
import { join } from "./String.js";
import { exists as exists_1, cache, forAll as forAll_1, fold as fold_3, reduce, iterate as iterate_1, map as map_1 } from "./Seq.js";
import { HashSet__get_Comparer, HashSet_$ctor_Z6150332D, HashSet } from "./MutableSet.js";
export class SetTreeLeaf$1 {
    constructor(k) {
        this.k = k;
    }
}
export function SetTreeLeaf$1_$reflection(gen0) {
    return class_type("Set.SetTreeLeaf`1", [gen0], SetTreeLeaf$1);
}
export function SetTreeLeaf$1_$ctor_2B595(k) {
    return new SetTreeLeaf$1(k);
}
export function SetTreeLeaf$1__get_Key(_) {
    return _.k;
}
export class SetTreeNode$1 extends SetTreeLeaf$1 {
    constructor(v, left, right, h) {
        super(v);
        this.left = left;
        this.right = right;
        this.h = (h | 0);
    }
}
export function SetTreeNode$1_$reflection(gen0) {
    return class_type("Set.SetTreeNode`1", [gen0], SetTreeNode$1, SetTreeLeaf$1_$reflection(gen0));
}
export function SetTreeNode$1_$ctor_5F465FC9(v, left, right, h) {
    return new SetTreeNode$1(v, left, right, h);
}
export function SetTreeNode$1__get_Left(_) {
    return _.left;
}
export function SetTreeNode$1__get_Right(_) {
    return _.right;
}
export function SetTreeNode$1__get_Height(_) {
    return _.h;
}
export function SetTreeModule_empty() {
    return void 0;
}
export function SetTreeModule_countAux(t_mut, acc_mut) {
    SetTreeModule_countAux: while (true) {
        const t = t_mut, acc = acc_mut;
        if (t != null) {
            const t2 = value_3(t);
            if (t2 instanceof SetTreeNode$1) {
                const tn = t2;
                t_mut = SetTreeNode$1__get_Left(tn);
                acc_mut = SetTreeModule_countAux(SetTreeNode$1__get_Right(tn), acc + 1);
                continue SetTreeModule_countAux;
            }
            else {
                return (acc + 1) | 0;
            }
        }
        else {
            return acc | 0;
        }
        break;
    }
}
export function SetTreeModule_count(s) {
    return SetTreeModule_countAux(s, 0);
}
export function SetTreeModule_mk(l, k, r) {
    let tn, tn_1;
    let hl;
    const t = l;
    if (t != null) {
        const t2 = value_3(t);
        hl = ((t2 instanceof SetTreeNode$1) ? ((tn = t2, SetTreeNode$1__get_Height(tn))) : 1);
    }
    else {
        hl = 0;
    }
    let hr;
    const t_1 = r;
    if (t_1 != null) {
        const t2_1 = value_3(t_1);
        hr = ((t2_1 instanceof SetTreeNode$1) ? ((tn_1 = t2_1, SetTreeNode$1__get_Height(tn_1))) : 1);
    }
    else {
        hr = 0;
    }
    const m = ((hl < hr) ? hr : hl) | 0;
    if (m === 0) {
        return SetTreeLeaf$1_$ctor_2B595(k);
    }
    else {
        return SetTreeNode$1_$ctor_5F465FC9(k, l, r, m + 1);
    }
}
export function SetTreeModule_rebalance(t1, v, t2) {
    let tn, tn_1, t_2, t2_3, tn_2, t_3, t2_4, tn_3;
    let t1h;
    const t = t1;
    if (t != null) {
        const t2_1 = value_3(t);
        t1h = ((t2_1 instanceof SetTreeNode$1) ? ((tn = t2_1, SetTreeNode$1__get_Height(tn))) : 1);
    }
    else {
        t1h = 0;
    }
    let t2h;
    const t_1 = t2;
    if (t_1 != null) {
        const t2_2 = value_3(t_1);
        t2h = ((t2_2 instanceof SetTreeNode$1) ? ((tn_1 = t2_2, SetTreeNode$1__get_Height(tn_1))) : 1);
    }
    else {
        t2h = 0;
    }
    if (t2h > (t1h + 2)) {
        const matchValue = value_3(t2);
        if (matchValue instanceof SetTreeNode$1) {
            const t2$0027 = matchValue;
            if (((t_2 = SetTreeNode$1__get_Left(t2$0027), (t_2 != null) ? ((t2_3 = value_3(t_2), (t2_3 instanceof SetTreeNode$1) ? ((tn_2 = t2_3, SetTreeNode$1__get_Height(tn_2))) : 1)) : 0)) > (t1h + 1)) {
                const matchValue_1 = value_3(SetTreeNode$1__get_Left(t2$0027));
                if (matchValue_1 instanceof SetTreeNode$1) {
                    const t2l = matchValue_1;
                    return SetTreeModule_mk(SetTreeModule_mk(t1, v, SetTreeNode$1__get_Left(t2l)), SetTreeLeaf$1__get_Key(t2l), SetTreeModule_mk(SetTreeNode$1__get_Right(t2l), SetTreeLeaf$1__get_Key(t2$0027), SetTreeNode$1__get_Right(t2$0027)));
                }
                else {
                    throw new Error("internal error: Set.rebalance");
                }
            }
            else {
                return SetTreeModule_mk(SetTreeModule_mk(t1, v, SetTreeNode$1__get_Left(t2$0027)), SetTreeLeaf$1__get_Key(t2$0027), SetTreeNode$1__get_Right(t2$0027));
            }
        }
        else {
            throw new Error("internal error: Set.rebalance");
        }
    }
    else if (t1h > (t2h + 2)) {
        const matchValue_2 = value_3(t1);
        if (matchValue_2 instanceof SetTreeNode$1) {
            const t1$0027 = matchValue_2;
            if (((t_3 = SetTreeNode$1__get_Right(t1$0027), (t_3 != null) ? ((t2_4 = value_3(t_3), (t2_4 instanceof SetTreeNode$1) ? ((tn_3 = t2_4, SetTreeNode$1__get_Height(tn_3))) : 1)) : 0)) > (t2h + 1)) {
                const matchValue_3 = value_3(SetTreeNode$1__get_Right(t1$0027));
                if (matchValue_3 instanceof SetTreeNode$1) {
                    const t1r = matchValue_3;
                    return SetTreeModule_mk(SetTreeModule_mk(SetTreeNode$1__get_Left(t1$0027), SetTreeLeaf$1__get_Key(t1$0027), SetTreeNode$1__get_Left(t1r)), SetTreeLeaf$1__get_Key(t1r), SetTreeModule_mk(SetTreeNode$1__get_Right(t1r), v, t2));
                }
                else {
                    throw new Error("internal error: Set.rebalance");
                }
            }
            else {
                return SetTreeModule_mk(SetTreeNode$1__get_Left(t1$0027), SetTreeLeaf$1__get_Key(t1$0027), SetTreeModule_mk(SetTreeNode$1__get_Right(t1$0027), v, t2));
            }
        }
        else {
            throw new Error("internal error: Set.rebalance");
        }
    }
    else {
        return SetTreeModule_mk(t1, v, t2);
    }
}
export function SetTreeModule_add(comparer, k, t) {
    if (t != null) {
        const t2 = value_3(t);
        const c = comparer.Compare(k, SetTreeLeaf$1__get_Key(t2)) | 0;
        if (t2 instanceof SetTreeNode$1) {
            const tn = t2;
            if (c < 0) {
                return SetTreeModule_rebalance(SetTreeModule_add(comparer, k, SetTreeNode$1__get_Left(tn)), SetTreeLeaf$1__get_Key(tn), SetTreeNode$1__get_Right(tn));
            }
            else if (c === 0) {
                return t;
            }
            else {
                return SetTreeModule_rebalance(SetTreeNode$1__get_Left(tn), SetTreeLeaf$1__get_Key(tn), SetTreeModule_add(comparer, k, SetTreeNode$1__get_Right(tn)));
            }
        }
        else {
            const c_1 = comparer.Compare(k, SetTreeLeaf$1__get_Key(t2)) | 0;
            if (c_1 < 0) {
                return SetTreeNode$1_$ctor_5F465FC9(k, SetTreeModule_empty(), t, 2);
            }
            else if (c_1 === 0) {
                return t;
            }
            else {
                return SetTreeNode$1_$ctor_5F465FC9(k, t, SetTreeModule_empty(), 2);
            }
        }
    }
    else {
        return SetTreeLeaf$1_$ctor_2B595(k);
    }
}
export function SetTreeModule_balance(comparer, t1, k, t2) {
    if (t1 != null) {
        const t1$0027 = value_3(t1);
        if (t2 != null) {
            const t2$0027 = value_3(t2);
            if (t1$0027 instanceof SetTreeNode$1) {
                const t1n = t1$0027;
                if (t2$0027 instanceof SetTreeNode$1) {
                    const t2n = t2$0027;
                    if ((SetTreeNode$1__get_Height(t1n) + 2) < SetTreeNode$1__get_Height(t2n)) {
                        return SetTreeModule_rebalance(SetTreeModule_balance(comparer, t1, k, SetTreeNode$1__get_Left(t2n)), SetTreeLeaf$1__get_Key(t2n), SetTreeNode$1__get_Right(t2n));
                    }
                    else if ((SetTreeNode$1__get_Height(t2n) + 2) < SetTreeNode$1__get_Height(t1n)) {
                        return SetTreeModule_rebalance(SetTreeNode$1__get_Left(t1n), SetTreeLeaf$1__get_Key(t1n), SetTreeModule_balance(comparer, SetTreeNode$1__get_Right(t1n), k, t2));
                    }
                    else {
                        return SetTreeModule_mk(t1, k, t2);
                    }
                }
                else {
                    return SetTreeModule_add(comparer, k, SetTreeModule_add(comparer, SetTreeLeaf$1__get_Key(t2$0027), t1));
                }
            }
            else {
                return SetTreeModule_add(comparer, k, SetTreeModule_add(comparer, SetTreeLeaf$1__get_Key(t1$0027), t2));
            }
        }
        else {
            return SetTreeModule_add(comparer, k, t1);
        }
    }
    else {
        return SetTreeModule_add(comparer, k, t2);
    }
}
export function SetTreeModule_split(comparer, pivot, t) {
    if (t != null) {
        const t2 = value_3(t);
        if (t2 instanceof SetTreeNode$1) {
            const tn = t2;
            const c = comparer.Compare(pivot, SetTreeLeaf$1__get_Key(tn)) | 0;
            if (c < 0) {
                const patternInput = SetTreeModule_split(comparer, pivot, SetTreeNode$1__get_Left(tn));
                return [patternInput[0], patternInput[1], SetTreeModule_balance(comparer, patternInput[2], SetTreeLeaf$1__get_Key(tn), SetTreeNode$1__get_Right(tn))];
            }
            else if (c === 0) {
                return [SetTreeNode$1__get_Left(tn), true, SetTreeNode$1__get_Right(tn)];
            }
            else {
                const patternInput_1 = SetTreeModule_split(comparer, pivot, SetTreeNode$1__get_Right(tn));
                return [SetTreeModule_balance(comparer, SetTreeNode$1__get_Left(tn), SetTreeLeaf$1__get_Key(tn), patternInput_1[0]), patternInput_1[1], patternInput_1[2]];
            }
        }
        else {
            const c_1 = comparer.Compare(SetTreeLeaf$1__get_Key(t2), pivot) | 0;
            if (c_1 < 0) {
                return [t, false, SetTreeModule_empty()];
            }
            else if (c_1 === 0) {
                return [SetTreeModule_empty(), true, SetTreeModule_empty()];
            }
            else {
                return [SetTreeModule_empty(), false, t];
            }
        }
    }
    else {
        return [SetTreeModule_empty(), false, SetTreeModule_empty()];
    }
}
export function SetTreeModule_spliceOutSuccessor(t) {
    if (t != null) {
        const t2 = value_3(t);
        if (t2 instanceof SetTreeNode$1) {
            const tn = t2;
            if (SetTreeNode$1__get_Left(tn) == null) {
                return [SetTreeLeaf$1__get_Key(tn), SetTreeNode$1__get_Right(tn)];
            }
            else {
                const patternInput = SetTreeModule_spliceOutSuccessor(SetTreeNode$1__get_Left(tn));
                return [patternInput[0], SetTreeModule_mk(patternInput[1], SetTreeLeaf$1__get_Key(tn), SetTreeNode$1__get_Right(tn))];
            }
        }
        else {
            return [SetTreeLeaf$1__get_Key(t2), SetTreeModule_empty()];
        }
    }
    else {
        throw new Error("internal error: Set.spliceOutSuccessor");
    }
}
export function SetTreeModule_remove(comparer, k, t) {
    if (t != null) {
        const t2 = value_3(t);
        const c = comparer.Compare(k, SetTreeLeaf$1__get_Key(t2)) | 0;
        if (t2 instanceof SetTreeNode$1) {
            const tn = t2;
            if (c < 0) {
                return SetTreeModule_rebalance(SetTreeModule_remove(comparer, k, SetTreeNode$1__get_Left(tn)), SetTreeLeaf$1__get_Key(tn), SetTreeNode$1__get_Right(tn));
            }
            else if (c === 0) {
                if (SetTreeNode$1__get_Left(tn) == null) {
                    return SetTreeNode$1__get_Right(tn);
                }
                else if (SetTreeNode$1__get_Right(tn) == null) {
                    return SetTreeNode$1__get_Left(tn);
                }
                else {
                    const patternInput = SetTreeModule_spliceOutSuccessor(SetTreeNode$1__get_Right(tn));
                    return SetTreeModule_mk(SetTreeNode$1__get_Left(tn), patternInput[0], patternInput[1]);
                }
            }
            else {
                return SetTreeModule_rebalance(SetTreeNode$1__get_Left(tn), SetTreeLeaf$1__get_Key(tn), SetTreeModule_remove(comparer, k, SetTreeNode$1__get_Right(tn)));
            }
        }
        else if (c === 0) {
            return SetTreeModule_empty();
        }
        else {
            return t;
        }
    }
    else {
        return t;
    }
}
export function SetTreeModule_mem(comparer_mut, k_mut, t_mut) {
    SetTreeModule_mem: while (true) {
        const comparer = comparer_mut, k = k_mut, t = t_mut;
        if (t != null) {
            const t2 = value_3(t);
            const c = comparer.Compare(k, SetTreeLeaf$1__get_Key(t2)) | 0;
            if (t2 instanceof SetTreeNode$1) {
                const tn = t2;
                if (c < 0) {
                    comparer_mut = comparer;
                    k_mut = k;
                    t_mut = SetTreeNode$1__get_Left(tn);
                    continue SetTreeModule_mem;
                }
                else if (c === 0) {
                    return true;
                }
                else {
                    comparer_mut = comparer;
                    k_mut = k;
                    t_mut = SetTreeNode$1__get_Right(tn);
                    continue SetTreeModule_mem;
                }
            }
            else {
                return c === 0;
            }
        }
        else {
            return false;
        }
        break;
    }
}
export function SetTreeModule_iter(f_mut, t_mut) {
    SetTreeModule_iter: while (true) {
        const f = f_mut, t = t_mut;
        if (t != null) {
            const t2 = value_3(t);
            if (t2 instanceof SetTreeNode$1) {
                const tn = t2;
                SetTreeModule_iter(f, SetTreeNode$1__get_Left(tn));
                f(SetTreeLeaf$1__get_Key(tn));
                f_mut = f;
                t_mut = SetTreeNode$1__get_Right(tn);
                continue SetTreeModule_iter;
            }
            else {
                f(SetTreeLeaf$1__get_Key(t2));
            }
        }
        break;
    }
}
export function SetTreeModule_foldBackOpt(f_mut, t_mut, x_mut) {
    SetTreeModule_foldBackOpt: while (true) {
        const f = f_mut, t = t_mut, x = x_mut;
        if (t != null) {
            const t2 = value_3(t);
            if (t2 instanceof SetTreeNode$1) {
                const tn = t2;
                f_mut = f;
                t_mut = SetTreeNode$1__get_Left(tn);
                x_mut = f(SetTreeLeaf$1__get_Key(tn), SetTreeModule_foldBackOpt(f, SetTreeNode$1__get_Right(tn), x));
                continue SetTreeModule_foldBackOpt;
            }
            else {
                return f(SetTreeLeaf$1__get_Key(t2), x);
            }
        }
        else {
            return x;
        }
        break;
    }
}
export function SetTreeModule_foldBack(f, m, x) {
    return SetTreeModule_foldBackOpt(f, m, x);
}
export function SetTreeModule_foldOpt(f_mut, x_mut, t_mut) {
    SetTreeModule_foldOpt: while (true) {
        const f = f_mut, x = x_mut, t = t_mut;
        if (t != null) {
            const t2 = value_3(t);
            if (t2 instanceof SetTreeNode$1) {
                const tn = t2;
                f_mut = f;
                x_mut = f(SetTreeModule_foldOpt(f, x, SetTreeNode$1__get_Left(tn)), SetTreeLeaf$1__get_Key(tn));
                t_mut = SetTreeNode$1__get_Right(tn);
                continue SetTreeModule_foldOpt;
            }
            else {
                return f(x, SetTreeLeaf$1__get_Key(t2));
            }
        }
        else {
            return x;
        }
        break;
    }
}
export function SetTreeModule_fold(f, x, m) {
    return SetTreeModule_foldOpt(f, x, m);
}
export function SetTreeModule_forall(f_mut, t_mut) {
    SetTreeModule_forall: while (true) {
        const f = f_mut, t = t_mut;
        if (t != null) {
            const t2 = value_3(t);
            if (t2 instanceof SetTreeNode$1) {
                const tn = t2;
                if (f(SetTreeLeaf$1__get_Key(tn)) && SetTreeModule_forall(f, SetTreeNode$1__get_Left(tn))) {
                    f_mut = f;
                    t_mut = SetTreeNode$1__get_Right(tn);
                    continue SetTreeModule_forall;
                }
                else {
                    return false;
                }
            }
            else {
                return f(SetTreeLeaf$1__get_Key(t2));
            }
        }
        else {
            return true;
        }
        break;
    }
}
export function SetTreeModule_exists(f_mut, t_mut) {
    SetTreeModule_exists: while (true) {
        const f = f_mut, t = t_mut;
        if (t != null) {
            const t2 = value_3(t);
            if (t2 instanceof SetTreeNode$1) {
                const tn = t2;
                if (f(SetTreeLeaf$1__get_Key(tn)) ? true : SetTreeModule_exists(f, SetTreeNode$1__get_Left(tn))) {
                    return true;
                }
                else {
                    f_mut = f;
                    t_mut = SetTreeNode$1__get_Right(tn);
                    continue SetTreeModule_exists;
                }
            }
            else {
                return f(SetTreeLeaf$1__get_Key(t2));
            }
        }
        else {
            return false;
        }
        break;
    }
}
export function SetTreeModule_subset(comparer, a, b) {
    return SetTreeModule_forall((x) => SetTreeModule_mem(comparer, x, b), a);
}
export function SetTreeModule_properSubset(comparer, a, b) {
    if (SetTreeModule_forall((x) => SetTreeModule_mem(comparer, x, b), a)) {
        return SetTreeModule_exists((x_1) => !SetTreeModule_mem(comparer, x_1, a), b);
    }
    else {
        return false;
    }
}
export function SetTreeModule_filterAux(comparer_mut, f_mut, t_mut, acc_mut) {
    SetTreeModule_filterAux: while (true) {
        const comparer = comparer_mut, f = f_mut, t = t_mut, acc = acc_mut;
        if (t != null) {
            const t2 = value_3(t);
            if (t2 instanceof SetTreeNode$1) {
                const tn = t2;
                const acc_1 = f(SetTreeLeaf$1__get_Key(tn)) ? SetTreeModule_add(comparer, SetTreeLeaf$1__get_Key(tn), acc) : acc;
                comparer_mut = comparer;
                f_mut = f;
                t_mut = SetTreeNode$1__get_Left(tn);
                acc_mut = SetTreeModule_filterAux(comparer, f, SetTreeNode$1__get_Right(tn), acc_1);
                continue SetTreeModule_filterAux;
            }
            else if (f(SetTreeLeaf$1__get_Key(t2))) {
                return SetTreeModule_add(comparer, SetTreeLeaf$1__get_Key(t2), acc);
            }
            else {
                return acc;
            }
        }
        else {
            return acc;
        }
        break;
    }
}
export function SetTreeModule_filter(comparer, f, s) {
    return SetTreeModule_filterAux(comparer, f, s, SetTreeModule_empty());
}
export function SetTreeModule_diffAux(comparer_mut, t_mut, acc_mut) {
    SetTreeModule_diffAux: while (true) {
        const comparer = comparer_mut, t = t_mut, acc = acc_mut;
        if (acc == null) {
            return acc;
        }
        else if (t != null) {
            const t2 = value_3(t);
            if (t2 instanceof SetTreeNode$1) {
                const tn = t2;
                comparer_mut = comparer;
                t_mut = SetTreeNode$1__get_Left(tn);
                acc_mut = SetTreeModule_diffAux(comparer, SetTreeNode$1__get_Right(tn), SetTreeModule_remove(comparer, SetTreeLeaf$1__get_Key(tn), acc));
                continue SetTreeModule_diffAux;
            }
            else {
                return SetTreeModule_remove(comparer, SetTreeLeaf$1__get_Key(t2), acc);
            }
        }
        else {
            return acc;
        }
        break;
    }
}
export function SetTreeModule_diff(comparer, a, b) {
    return SetTreeModule_diffAux(comparer, b, a);
}
export function SetTreeModule_union(comparer, t1, t2) {
    if (t1 != null) {
        const t1$0027 = value_3(t1);
        if (t2 != null) {
            const t2$0027 = value_3(t2);
            if (t1$0027 instanceof SetTreeNode$1) {
                const t1n = t1$0027;
                if (t2$0027 instanceof SetTreeNode$1) {
                    const t2n = t2$0027;
                    if (SetTreeNode$1__get_Height(t1n) > SetTreeNode$1__get_Height(t2n)) {
                        const patternInput = SetTreeModule_split(comparer, SetTreeLeaf$1__get_Key(t1n), t2);
                        return SetTreeModule_balance(comparer, SetTreeModule_union(comparer, SetTreeNode$1__get_Left(t1n), patternInput[0]), SetTreeLeaf$1__get_Key(t1n), SetTreeModule_union(comparer, SetTreeNode$1__get_Right(t1n), patternInput[2]));
                    }
                    else {
                        const patternInput_1 = SetTreeModule_split(comparer, SetTreeLeaf$1__get_Key(t2n), t1);
                        return SetTreeModule_balance(comparer, SetTreeModule_union(comparer, SetTreeNode$1__get_Left(t2n), patternInput_1[0]), SetTreeLeaf$1__get_Key(t2n), SetTreeModule_union(comparer, SetTreeNode$1__get_Right(t2n), patternInput_1[2]));
                    }
                }
                else {
                    return SetTreeModule_add(comparer, SetTreeLeaf$1__get_Key(t2$0027), t1);
                }
            }
            else {
                return SetTreeModule_add(comparer, SetTreeLeaf$1__get_Key(t1$0027), t2);
            }
        }
        else {
            return t1;
        }
    }
    else {
        return t2;
    }
}
export function SetTreeModule_intersectionAux(comparer_mut, b_mut, t_mut, acc_mut) {
    SetTreeModule_intersectionAux: while (true) {
        const comparer = comparer_mut, b = b_mut, t = t_mut, acc = acc_mut;
        if (t != null) {
            const t2 = value_3(t);
            if (t2 instanceof SetTreeNode$1) {
                const tn = t2;
                const acc_1 = SetTreeModule_intersectionAux(comparer, b, SetTreeNode$1__get_Right(tn), acc);
                const acc_2 = SetTreeModule_mem(comparer, SetTreeLeaf$1__get_Key(tn), b) ? SetTreeModule_add(comparer, SetTreeLeaf$1__get_Key(tn), acc_1) : acc_1;
                comparer_mut = comparer;
                b_mut = b;
                t_mut = SetTreeNode$1__get_Left(tn);
                acc_mut = acc_2;
                continue SetTreeModule_intersectionAux;
            }
            else if (SetTreeModule_mem(comparer, SetTreeLeaf$1__get_Key(t2), b)) {
                return SetTreeModule_add(comparer, SetTreeLeaf$1__get_Key(t2), acc);
            }
            else {
                return acc;
            }
        }
        else {
            return acc;
        }
        break;
    }
}
export function SetTreeModule_intersection(comparer, a, b) {
    return SetTreeModule_intersectionAux(comparer, b, a, SetTreeModule_empty());
}
export function SetTreeModule_partition1(comparer, f, k, acc1, acc2) {
    if (f(k)) {
        return [SetTreeModule_add(comparer, k, acc1), acc2];
    }
    else {
        return [acc1, SetTreeModule_add(comparer, k, acc2)];
    }
}
export function SetTreeModule_partitionAux(comparer_mut, f_mut, t_mut, acc__mut, acc__1_mut) {
    SetTreeModule_partitionAux: while (true) {
        const comparer = comparer_mut, f = f_mut, t = t_mut, acc_ = acc__mut, acc__1 = acc__1_mut;
        const acc = [acc_, acc__1];
        if (t != null) {
            const t2 = value_3(t);
            if (t2 instanceof SetTreeNode$1) {
                const tn = t2;
                const acc_1 = SetTreeModule_partitionAux(comparer, f, SetTreeNode$1__get_Right(tn), acc[0], acc[1]);
                const acc_4 = SetTreeModule_partition1(comparer, f, SetTreeLeaf$1__get_Key(tn), acc_1[0], acc_1[1]);
                comparer_mut = comparer;
                f_mut = f;
                t_mut = SetTreeNode$1__get_Left(tn);
                acc__mut = acc_4[0];
                acc__1_mut = acc_4[1];
                continue SetTreeModule_partitionAux;
            }
            else {
                return SetTreeModule_partition1(comparer, f, SetTreeLeaf$1__get_Key(t2), acc[0], acc[1]);
            }
        }
        else {
            return acc;
        }
        break;
    }
}
export function SetTreeModule_partition(comparer, f, s) {
    return SetTreeModule_partitionAux(comparer, f, s, SetTreeModule_empty(), SetTreeModule_empty());
}
export function SetTreeModule_minimumElementAux(t_mut, n_mut) {
    SetTreeModule_minimumElementAux: while (true) {
        const t = t_mut, n = n_mut;
        if (t != null) {
            const t2 = value_3(t);
            if (t2 instanceof SetTreeNode$1) {
                const tn = t2;
                t_mut = SetTreeNode$1__get_Left(tn);
                n_mut = SetTreeLeaf$1__get_Key(tn);
                continue SetTreeModule_minimumElementAux;
            }
            else {
                return SetTreeLeaf$1__get_Key(t2);
            }
        }
        else {
            return n;
        }
        break;
    }
}
export function SetTreeModule_minimumElementOpt(t) {
    if (t != null) {
        const t2 = value_3(t);
        if (t2 instanceof SetTreeNode$1) {
            const tn = t2;
            return some(SetTreeModule_minimumElementAux(SetTreeNode$1__get_Left(tn), SetTreeLeaf$1__get_Key(tn)));
        }
        else {
            return some(SetTreeLeaf$1__get_Key(t2));
        }
    }
    else {
        return void 0;
    }
}
export function SetTreeModule_maximumElementAux(t_mut, n_mut) {
    SetTreeModule_maximumElementAux: while (true) {
        const t = t_mut, n = n_mut;
        if (t != null) {
            const t2 = value_3(t);
            if (t2 instanceof SetTreeNode$1) {
                const tn = t2;
                t_mut = SetTreeNode$1__get_Right(tn);
                n_mut = SetTreeLeaf$1__get_Key(tn);
                continue SetTreeModule_maximumElementAux;
            }
            else {
                return SetTreeLeaf$1__get_Key(t2);
            }
        }
        else {
            return n;
        }
        break;
    }
}
export function SetTreeModule_maximumElementOpt(t) {
    if (t != null) {
        const t2 = value_3(t);
        if (t2 instanceof SetTreeNode$1) {
            const tn = t2;
            return some(SetTreeModule_maximumElementAux(SetTreeNode$1__get_Right(tn), SetTreeLeaf$1__get_Key(tn)));
        }
        else {
            return some(SetTreeLeaf$1__get_Key(t2));
        }
    }
    else {
        return void 0;
    }
}
export function SetTreeModule_minimumElement(s) {
    const matchValue = SetTreeModule_minimumElementOpt(s);
    if (matchValue == null) {
        throw new Error("Set contains no elements");
    }
    else {
        return value_3(matchValue);
    }
}
export function SetTreeModule_maximumElement(s) {
    const matchValue = SetTreeModule_maximumElementOpt(s);
    if (matchValue == null) {
        throw new Error("Set contains no elements");
    }
    else {
        return value_3(matchValue);
    }
}
export class SetTreeModule_SetIterator$1 extends Record {
    constructor(stack, started) {
        super();
        this.stack = stack;
        this.started = started;
    }
}
export function SetTreeModule_SetIterator$1_$reflection(gen0) {
    return record_type("Set.SetTreeModule.SetIterator`1", [gen0], SetTreeModule_SetIterator$1, () => [["stack", list_type(option_type(SetTreeLeaf$1_$reflection(gen0)))], ["started", bool_type]]);
}
export function SetTreeModule_collapseLHS(stack_mut) {
    SetTreeModule_collapseLHS: while (true) {
        const stack = stack_mut;
        if (!isEmpty_1(stack)) {
            const x = head(stack);
            const rest = tail(stack);
            if (x != null) {
                const x2 = value_3(x);
                if (x2 instanceof SetTreeNode$1) {
                    const xn = x2;
                    stack_mut = ofArrayWithTail([SetTreeNode$1__get_Left(xn), SetTreeLeaf$1_$ctor_2B595(SetTreeLeaf$1__get_Key(xn)), SetTreeNode$1__get_Right(xn)], rest);
                    continue SetTreeModule_collapseLHS;
                }
                else {
                    return stack;
                }
            }
            else {
                stack_mut = rest;
                continue SetTreeModule_collapseLHS;
            }
        }
        else {
            return empty_1();
        }
        break;
    }
}
export function SetTreeModule_mkIterator(s) {
    return new SetTreeModule_SetIterator$1(SetTreeModule_collapseLHS(singleton_1(s)), false);
}
export function SetTreeModule_notStarted() {
    throw new Error("Enumeration not started");
}
export function SetTreeModule_alreadyFinished() {
    throw new Error("Enumeration already started");
}
export function SetTreeModule_current(i) {
    if (i.started) {
        const matchValue = i.stack;
        if (isEmpty_1(matchValue)) {
            return SetTreeModule_alreadyFinished();
        }
        else if (head(matchValue) != null) {
            const t = value_3(head(matchValue));
            return SetTreeLeaf$1__get_Key(t);
        }
        else {
            throw new Error("Please report error: Set iterator, unexpected stack for current");
        }
    }
    else {
        return SetTreeModule_notStarted();
    }
}
export function SetTreeModule_moveNext(i) {
    if (i.started) {
        const matchValue = i.stack;
        if (!isEmpty_1(matchValue)) {
            if (head(matchValue) != null) {
                const t = value_3(head(matchValue));
                if (t instanceof SetTreeNode$1) {
                    throw new Error("Please report error: Set iterator, unexpected stack for moveNext");
                }
                else {
                    i.stack = SetTreeModule_collapseLHS(tail(matchValue));
                    return !isEmpty_1(i.stack);
                }
            }
            else {
                throw new Error("Please report error: Set iterator, unexpected stack for moveNext");
            }
        }
        else {
            return false;
        }
    }
    else {
        i.started = true;
        return !isEmpty_1(i.stack);
    }
}
export function SetTreeModule_mkIEnumerator(s) {
    let i = SetTreeModule_mkIterator(s);
    return {
        "System.Collections.Generic.IEnumerator`1.get_Current"() {
            return SetTreeModule_current(i);
        },
        "System.Collections.IEnumerator.get_Current"() {
            return SetTreeModule_current(i);
        },
        "System.Collections.IEnumerator.MoveNext"() {
            return SetTreeModule_moveNext(i);
        },
        "System.Collections.IEnumerator.Reset"() {
            i = SetTreeModule_mkIterator(s);
        },
        Dispose() {
        },
    };
}
/**
 * Set comparison.  Note this can be expensive.
 */
export function SetTreeModule_compareStacks(comparer_mut, l1_mut, l2_mut) {
    SetTreeModule_compareStacks: while (true) {
        const comparer = comparer_mut, l1 = l1_mut, l2 = l2_mut;
        if (!isEmpty_1(l1)) {
            if (!isEmpty_1(l2)) {
                if (head(l2) != null) {
                    if (head(l1) != null) {
                        const x1_3 = value_3(head(l1));
                        const x2_3 = value_3(head(l2));
                        if (x1_3 instanceof SetTreeNode$1) {
                            const x1n_2 = x1_3;
                            if (SetTreeNode$1__get_Left(x1n_2) == null) {
                                if (x2_3 instanceof SetTreeNode$1) {
                                    const x2n_2 = x2_3;
                                    if (SetTreeNode$1__get_Left(x2n_2) == null) {
                                        const c = comparer.Compare(SetTreeLeaf$1__get_Key(x1n_2), SetTreeLeaf$1__get_Key(x2n_2)) | 0;
                                        if (c !== 0) {
                                            return c | 0;
                                        }
                                        else {
                                            comparer_mut = comparer;
                                            l1_mut = cons(SetTreeNode$1__get_Right(x1n_2), tail(l1));
                                            l2_mut = cons(SetTreeNode$1__get_Right(x2n_2), tail(l2));
                                            continue SetTreeModule_compareStacks;
                                        }
                                    }
                                    else {
                                        let matchResult, t1_6, x1_4, t2_6, x2_4;
                                        if (!isEmpty_1(l1)) {
                                            if (head(l1) != null) {
                                                matchResult = 0;
                                                t1_6 = tail(l1);
                                                x1_4 = value_3(head(l1));
                                            }
                                            else if (!isEmpty_1(l2)) {
                                                if (head(l2) != null) {
                                                    matchResult = 1;
                                                    t2_6 = tail(l2);
                                                    x2_4 = value_3(head(l2));
                                                }
                                                else {
                                                    matchResult = 2;
                                                }
                                            }
                                            else {
                                                matchResult = 2;
                                            }
                                        }
                                        else if (!isEmpty_1(l2)) {
                                            if (head(l2) != null) {
                                                matchResult = 1;
                                                t2_6 = tail(l2);
                                                x2_4 = value_3(head(l2));
                                            }
                                            else {
                                                matchResult = 2;
                                            }
                                        }
                                        else {
                                            matchResult = 2;
                                        }
                                        switch (matchResult) {
                                            case 0:
                                                if (x1_4 instanceof SetTreeNode$1) {
                                                    const x1n_3 = x1_4;
                                                    comparer_mut = comparer;
                                                    l1_mut = ofArrayWithTail([SetTreeNode$1__get_Left(x1n_3), SetTreeNode$1_$ctor_5F465FC9(SetTreeLeaf$1__get_Key(x1n_3), SetTreeModule_empty(), SetTreeNode$1__get_Right(x1n_3), 0)], t1_6);
                                                    l2_mut = l2;
                                                    continue SetTreeModule_compareStacks;
                                                }
                                                else {
                                                    comparer_mut = comparer;
                                                    l1_mut = ofArrayWithTail([SetTreeModule_empty(), SetTreeLeaf$1_$ctor_2B595(SetTreeLeaf$1__get_Key(x1_4))], t1_6);
                                                    l2_mut = l2;
                                                    continue SetTreeModule_compareStacks;
                                                }
                                            case 1:
                                                if (x2_4 instanceof SetTreeNode$1) {
                                                    const x2n_3 = x2_4;
                                                    comparer_mut = comparer;
                                                    l1_mut = l1;
                                                    l2_mut = ofArrayWithTail([SetTreeNode$1__get_Left(x2n_3), SetTreeNode$1_$ctor_5F465FC9(SetTreeLeaf$1__get_Key(x2n_3), SetTreeModule_empty(), SetTreeNode$1__get_Right(x2n_3), 0)], t2_6);
                                                    continue SetTreeModule_compareStacks;
                                                }
                                                else {
                                                    comparer_mut = comparer;
                                                    l1_mut = l1;
                                                    l2_mut = ofArrayWithTail([SetTreeModule_empty(), SetTreeLeaf$1_$ctor_2B595(SetTreeLeaf$1__get_Key(x2_4))], t2_6);
                                                    continue SetTreeModule_compareStacks;
                                                }
                                            default:
                                                throw new Error("unexpected state in SetTree.compareStacks");
                                        }
                                    }
                                }
                                else {
                                    const c_1 = comparer.Compare(SetTreeLeaf$1__get_Key(x1n_2), SetTreeLeaf$1__get_Key(x2_3)) | 0;
                                    if (c_1 !== 0) {
                                        return c_1 | 0;
                                    }
                                    else {
                                        comparer_mut = comparer;
                                        l1_mut = cons(SetTreeNode$1__get_Right(x1n_2), tail(l1));
                                        l2_mut = cons(SetTreeModule_empty(), tail(l2));
                                        continue SetTreeModule_compareStacks;
                                    }
                                }
                            }
                            else {
                                let matchResult_1, t1_7, x1_5, t2_7, x2_5;
                                if (!isEmpty_1(l1)) {
                                    if (head(l1) != null) {
                                        matchResult_1 = 0;
                                        t1_7 = tail(l1);
                                        x1_5 = value_3(head(l1));
                                    }
                                    else if (!isEmpty_1(l2)) {
                                        if (head(l2) != null) {
                                            matchResult_1 = 1;
                                            t2_7 = tail(l2);
                                            x2_5 = value_3(head(l2));
                                        }
                                        else {
                                            matchResult_1 = 2;
                                        }
                                    }
                                    else {
                                        matchResult_1 = 2;
                                    }
                                }
                                else if (!isEmpty_1(l2)) {
                                    if (head(l2) != null) {
                                        matchResult_1 = 1;
                                        t2_7 = tail(l2);
                                        x2_5 = value_3(head(l2));
                                    }
                                    else {
                                        matchResult_1 = 2;
                                    }
                                }
                                else {
                                    matchResult_1 = 2;
                                }
                                switch (matchResult_1) {
                                    case 0:
                                        if (x1_5 instanceof SetTreeNode$1) {
                                            const x1n_4 = x1_5;
                                            comparer_mut = comparer;
                                            l1_mut = ofArrayWithTail([SetTreeNode$1__get_Left(x1n_4), SetTreeNode$1_$ctor_5F465FC9(SetTreeLeaf$1__get_Key(x1n_4), SetTreeModule_empty(), SetTreeNode$1__get_Right(x1n_4), 0)], t1_7);
                                            l2_mut = l2;
                                            continue SetTreeModule_compareStacks;
                                        }
                                        else {
                                            comparer_mut = comparer;
                                            l1_mut = ofArrayWithTail([SetTreeModule_empty(), SetTreeLeaf$1_$ctor_2B595(SetTreeLeaf$1__get_Key(x1_5))], t1_7);
                                            l2_mut = l2;
                                            continue SetTreeModule_compareStacks;
                                        }
                                    case 1:
                                        if (x2_5 instanceof SetTreeNode$1) {
                                            const x2n_4 = x2_5;
                                            comparer_mut = comparer;
                                            l1_mut = l1;
                                            l2_mut = ofArrayWithTail([SetTreeNode$1__get_Left(x2n_4), SetTreeNode$1_$ctor_5F465FC9(SetTreeLeaf$1__get_Key(x2n_4), SetTreeModule_empty(), SetTreeNode$1__get_Right(x2n_4), 0)], t2_7);
                                            continue SetTreeModule_compareStacks;
                                        }
                                        else {
                                            comparer_mut = comparer;
                                            l1_mut = l1;
                                            l2_mut = ofArrayWithTail([SetTreeModule_empty(), SetTreeLeaf$1_$ctor_2B595(SetTreeLeaf$1__get_Key(x2_5))], t2_7);
                                            continue SetTreeModule_compareStacks;
                                        }
                                    default:
                                        throw new Error("unexpected state in SetTree.compareStacks");
                                }
                            }
                        }
                        else if (x2_3 instanceof SetTreeNode$1) {
                            const x2n_5 = x2_3;
                            if (SetTreeNode$1__get_Left(x2n_5) == null) {
                                const c_2 = comparer.Compare(SetTreeLeaf$1__get_Key(x1_3), SetTreeLeaf$1__get_Key(x2n_5)) | 0;
                                if (c_2 !== 0) {
                                    return c_2 | 0;
                                }
                                else {
                                    comparer_mut = comparer;
                                    l1_mut = cons(SetTreeModule_empty(), tail(l1));
                                    l2_mut = cons(SetTreeNode$1__get_Right(x2n_5), tail(l2));
                                    continue SetTreeModule_compareStacks;
                                }
                            }
                            else {
                                let matchResult_2, t1_8, x1_6, t2_8, x2_6;
                                if (!isEmpty_1(l1)) {
                                    if (head(l1) != null) {
                                        matchResult_2 = 0;
                                        t1_8 = tail(l1);
                                        x1_6 = value_3(head(l1));
                                    }
                                    else if (!isEmpty_1(l2)) {
                                        if (head(l2) != null) {
                                            matchResult_2 = 1;
                                            t2_8 = tail(l2);
                                            x2_6 = value_3(head(l2));
                                        }
                                        else {
                                            matchResult_2 = 2;
                                        }
                                    }
                                    else {
                                        matchResult_2 = 2;
                                    }
                                }
                                else if (!isEmpty_1(l2)) {
                                    if (head(l2) != null) {
                                        matchResult_2 = 1;
                                        t2_8 = tail(l2);
                                        x2_6 = value_3(head(l2));
                                    }
                                    else {
                                        matchResult_2 = 2;
                                    }
                                }
                                else {
                                    matchResult_2 = 2;
                                }
                                switch (matchResult_2) {
                                    case 0:
                                        if (x1_6 instanceof SetTreeNode$1) {
                                            const x1n_5 = x1_6;
                                            comparer_mut = comparer;
                                            l1_mut = ofArrayWithTail([SetTreeNode$1__get_Left(x1n_5), SetTreeNode$1_$ctor_5F465FC9(SetTreeLeaf$1__get_Key(x1n_5), SetTreeModule_empty(), SetTreeNode$1__get_Right(x1n_5), 0)], t1_8);
                                            l2_mut = l2;
                                            continue SetTreeModule_compareStacks;
                                        }
                                        else {
                                            comparer_mut = comparer;
                                            l1_mut = ofArrayWithTail([SetTreeModule_empty(), SetTreeLeaf$1_$ctor_2B595(SetTreeLeaf$1__get_Key(x1_6))], t1_8);
                                            l2_mut = l2;
                                            continue SetTreeModule_compareStacks;
                                        }
                                    case 1:
                                        if (x2_6 instanceof SetTreeNode$1) {
                                            const x2n_6 = x2_6;
                                            comparer_mut = comparer;
                                            l1_mut = l1;
                                            l2_mut = ofArrayWithTail([SetTreeNode$1__get_Left(x2n_6), SetTreeNode$1_$ctor_5F465FC9(SetTreeLeaf$1__get_Key(x2n_6), SetTreeModule_empty(), SetTreeNode$1__get_Right(x2n_6), 0)], t2_8);
                                            continue SetTreeModule_compareStacks;
                                        }
                                        else {
                                            comparer_mut = comparer;
                                            l1_mut = l1;
                                            l2_mut = ofArrayWithTail([SetTreeModule_empty(), SetTreeLeaf$1_$ctor_2B595(SetTreeLeaf$1__get_Key(x2_6))], t2_8);
                                            continue SetTreeModule_compareStacks;
                                        }
                                    default:
                                        throw new Error("unexpected state in SetTree.compareStacks");
                                }
                            }
                        }
                        else {
                            const c_3 = comparer.Compare(SetTreeLeaf$1__get_Key(x1_3), SetTreeLeaf$1__get_Key(x2_3)) | 0;
                            if (c_3 !== 0) {
                                return c_3 | 0;
                            }
                            else {
                                comparer_mut = comparer;
                                l1_mut = tail(l1);
                                l2_mut = tail(l2);
                                continue SetTreeModule_compareStacks;
                            }
                        }
                    }
                    else {
                        const x2 = value_3(head(l2));
                        let matchResult_3, t1_2, x1, t2_2, x2_1;
                        if (!isEmpty_1(l1)) {
                            if (head(l1) != null) {
                                matchResult_3 = 0;
                                t1_2 = tail(l1);
                                x1 = value_3(head(l1));
                            }
                            else if (!isEmpty_1(l2)) {
                                if (head(l2) != null) {
                                    matchResult_3 = 1;
                                    t2_2 = tail(l2);
                                    x2_1 = value_3(head(l2));
                                }
                                else {
                                    matchResult_3 = 2;
                                }
                            }
                            else {
                                matchResult_3 = 2;
                            }
                        }
                        else if (!isEmpty_1(l2)) {
                            if (head(l2) != null) {
                                matchResult_3 = 1;
                                t2_2 = tail(l2);
                                x2_1 = value_3(head(l2));
                            }
                            else {
                                matchResult_3 = 2;
                            }
                        }
                        else {
                            matchResult_3 = 2;
                        }
                        switch (matchResult_3) {
                            case 0:
                                if (x1 instanceof SetTreeNode$1) {
                                    const x1n = x1;
                                    comparer_mut = comparer;
                                    l1_mut = ofArrayWithTail([SetTreeNode$1__get_Left(x1n), SetTreeNode$1_$ctor_5F465FC9(SetTreeLeaf$1__get_Key(x1n), SetTreeModule_empty(), SetTreeNode$1__get_Right(x1n), 0)], t1_2);
                                    l2_mut = l2;
                                    continue SetTreeModule_compareStacks;
                                }
                                else {
                                    comparer_mut = comparer;
                                    l1_mut = ofArrayWithTail([SetTreeModule_empty(), SetTreeLeaf$1_$ctor_2B595(SetTreeLeaf$1__get_Key(x1))], t1_2);
                                    l2_mut = l2;
                                    continue SetTreeModule_compareStacks;
                                }
                            case 1:
                                if (x2_1 instanceof SetTreeNode$1) {
                                    const x2n = x2_1;
                                    comparer_mut = comparer;
                                    l1_mut = l1;
                                    l2_mut = ofArrayWithTail([SetTreeNode$1__get_Left(x2n), SetTreeNode$1_$ctor_5F465FC9(SetTreeLeaf$1__get_Key(x2n), SetTreeModule_empty(), SetTreeNode$1__get_Right(x2n), 0)], t2_2);
                                    continue SetTreeModule_compareStacks;
                                }
                                else {
                                    comparer_mut = comparer;
                                    l1_mut = l1;
                                    l2_mut = ofArrayWithTail([SetTreeModule_empty(), SetTreeLeaf$1_$ctor_2B595(SetTreeLeaf$1__get_Key(x2_1))], t2_2);
                                    continue SetTreeModule_compareStacks;
                                }
                            default:
                                throw new Error("unexpected state in SetTree.compareStacks");
                        }
                    }
                }
                else if (head(l1) != null) {
                    const x1_1 = value_3(head(l1));
                    let matchResult_4, t1_4, x1_2, t2_4, x2_2;
                    if (!isEmpty_1(l1)) {
                        if (head(l1) != null) {
                            matchResult_4 = 0;
                            t1_4 = tail(l1);
                            x1_2 = value_3(head(l1));
                        }
                        else if (!isEmpty_1(l2)) {
                            if (head(l2) != null) {
                                matchResult_4 = 1;
                                t2_4 = tail(l2);
                                x2_2 = value_3(head(l2));
                            }
                            else {
                                matchResult_4 = 2;
                            }
                        }
                        else {
                            matchResult_4 = 2;
                        }
                    }
                    else if (!isEmpty_1(l2)) {
                        if (head(l2) != null) {
                            matchResult_4 = 1;
                            t2_4 = tail(l2);
                            x2_2 = value_3(head(l2));
                        }
                        else {
                            matchResult_4 = 2;
                        }
                    }
                    else {
                        matchResult_4 = 2;
                    }
                    switch (matchResult_4) {
                        case 0:
                            if (x1_2 instanceof SetTreeNode$1) {
                                const x1n_1 = x1_2;
                                comparer_mut = comparer;
                                l1_mut = ofArrayWithTail([SetTreeNode$1__get_Left(x1n_1), SetTreeNode$1_$ctor_5F465FC9(SetTreeLeaf$1__get_Key(x1n_1), SetTreeModule_empty(), SetTreeNode$1__get_Right(x1n_1), 0)], t1_4);
                                l2_mut = l2;
                                continue SetTreeModule_compareStacks;
                            }
                            else {
                                comparer_mut = comparer;
                                l1_mut = ofArrayWithTail([SetTreeModule_empty(), SetTreeLeaf$1_$ctor_2B595(SetTreeLeaf$1__get_Key(x1_2))], t1_4);
                                l2_mut = l2;
                                continue SetTreeModule_compareStacks;
                            }
                        case 1:
                            if (x2_2 instanceof SetTreeNode$1) {
                                const x2n_1 = x2_2;
                                comparer_mut = comparer;
                                l1_mut = l1;
                                l2_mut = ofArrayWithTail([SetTreeNode$1__get_Left(x2n_1), SetTreeNode$1_$ctor_5F465FC9(SetTreeLeaf$1__get_Key(x2n_1), SetTreeModule_empty(), SetTreeNode$1__get_Right(x2n_1), 0)], t2_4);
                                continue SetTreeModule_compareStacks;
                            }
                            else {
                                comparer_mut = comparer;
                                l1_mut = l1;
                                l2_mut = ofArrayWithTail([SetTreeModule_empty(), SetTreeLeaf$1_$ctor_2B595(SetTreeLeaf$1__get_Key(x2_2))], t2_4);
                                continue SetTreeModule_compareStacks;
                            }
                        default:
                            throw new Error("unexpected state in SetTree.compareStacks");
                    }
                }
                else {
                    comparer_mut = comparer;
                    l1_mut = tail(l1);
                    l2_mut = tail(l2);
                    continue SetTreeModule_compareStacks;
                }
            }
            else {
                return 1;
            }
        }
        else if (isEmpty_1(l2)) {
            return 0;
        }
        else {
            return -1;
        }
        break;
    }
}
export function SetTreeModule_compare(comparer, t1, t2) {
    if (t1 == null) {
        if (t2 == null) {
            return 0;
        }
        else {
            return -1;
        }
    }
    else if (t2 == null) {
        return 1;
    }
    else {
        return SetTreeModule_compareStacks(comparer, singleton_1(t1), singleton_1(t2)) | 0;
    }
}
export function SetTreeModule_choose(s) {
    return SetTreeModule_minimumElement(s);
}
export function SetTreeModule_toList(t) {
    const loop = (t$0027_mut, acc_mut) => {
        loop: while (true) {
            const t$0027 = t$0027_mut, acc = acc_mut;
            if (t$0027 != null) {
                const t2 = value_3(t$0027);
                if (t2 instanceof SetTreeNode$1) {
                    const tn = t2;
                    t$0027_mut = SetTreeNode$1__get_Left(tn);
                    acc_mut = cons(SetTreeLeaf$1__get_Key(tn), loop(SetTreeNode$1__get_Right(tn), acc));
                    continue loop;
                }
                else {
                    return cons(SetTreeLeaf$1__get_Key(t2), acc);
                }
            }
            else {
                return acc;
            }
            break;
        }
    };
    return loop(t, empty_1());
}
export function SetTreeModule_copyToArray(s, arr, i) {
    let j = i;
    SetTreeModule_iter((x) => {
        arr[j] = x;
        j = ((j + 1) | 0);
    }, s);
}
export function SetTreeModule_toArray(s) {
    const n = SetTreeModule_count(s) | 0;
    const res = fill(new Array(n), 0, n, null);
    SetTreeModule_copyToArray(s, res, 0);
    return res;
}
export function SetTreeModule_mkFromEnumerator(comparer_mut, acc_mut, e_mut) {
    SetTreeModule_mkFromEnumerator: while (true) {
        const comparer = comparer_mut, acc = acc_mut, e = e_mut;
        if (e["System.Collections.IEnumerator.MoveNext"]()) {
            comparer_mut = comparer;
            acc_mut = SetTreeModule_add(comparer, e["System.Collections.Generic.IEnumerator`1.get_Current"](), acc);
            e_mut = e;
            continue SetTreeModule_mkFromEnumerator;
        }
        else {
            return acc;
        }
        break;
    }
}
export function SetTreeModule_ofArray(comparer, l) {
    return fold_1((acc, k) => SetTreeModule_add(comparer, k, acc), SetTreeModule_empty(), l);
}
export function SetTreeModule_ofList(comparer, l) {
    return fold_2((acc, k) => SetTreeModule_add(comparer, k, acc), SetTreeModule_empty(), l);
}
export function SetTreeModule_ofSeq(comparer, c) {
    if (isArrayLike(c)) {
        return SetTreeModule_ofArray(comparer, c);
    }
    else if (c instanceof FSharpList) {
        return SetTreeModule_ofList(comparer, c);
    }
    else {
        const ie = getEnumerator(c);
        try {
            return SetTreeModule_mkFromEnumerator(comparer, SetTreeModule_empty(), ie);
        }
        finally {
            disposeSafe(ie);
        }
    }
}
export class FSharpSet {
    constructor(comparer, tree) {
        this.comparer = comparer;
        this.tree = tree;
    }
    GetHashCode() {
        const this$ = this;
        return FSharpSet__ComputeHashCode(this$) | 0;
    }
    Equals(that) {
        let that_1;
        const this$ = this;
        return (that instanceof FSharpSet) && ((that_1 = that, SetTreeModule_compare(FSharpSet__get_Comparer(this$), FSharpSet__get_Tree(this$), FSharpSet__get_Tree(that_1)) === 0));
    }
    toString() {
        const this$ = this;
        return ("set [" + join("; ", map_1((x) => {
            let copyOfStruct = x;
            return toString(copyOfStruct);
        }, this$))) + "]";
    }
    get [Symbol.toStringTag]() {
        return "FSharpSet";
    }
    toJSON() {
        const this$ = this;
        return Array.from(this$);
    }
    CompareTo(that) {
        const s = this;
        return SetTreeModule_compare(FSharpSet__get_Comparer(s), FSharpSet__get_Tree(s), FSharpSet__get_Tree(that)) | 0;
    }
    "System.Collections.Generic.ICollection`1.Add2B595"(x) {
        throw new Error("ReadOnlyCollection");
    }
    "System.Collections.Generic.ICollection`1.Clear"() {
        throw new Error("ReadOnlyCollection");
    }
    "System.Collections.Generic.ICollection`1.Remove2B595"(x) {
        throw new Error("ReadOnlyCollection");
    }
    "System.Collections.Generic.ICollection`1.Contains2B595"(x) {
        const s = this;
        return SetTreeModule_mem(FSharpSet__get_Comparer(s), x, FSharpSet__get_Tree(s));
    }
    "System.Collections.Generic.ICollection`1.CopyToZ3B4C077E"(arr, i) {
        const s = this;
        SetTreeModule_copyToArray(FSharpSet__get_Tree(s), arr, i);
    }
    "System.Collections.Generic.ICollection`1.get_IsReadOnly"() {
        return true;
    }
    "System.Collections.Generic.ICollection`1.get_Count"() {
        const s = this;
        return FSharpSet__get_Count(s) | 0;
    }
    "System.Collections.Generic.IReadOnlyCollection`1.get_Count"() {
        const s = this;
        return FSharpSet__get_Count(s) | 0;
    }
    GetEnumerator() {
        const s = this;
        return SetTreeModule_mkIEnumerator(FSharpSet__get_Tree(s));
    }
    [Symbol.iterator]() {
        return toIterator(getEnumerator(this));
    }
    "System.Collections.IEnumerable.GetEnumerator"() {
        const s = this;
        return SetTreeModule_mkIEnumerator(FSharpSet__get_Tree(s));
    }
    get size() {
        const s = this;
        return FSharpSet__get_Count(s) | 0;
    }
    add(k) {
        const s = this;
        throw new Error("Set cannot be mutated");
        return s;
    }
    clear() {
        throw new Error("Set cannot be mutated");
    }
    delete(k) {
        throw new Error("Set cannot be mutated");
        return false;
    }
    has(k) {
        const s = this;
        return FSharpSet__Contains(s, k);
    }
    keys() {
        const s = this;
        return map_1((x) => x, s);
    }
    values() {
        const s = this;
        return map_1((x) => x, s);
    }
    entries() {
        const s = this;
        return map_1((v) => [v, v], s);
    }
    forEach(f, thisArg) {
        const s = this;
        iterate_1((x) => {
            f(x, x, s);
        }, s);
    }
}
export function FSharpSet_$reflection(gen0) {
    return class_type("Set.FSharpSet", [gen0], FSharpSet);
}
export function FSharpSet_$ctor(comparer, tree) {
    return new FSharpSet(comparer, tree);
}
export function FSharpSet__get_Comparer(set$) {
    return set$.comparer;
}
export function FSharpSet__get_Tree(set$) {
    return set$.tree;
}
export function FSharpSet_Empty(comparer) {
    return FSharpSet_$ctor(comparer, SetTreeModule_empty());
}
export function FSharpSet__Add(s, value) {
    return FSharpSet_$ctor(FSharpSet__get_Comparer(s), SetTreeModule_add(FSharpSet__get_Comparer(s), value, FSharpSet__get_Tree(s)));
}
export function FSharpSet__Remove(s, value) {
    return FSharpSet_$ctor(FSharpSet__get_Comparer(s), SetTreeModule_remove(FSharpSet__get_Comparer(s), value, FSharpSet__get_Tree(s)));
}
export function FSharpSet__get_Count(s) {
    return SetTreeModule_count(FSharpSet__get_Tree(s));
}
export function FSharpSet__Contains(s, value) {
    return SetTreeModule_mem(FSharpSet__get_Comparer(s), value, FSharpSet__get_Tree(s));
}
export function FSharpSet__Iterate(s, x) {
    SetTreeModule_iter(x, FSharpSet__get_Tree(s));
}
export function FSharpSet__Fold(s, f, z) {
    const f_1 = f;
    return SetTreeModule_fold((x, z_1) => f_1(z_1, x), z, FSharpSet__get_Tree(s));
}
export function FSharpSet__get_IsEmpty(s) {
    return FSharpSet__get_Tree(s) == null;
}
export function FSharpSet__Partition(s, f) {
    if (FSharpSet__get_Tree(s) == null) {
        return [s, s];
    }
    else {
        const patternInput = SetTreeModule_partition(FSharpSet__get_Comparer(s), f, FSharpSet__get_Tree(s));
        return [FSharpSet_$ctor(FSharpSet__get_Comparer(s), patternInput[0]), FSharpSet_$ctor(FSharpSet__get_Comparer(s), patternInput[1])];
    }
}
export function FSharpSet__Filter(s, f) {
    if (FSharpSet__get_Tree(s) == null) {
        return s;
    }
    else {
        return FSharpSet_$ctor(FSharpSet__get_Comparer(s), SetTreeModule_filter(FSharpSet__get_Comparer(s), f, FSharpSet__get_Tree(s)));
    }
}
export function FSharpSet__Map(s, f, comparer) {
    return FSharpSet_$ctor(comparer, SetTreeModule_fold((acc, k) => SetTreeModule_add(comparer, f(k), acc), SetTreeModule_empty(), FSharpSet__get_Tree(s)));
}
export function FSharpSet__Exists(s, f) {
    return SetTreeModule_exists(f, FSharpSet__get_Tree(s));
}
export function FSharpSet__ForAll(s, f) {
    return SetTreeModule_forall(f, FSharpSet__get_Tree(s));
}
export function FSharpSet_op_Subtraction(set1, set2) {
    if (FSharpSet__get_Tree(set1) == null) {
        return set1;
    }
    else if (FSharpSet__get_Tree(set2) == null) {
        return set1;
    }
    else {
        return FSharpSet_$ctor(FSharpSet__get_Comparer(set1), SetTreeModule_diff(FSharpSet__get_Comparer(set1), FSharpSet__get_Tree(set1), FSharpSet__get_Tree(set2)));
    }
}
export function FSharpSet_op_Addition(set1, set2) {
    if (FSharpSet__get_Tree(set2) == null) {
        return set1;
    }
    else if (FSharpSet__get_Tree(set1) == null) {
        return set2;
    }
    else {
        return FSharpSet_$ctor(FSharpSet__get_Comparer(set1), SetTreeModule_union(FSharpSet__get_Comparer(set1), FSharpSet__get_Tree(set1), FSharpSet__get_Tree(set2)));
    }
}
export function FSharpSet_Intersection(a, b) {
    if (FSharpSet__get_Tree(b) == null) {
        return b;
    }
    else if (FSharpSet__get_Tree(a) == null) {
        return a;
    }
    else {
        return FSharpSet_$ctor(FSharpSet__get_Comparer(a), SetTreeModule_intersection(FSharpSet__get_Comparer(a), FSharpSet__get_Tree(a), FSharpSet__get_Tree(b)));
    }
}
export function FSharpSet_IntersectionMany(sets) {
    return reduce(FSharpSet_Intersection, sets);
}
export function FSharpSet_Equality(a, b) {
    return SetTreeModule_compare(FSharpSet__get_Comparer(a), FSharpSet__get_Tree(a), FSharpSet__get_Tree(b)) === 0;
}
export function FSharpSet_Compare(a, b) {
    return SetTreeModule_compare(FSharpSet__get_Comparer(a), FSharpSet__get_Tree(a), FSharpSet__get_Tree(b));
}
export function FSharpSet__get_Choose(x) {
    return SetTreeModule_choose(FSharpSet__get_Tree(x));
}
export function FSharpSet__get_MinimumElement(x) {
    return SetTreeModule_minimumElement(FSharpSet__get_Tree(x));
}
export function FSharpSet__get_MaximumElement(x) {
    return SetTreeModule_maximumElement(FSharpSet__get_Tree(x));
}
export function FSharpSet__IsSubsetOf(x, otherSet) {
    return SetTreeModule_subset(FSharpSet__get_Comparer(x), FSharpSet__get_Tree(x), FSharpSet__get_Tree(otherSet));
}
export function FSharpSet__IsSupersetOf(x, otherSet) {
    return SetTreeModule_subset(FSharpSet__get_Comparer(x), FSharpSet__get_Tree(otherSet), FSharpSet__get_Tree(x));
}
export function FSharpSet__IsProperSubsetOf(x, otherSet) {
    return SetTreeModule_properSubset(FSharpSet__get_Comparer(x), FSharpSet__get_Tree(x), FSharpSet__get_Tree(otherSet));
}
export function FSharpSet__IsProperSupersetOf(x, otherSet) {
    return SetTreeModule_properSubset(FSharpSet__get_Comparer(x), FSharpSet__get_Tree(otherSet), FSharpSet__get_Tree(x));
}
export function FSharpSet__ToList(x) {
    return SetTreeModule_toList(FSharpSet__get_Tree(x));
}
export function FSharpSet__ToArray(x) {
    return SetTreeModule_toArray(FSharpSet__get_Tree(x));
}
export function FSharpSet__ComputeHashCode(this$) {
    let res = 0;
    const enumerator = getEnumerator(this$);
    try {
        while (enumerator["System.Collections.IEnumerator.MoveNext"]()) {
            const x_1 = enumerator["System.Collections.Generic.IEnumerator`1.get_Current"]();
            res = ((((res << 1) + structuralHash(x_1)) + 631) | 0);
        }
    }
    finally {
        disposeSafe(enumerator);
    }
    return Math.abs(res) | 0;
}
export function isEmpty(set$) {
    return FSharpSet__get_IsEmpty(set$);
}
export function contains(element, set$) {
    return FSharpSet__Contains(set$, element);
}
export function add(value, set$) {
    return FSharpSet__Add(set$, value);
}
export function singleton(value, comparer) {
    return FSharpSet__Add(FSharpSet_Empty(comparer), value);
}
export function remove(value, set$) {
    return FSharpSet__Remove(set$, value);
}
export function union(set1, set2) {
    return FSharpSet_op_Addition(set1, set2);
}
export function unionMany(sets, comparer) {
    return fold_3(FSharpSet_op_Addition, FSharpSet_Empty(comparer), sets);
}
export function intersect(set1, set2) {
    return FSharpSet_Intersection(set1, set2);
}
export function intersectMany(sets) {
    return FSharpSet_IntersectionMany(sets);
}
export function iterate(action, set$) {
    FSharpSet__Iterate(set$, action);
}
export function empty(comparer) {
    return FSharpSet_Empty(comparer);
}
export function forAll(predicate, set$) {
    return FSharpSet__ForAll(set$, predicate);
}
export function exists(predicate, set$) {
    return FSharpSet__Exists(set$, predicate);
}
export function filter(predicate, set$) {
    return FSharpSet__Filter(set$, predicate);
}
export function partition(predicate, set$) {
    return FSharpSet__Partition(set$, predicate);
}
export function fold(folder, state, set$) {
    return SetTreeModule_fold(folder, state, FSharpSet__get_Tree(set$));
}
export function foldBack(folder, set$, state) {
    return SetTreeModule_foldBack(folder, FSharpSet__get_Tree(set$), state);
}
export function map(mapping, set$, comparer) {
    return FSharpSet__Map(set$, mapping, comparer);
}
export function count(set$) {
    return FSharpSet__get_Count(set$);
}
export function ofList(elements, comparer) {
    return FSharpSet_$ctor(comparer, SetTreeModule_ofSeq(comparer, elements));
}
export function ofArray(array, comparer) {
    return FSharpSet_$ctor(comparer, SetTreeModule_ofArray(comparer, array));
}
export function toList(set$) {
    return FSharpSet__ToList(set$);
}
export function toArray(set$) {
    return FSharpSet__ToArray(set$);
}
export function toSeq(set$) {
    return map_1((x) => x, set$);
}
export function ofSeq(elements, comparer) {
    return FSharpSet_$ctor(comparer, SetTreeModule_ofSeq(comparer, elements));
}
export function difference(set1, set2) {
    return FSharpSet_op_Subtraction(set1, set2);
}
export function isSubset(set1, set2) {
    return SetTreeModule_subset(FSharpSet__get_Comparer(set1), FSharpSet__get_Tree(set1), FSharpSet__get_Tree(set2));
}
export function isSuperset(set1, set2) {
    return SetTreeModule_subset(FSharpSet__get_Comparer(set1), FSharpSet__get_Tree(set2), FSharpSet__get_Tree(set1));
}
export function isProperSubset(set1, set2) {
    return SetTreeModule_properSubset(FSharpSet__get_Comparer(set1), FSharpSet__get_Tree(set1), FSharpSet__get_Tree(set2));
}
export function isProperSuperset(set1, set2) {
    return SetTreeModule_properSubset(FSharpSet__get_Comparer(set1), FSharpSet__get_Tree(set2), FSharpSet__get_Tree(set1));
}
export function minElement(set$) {
    return FSharpSet__get_MinimumElement(set$);
}
export function maxElement(set$) {
    return FSharpSet__get_MaximumElement(set$);
}
export function unionWith(s1, s2) {
    return fold_3((acc, x) => acc.add(x), s1, s2);
}
export function newMutableSetWith(s1, s2) {
    if (s1 instanceof HashSet) {
        const s1_1 = s1;
        return HashSet_$ctor_Z6150332D(s2, HashSet__get_Comparer(s1_1));
    }
    else {
        return new Set(s2);
    }
}
export function intersectWith(s1, s2) {
    const s2_1 = newMutableSetWith(s1, s2);
    iterate_1((x) => {
        if (!s2_1.has(x)) {
            s1.delete(x);
        }
    }, s1.values());
}
export function exceptWith(s1, s2) {
    iterate_1((x) => {
        s1.delete(x);
    }, s2);
}
export function isSubsetOf(s1, s2) {
    const s2_1 = newMutableSetWith(s1, s2);
    return forAll_1((value) => s2_1.has(value), s1.values());
}
export function isSupersetOf(s1, s2) {
    return forAll_1((value) => s1.has(value), s2);
}
export function isProperSubsetOf(s1, s2) {
    const s2_1 = newMutableSetWith(s1, s2);
    if (s2_1.size > s1.size) {
        return forAll_1((value) => s2_1.has(value), s1.values());
    }
    else {
        return false;
    }
}
export function isProperSupersetOf(s1, s2) {
    const s2_1 = cache(s2);
    if (exists_1((arg) => !s1.has(arg), s2_1)) {
        return forAll_1((value_2) => s1.has(value_2), s2_1);
    }
    else {
        return false;
    }
}
